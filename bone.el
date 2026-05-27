;;; bone.el --- Browse and manage BARK reports in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail, news
;; URL: https://codeberg.org/bzg/bone.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Browser and shared data layer for BARK/bone in Emacs.  `M-x bone'
;; opens a report browser; the same data layer backs the mail front-ends
;; gnus-bone, notmuch-bone and mu4e-bone.  This library:
;;
;; - reads the bone configuration (`config.edn') and report sources,
;; - manages the local cache of remote `reports.json' files,
;; - parses and serializes the EDN config and `state.edn' files shared
;;   with the bone CLI,
;; - exposes the report list and the local-mark API the front-ends use.
;;
;; Front-ends provide the message metadata (an INFO plist with keys
;; :type :subject :date :from :from-name, plus display keys :flags
;; :priority :votes :deadline :expiry :last-activity :topic) and the
;; presentation; this library provides everything that is independent of
;; the mail user agent.
;;
;; Entry points:
;;
;;   `bone'              browse reports (interactive)
;;   `bone-reports'      collect open reports from all sources
;;   `bone-update'       force-refresh the local cache (interactive)
;;   `bone-toggle-mark'  toggle :read/:todo/:sticky for a message-id
;;   `bone-read-state' / `bone-write-state'   state.edn I/O
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)

(defvar url-http-response-status)

(defgroup bone nil
  "Read and manage BARK reports shared with the bone CLI."
  :group 'mail)

(defcustom bone-config-dir "~/.config/bone"
  "Directory containing bone configuration and state/cache files."
  :type 'directory
  :group 'bone)

(defcustom bone-after-update-hook nil
  "Functions run after `bone-update' refreshes the local cache.
Front-ends can use this to re-apply their display."
  :type 'hook
  :group 'bone)

(defvar bone-addresses nil
  "List of user email addresses loaded from config.")

(defconst bone-supported-bark-format "0.9.2"
  "Minimum reports.json bark-format bone.el reads without warning.")

;;; EDN reader/writer (the subset emitted by bone's config.edn and state.edn)

(defun bone-edn--skip-ws ()
  "Skip EDN whitespace, commas and line comments at point."
  (skip-chars-forward " \t\n\r,")
  (while (eq (char-after) ?\;)
    (forward-line 1)
    (skip-chars-forward " \t\n\r,")))

(defun bone-edn--read ()
  "Read one EDN value at point."
  (bone-edn--skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "EDN: unexpected EOF"))
     ((eq c ?\") (read (current-buffer)))
     ((eq c ?:)  (bone-edn--read-keyword))
     ((eq c ?\{) (bone-edn--read-map))
     ((eq c ?\[) (bone-edn--read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                           (and d (>= d ?0) (<= d ?9)))))
      (bone-edn--read-number))
     (t (bone-edn--read-symbol)))))

(defun bone-edn--read-keyword ()
  "Read an EDN keyword at point."
  (forward-char 1)
  (let ((start (1- (point))))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (intern (buffer-substring-no-properties start (point)))))

(defun bone-edn--read-symbol ()
  "Read an EDN symbol at point."
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (pcase (buffer-substring-no-properties start (point))
      ("nil"   nil)
      ("true"  t)
      ("false" nil)
      (s       (intern s)))))

(defun bone-edn--read-number ()
  "Read an EDN number at point."
  (let ((start (point)))
    (skip-chars-forward "0-9.eE+-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun bone-edn--read-map ()
  "Read an EDN map at point."
  (forward-char 1)
  (let ((acc nil))
    (bone-edn--skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (bone-edn--read)))
        (bone-edn--skip-ws)
        (push (cons k (bone-edn--read)) acc))
      (bone-edn--skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun bone-edn--read-vector ()
  "Read an EDN vector at point."
  (forward-char 1)
  (let ((acc nil))
    (bone-edn--skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (bone-edn--read) acc)
      (bone-edn--skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun bone-edn--map-p (x)
  "Non-nil if list X is an alist to write as an EDN map (not a vector).
True when every element is a cons whose car is an atom (a key)."
  (and (consp x) (cl-every (lambda (e) (and (consp e) (atom (car e)))) x)))

(defun bone--edn-write (v)
  "Serialize EDN value V (maps, vectors and scalars) to a string."
  (cond
   ((stringp v)  (format "%S" v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((symbolp v)  (symbol-name v))
   ((bone-edn--map-p v)
    (concat "{" (mapconcat (lambda (kv)
                             (concat (bone--edn-write (car kv)) " "
                                     (bone--edn-write (cdr kv))))
                           v " ")
            "}"))
   ((listp v) (concat "[" (mapconcat #'bone--edn-write v " ") "]"))
   (t (error "EDN: cannot serialize %S" v))))

(defun bone-edn-read-buffer ()
  "Read one EDN map from the current buffer if it starts with `{'.
Return nil on parse failure or when no map is present."
  (goto-char (point-min))
  (bone-edn--skip-ws)
  (when (eq (char-after) ?\{)
    (bone-edn--read-map)))

;;; Configuration and report sources

(defun bone--uri-to-path (uri)
  "Convert file:// URI to local path, otherwise return URI."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun bone-load-config ()
  "Load `config.edn' and return a plist.
Keys: :addresses :skip-columns :source-configs (raw `:sources' maps)
and :sources (their URLs)."
  (let* ((file (expand-file-name "config.edn" bone-config-dir))
         (cfg (when (file-readable-p file)
                (condition-case err
                    (with-temp-buffer
                      (insert-file-contents file)
                      (bone-edn-read-buffer))
                  (error
                   (message "bone: cannot parse %s: %s"
                            file (error-message-string err))
                   nil))))
         (addresses (or (alist-get :my-addresses cfg)
                        (alist-get :addresses cfg)))
         (skip      (alist-get :skip-columns cfg))
         (src-cfgs  (alist-get :sources cfg))
         (sources   (mapcan (lambda (s) (append (alist-get :urls s) nil)) src-cfgs)))
    (setq bone-addresses addresses)
    (list :addresses addresses :skip-columns skip
          :source-configs src-cfgs :sources sources)))

(defun bone-sources ()
  "Return report sources from config.edn as URLs or absolute local paths.
Relative paths are resolved against `bone-config-dir'."
  (mapcar #'bone--resolve-source
          (plist-get (bone-load-config) :sources)))

(defun bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun bone--resolve-source (source)
  "Resolve SOURCE to an HTTP(S) URL or an absolute local path.
HTTP(S) URLs are returned unchanged; other sources are file paths,
relative ones resolved against `bone-config-dir'."
  (if (bone--http-url-p source)
      source
    (expand-file-name (bone--uri-to-path source)
                      (expand-file-name bone-config-dir))))

(defun bone--source-repo (info)
  "Return the local git repo for report INFO's source, or nil.
Reads `:repo' from the matching config.edn `:sources' entry, matched by
URL or by `:name'."
  (let* ((url  (plist-get info :source))
         (name (plist-get info :source-name))
         (entry (seq-find
                 (lambda (s)
                   (or (and url (member url (mapcar #'bone--resolve-source
                                                    (alist-get :urls s))))
                       (and name (equal name (alist-get :name s)))))
                 (plist-get (bone-load-config) :source-configs))))
    (when-let* ((repo (alist-get :repo entry)))
      (expand-file-name repo))))

(defun bone--java-hash (str)
  "Calculate Java String hashCode of STR as an unsigned 32-bit integer."
  (let ((h 0)
        (len (length str)))
    (dotimes (i len)
      (setq h (logand (+ (* h 31) (aref str i)) #xffffffff)))
    h))

(defun bone--source-to-cache-file (src)
  "Return cache file path for remote source SRC."
  (let* ((h (format "%08x" (bone--java-hash src)))
         (safe (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" src))
         (prefix (substring safe 0 (min 80 (length safe)))))
    (expand-file-name
     (concat "cache/reports/" prefix "-" h ".json")
     bone-config-dir)))

(defun bone--fetch-json-from-url (url)
  "Synchronously fetch JSON from URL."
  (let ((buf (url-retrieve-synchronously url t)))
    (unless buf (error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (when (and (bound-and-true-p url-http-response-status)
                     (>= url-http-response-status 400))
            (error "HTTP error %d from %s" url-http-response-status url))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Malformed HTTP response from %s" url))
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-false nil)
                (body (buffer-substring-no-properties (point) (point-max))))
            (json-read-from-string (decode-coding-string body 'utf-8))))
      (kill-buffer buf))))

(defun bone--write-json-to-file (data file)
  "Write JSON DATA to FILE as UTF-8."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert (json-encode data)))))

(defun bone--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false nil))
    (if (bone--http-url-p source)
        (let ((cache-file (bone--source-to-cache-file source)))
          (if (file-exists-p cache-file)
              (json-read-file cache-file)
            (let ((data (bone--fetch-json-from-url source)))
              (bone--write-json-to-file data cache-file)
              data)))
      (json-read-file source))))

(defun bone-normalize-mid (mid)
  "Ensure MID has angle brackets."
  (if (string-match-p "\\`<.*>\\'" mid)
      mid
    (concat "<" mid ">")))

(defun bone--extract-open-reports (source)
  "Extract open reports from SOURCE as (MID . INFO) pairs."
  (let* ((data (bone--read-json source))
         (fv (alist-get 'bark-format data))
         (sname (alist-get 'source data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv bone-supported-bark-format))
      (message "bone: %s has format %s, min supported is %s"
               source fv bone-supported-bark-format))
    (dolist (r reports result)
      (let ((mid          (alist-get 'message-id r))
            (status       (alist-get 'status r))
            (type         (alist-get 'type r))
            (acked        (alist-get 'acked r))
            (owned        (alist-get 'owned r))
            (closed       (alist-get 'closed r))
            (close-reason (alist-get 'close-reason r))
            (priority     (alist-get 'priority r))
            (votes        (alist-get 'votes r))
            (deadline     (alist-get 'deadline r))
            (expiry       (alist-get 'expiry r))
            (last-activity (alist-get 'last-activity r))
            (topic        (alist-get 'topic r))
            (subject      (alist-get 'subject r))
            (from         (alist-get 'from r))
            (from-name    (alist-get 'from-name r))
            (date         (alist-get 'date r))
            (archived-at  (alist-get 'archived-at r))
            (patches      (alist-get 'patches r))
            (series       (alist-get 'series r))
            (patch-seq    (alist-get 'patch-seq r))
            (version      (alist-get 'version r))
            (superseded-by (alist-get 'superseded-by r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-")))))
                (norm-mid (bone-normalize-mid mid)))
            (push (cons norm-mid (list :type (or type "bug")
                                       :flags flags
                                       :priority (or priority 0)
                                       :votes votes
                                       :deadline deadline
                                       :expiry expiry
                                       :last-activity last-activity
                                       :topic topic
                                       :subject subject
                                       :from from
                                       :from-name from-name
                                       :date date
                                       :source source
                                       :source-name sname
                                       :archived-at archived-at
                                       :patches patches
                                       :series series
                                       :patch-seq patch-seq
                                       :version version
                                       :superseded-by superseded-by))
                  result)))))))

(defun bone-reports ()
  "Collect open report pairs from all sources, tolerating failures."
  (let ((result nil))
    (dolist (source (bone-sources))
      (condition-case err
          (setq result (append result (bone--extract-open-reports source)))
        (error
         (message "bone: failed loading source %s: %s"
                  source (error-message-string err)))))
    result))

(defun bone-update ()
  "Force-refresh the local cache from remote JSON sources.
Run `bone-after-update-hook' when finished."
  (interactive)
  (let ((sources (bone-sources))
        (count 0))
    (dolist (source sources)
      (when (bone--http-url-p source)
        (message "bone: updating cache for %s..." source)
        (condition-case err
            (let ((data (bone--fetch-json-from-url source))
                  (cache-file (bone--source-to-cache-file source)))
              (bone--write-json-to-file data cache-file)
              (setq count (1+ count))
              (message "bone: cache updated for %s" source))
          (error
           (message "bone: failed updating %s: %s"
                    source (error-message-string err))))))
    (run-hooks 'bone-after-update-hook)
    (message "bone: cache update finished (%d updated)." count)))

;;; State file (state.edn)

(defun bone-read-state ()
  "Read and return the bone state alist, or nil."
  (let ((file (expand-file-name "state.edn" bone-config-dir)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (bone-edn-read-buffer))
        (error
         (message "bone: cannot parse %s: %s"
                  file (error-message-string err))
         nil)))))

(defun bone-write-state (state)
  "Write STATE to the state file as UTF-8, one entry per line."
  (let ((file (expand-file-name "state.edn" bone-config-dir))
        (coding-system-for-write 'utf-8))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{"
                (mapconcat (lambda (kv)
                             (concat (bone--edn-write (car kv)) " "
                                     (bone--edn-write (cdr kv))))
                           state "\n ")
                "}\n")))))

;;; Local marks (read / todo / sticky)

(defun bone--iso-now ()
  "Return the current time as an ISO-8601 UTC string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun bone--author-string (info)
  "Build author string from INFO."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string= n ""))) (concat n " <" e ">"))
     (e e)
     (n n))))

(defun bone--enrich-entry (existing info)
  "Refresh metadata from INFO in EXISTING state entry."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (let ((v (plist-get info (car pair))))
        (when v
          (setf (alist-get (cdr pair) entry) v))))
    (let ((author (bone--author-string info)))
      (when author
        (setf (alist-get :author entry) author)))
    entry))

(defun bone--state-put (state mid entry)
  "Set MID to ENTRY in STATE, keeping order."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun bone--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun bone--alist-dissoc (alist key)
  "Remove KEY from ALIST copy."
  (assq-delete-all key (copy-alist alist)))

(defun bone--alist-assoc (alist key value)
  "Set KEY to VALUE in ALIST copy."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defun bone--apply-transition (state action mid info)
  "Apply ACTION transition for MID in STATE using metadata INFO."
  (let* ((base (bone--enrich-entry (cdr (assoc mid state)) info))
         (flag (alist-get :flag base))
         (new
          (pcase action
            (:read   (if (alist-get :read-at base)
                         (bone--alist-dissoc base :read-at)
                       (bone--alist-assoc  base :read-at
                                           (bone--iso-now))))
            (:todo   (if (eq flag :todo)
                         (bone--alist-dissoc base :flag)
                       (bone--alist-assoc  base :flag :todo)))
            (:sticky (if (eq flag :sticky)
                         (bone--alist-dissoc base :flag)
                       (bone--alist-assoc  base :flag :sticky))))))
    (if (and (null (alist-get :flag    new))
             (null (alist-get :read-at new)))
        (bone--state-delete state mid)
      (bone--state-put state mid new))))

(defun bone-action-on-p (state mid action)
  "Return non-nil if ACTION is set for MID in STATE."
  (let ((entry (cdr (assoc mid state))))
    (pcase action
      (:read   (cdr (assq :read-at entry)))
      (:todo   (eq (cdr (assq :flag entry)) :todo))
      (:sticky (eq (cdr (assq :flag entry)) :sticky)))))

(defun bone-toggle-mark (mid info action)
  "Toggle ACTION (:read, :todo or :sticky) for MID using metadata INFO.
Persist the new state and return non-nil if ACTION is now on."
  (let* ((state (bone-read-state))
         (new   (bone--apply-transition state action mid info)))
    (bone-write-state new)
    (bone-action-on-p new mid action)))

;;; Source metadata (meta.json) and patch files

(defun bone--http-body (url &optional binary)
  "Return the HTTP body of URL.  Read raw bytes when BINARY is non-nil."
  (let* ((coding-system-for-read (if binary 'binary coding-system-for-read))
         (buf (url-retrieve-synchronously url t)))
    (unless buf (error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (when (and (bound-and-true-p url-http-response-status)
                     (>= url-http-response-status 400))
            (error "HTTP error %d from %s" url-http-response-status url))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Malformed HTTP response from %s" url))
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buf))))

(defun bone--fetch-url-to-file (url file)
  "Fetch URL synchronously and write its raw body bytes to FILE."
  (let ((body (bone--http-body url t)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert body)))))

(defun bone-source-meta (source)
  "Return the parsed meta.json sibling of reports SOURCE, or nil."
  (condition-case nil
      (bone--read-json (concat (file-name-directory source) "meta.json"))
    (error nil)))

(defun bone--patches-base (source)
  "Return the patches base URL or directory for reports SOURCE."
  (concat (file-name-directory (directory-file-name (file-name-directory source)))
          "patches/"))

(defun bone-patch-file (info patch)
  "Return a local file for PATCH of report INFO, fetching it if absent.
PATCH is an entry of the report `:patches' list."
  (let* ((file   (alist-get 'file patch))
         (source (plist-get info :source))
         (sname  (or (plist-get info :source-name) "unknown"))
         (cache  (expand-file-name (concat "cache/patches/" sname "/" file)
                                   bone-config-dir)))
    (unless (file-exists-p cache)
      (let ((loc (concat (bone--patches-base source) file)))
        (cond ((bone--http-url-p source) (bone--fetch-url-to-file loc cache))
              ((file-exists-p loc)
               (make-directory (file-name-directory cache) t)
               (copy-file loc cache t)))))
    (and (file-exists-p cache) cache)))

;;; Reading the report message

(defcustom bone-open-message-method '((t . auto))
  "How `bone-read-message' opens a report's email, per source.
An alist mapping a source name to a method; the entry keyed by t is the
default for sources not listed.  A source name is BARK's source, the
`:name' of a `:sources' entry in config.edn.  Methods: `auto' uses
`bone-open-message-function' if set, else the web archive; `mua' forces
that function; `gnus', `notmuch' and `mu4e' open in that MUA by
message-id (Gnus also reads `bone-gnus-group'); `web' forces the web
archive."
  :type '(alist :key-type (choice (const :tag "Default (any source)" t)
                                  (string :tag "Source name"))
                :value-type (choice (const :tag "Auto" auto)
                                    (const :tag "MUA function" mua)
                                    (const :tag "Gnus" gnus)
                                    (const :tag "Notmuch" notmuch)
                                    (const :tag "mu4e" mu4e)
                                    (const :tag "Web archive" web)))
  :group 'bone)

(defcustom bone-gnus-group nil
  "Alist mapping a source name to the Gnus group holding its mails.
Used by the `gnus' open method.  For a source not listed, the group is
asked for with completion, falling back to the Gnus registry."
  :type '(alist :key-type (string :tag "Source name")
                :value-type (string :tag "Gnus group"))
  :group 'bone)

(defvar bone-open-message-function nil
  "Function (MID INFO) a front-end sets to open a message in its MUA.")

(defun bone--method-for (info)
  "Return the open method for report INFO's source."
  (let ((m bone-open-message-method)
        (name (plist-get info :source-name)))
    (cond ((symbolp m) m)
          ((and name (assoc name m)) (cdr (assoc name m)))
          ((assq t m) (cdr (assq t m)))
          (t 'auto))))

(defun bone--gnus-group-for (info)
  "Return the configured Gnus group for report INFO's source, or nil.
A string `bone-gnus-group' applies to every source; an alist maps a
source name to its group."
  (let ((g bone-gnus-group)
        (name (plist-get info :source-name)))
    (cond ((stringp g) g)
          ((and name (assoc name g)) (cdr (assoc name g))))))

(defvar bone--message-archive-url nil
  "Web archive URL of the message shown in the current buffer.")

(defun bone--strip-mid (mid)
  "Return message-id MID without surrounding angle brackets."
  (replace-regexp-in-string "\\`<\\|>\\'" "" mid))

(defun bone-message-archive-url (mid info)
  "Return the web archive URL for MID using INFO, or nil."
  (or (plist-get info :archived-at)
      (let* ((source (plist-get info :source))
             (meta (and source (bone-source-meta source)))
             (fmt (and meta (alist-get 'archive-format-string meta))))
        (and fmt (format fmt (bone--strip-mid mid))))))

(defvar bone-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'bone-message-browse)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `bone-message-mode'.")

(define-derived-mode bone-message-mode special-mode "Bone-Message"
  "Major mode for a fetched BARK report message.")

(defun bone-message-browse ()
  "Open the current message's web archive page in a browser."
  (interactive)
  (if bone--message-archive-url
      (browse-url bone--message-archive-url)
    (user-error "No archive URL for this message")))

(declare-function quoted-printable-decode-string "qp")
(declare-function rfc2047-decode-string "rfc2047")

(defun bone--decode-message (raw)
  "Decode raw RFC822 message string RAW for display (best effort).
Decode encoded-word headers, and the single part's transfer-encoding
and charset; multipart bodies are decoded as UTF-8."
  (require 'qp)
  (require 'rfc2047)
  (let* ((case-fold-search t)
         (sep (string-match "\r?\n\r?\n" raw))
         (headers (if sep (substring raw 0 sep) raw))
         (body (if sep (substring raw (match-end 0)) ""))
         (cte (and (string-match
                    "^content-transfer-encoding:[ \t]*\\([^ \t\r\n]+\\)" headers)
                   (downcase (match-string 1 headers))))
         (charset (and (string-match "charset=\"?\\([^\";[:space:]]+\\)" headers)
                       (downcase (match-string 1 headers))))
         (coding (let ((c (and charset (intern charset))))
                   (if (and c (coding-system-p c)) c 'utf-8)))
         (bytes (cond ((string-match-p "^content-type:[ \t]*multipart/" headers) body)
                      ((equal cte "quoted-printable")
                       (quoted-printable-decode-string body))
                      ((equal cte "base64")
                       (or (ignore-errors
                             (base64-decode-string
                              (replace-regexp-in-string "[ \t\r\n]" "" body)))
                           body))
                      (t body))))
    (concat (rfc2047-decode-string headers) "\n\n"
            (decode-coding-string bytes coding))))

(defun bone--show-message-web (mid info)
  "Fetch and display the message for MID using INFO, decoded for reading."
  (let ((url (bone-message-archive-url mid info)))
    (unless url (error "No web archive URL for %s" mid))
    (let ((raw (bone--http-body (concat url "/raw") t)))
      (with-current-buffer (get-buffer-create "*bone-message*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (bone--decode-message raw))
          (goto-char (point-min)))
        (bone-message-mode)
        (setq bone--message-archive-url url)
        (pop-to-buffer (current-buffer))))))

(declare-function gnus "gnus")
(declare-function gnus-alive-p "gnus-util")
(declare-function gnus-activate-group "gnus-start")
(declare-function gnus-group-read-group "gnus-group")
(declare-function gnus-summary-goto-article "gnus-sum")
(declare-function gnus-group-completing-read "gnus-group")
(declare-function gnus-registry-get-id-key "gnus-registry")
(declare-function notmuch-show "notmuch-show")
(declare-function mu4e "mu4e")
(declare-function mu4e-view-message-with-message-id "mu4e-view")
(declare-function mu4e-search "mu4e-search")
(declare-function mu4e-headers-search "mu4e-headers")
(defvar gnus-registry-enabled)

(defun bone--read-gnus-group (prompt)
  "Read a Gnus group with PROMPT, completing over all known groups.
Start Gnus first when needed so the group list is populated."
  (require 'gnus)
  (unless (gnus-alive-p) (gnus))
  (gnus-group-completing-read prompt))

(defun bone--gnus-return-to (buffer)
  "Switch back to BUFFER once the next Gnus summary is exited."
  (when (buffer-live-p buffer)
    (letrec ((fn (lambda ()
                   (remove-hook 'gnus-summary-exit-hook fn)
                   (run-at-time 0 nil
                                (lambda ()
                                  (when (buffer-live-p buffer)
                                    (switch-to-buffer buffer)))))))
      (add-hook 'gnus-summary-exit-hook fn))))

(defun bone--show-message-gnus (mid info)
  "Open MID in Gnus using INFO's source group, the registry, or a prompt.
Enter the group reading a single article (avoiding a full fetch of a
large group), then select MID by its message-id; return to the calling
buffer on summary exit."
  (require 'gnus)
  (let ((origin (current-buffer)))
    (unless (gnus-alive-p) (gnus))
    (let* ((id (bone-normalize-mid mid))
           (cfg (bone--gnus-group-for info))
           (group (or (and cfg (not (string-empty-p cfg)) cfg)
                      (and (bound-and-true-p gnus-registry-enabled)
                           (fboundp 'gnus-registry-get-id-key)
                           (car (gnus-registry-get-id-key id 'group)))
                      (bone--read-gnus-group "Gnus group for this message: "))))
      (gnus-activate-group group)
      (gnus-group-read-group 1 t group)
      (gnus-summary-goto-article id nil t)
      (bone--gnus-return-to origin))))

(defun bone--show-message-notmuch (mid info)
  "Open MID in Notmuch by message-id (INFO is unused)."
  (ignore info)
  (require 'notmuch)
  (notmuch-show (concat "id:" (bone--strip-mid mid))))

(defun bone--show-message-mu4e (mid info)
  "Open MID in mu4e by message-id (INFO is unused)."
  (ignore info)
  (require 'mu4e)
  (let ((id (bone--strip-mid mid)))
    (cond
     ((fboundp 'mu4e-view-message-with-message-id)
      (mu4e-view-message-with-message-id id))
     ((fboundp 'mu4e-search) (mu4e-search (concat "msgid:" id)))
     ((fboundp 'mu4e-headers-search) (mu4e-headers-search (concat "msgid:" id)))
     (t (user-error "Cannot open by message-id in this mu4e version")))))

(declare-function customize-save-variable "cus-edit")

(defun bone--alist-put (alist key val)
  "Return ALIST with KEY set to VAL, KEY compared with `equal'."
  (cons (cons key val)
        (assoc-delete-all key (copy-alist (if (listp alist) alist nil)))))

(defun bone--known-source-names ()
  "Return known source names from each source's meta.json."
  (delete-dups
   (delq nil (mapcar (lambda (s) (alist-get 'source (bone-source-meta s)))
                     (bone-sources)))))

;;;###autoload
(defun bone-set-source-open-method (source method &optional group)
  "Set the open METHOD for SOURCE (a source name, or t for the default).
For Gnus, also store GROUP.  Interactively, complete SOURCE, METHOD and
\(for Gnus) GROUP, then save with Customize."
  (interactive
   (let* ((s (completing-read "Source name (empty = default): "
                              (bone--known-source-names) nil nil))
          (src (if (string-empty-p s) t s))
          (m (intern (completing-read
                      "Open with: "
                      '("auto" "mua" "gnus" "notmuch" "mu4e" "web") nil t)))
          (g (when (eq m 'gnus)
               (bone--read-gnus-group "Gnus group (empty = ask each time): "))))
     (list src m g)))
  (customize-save-variable 'bone-open-message-method
                           (bone--alist-put bone-open-message-method source method))
  (when (and (eq method 'gnus) group (not (string-empty-p group)))
    (customize-save-variable 'bone-gnus-group
                             (bone--alist-put bone-gnus-group source group)))
  method)

(defun bone-read-message (mid info)
  "Open the email for MID using INFO per `bone-open-message-method'."
  (pcase (bone--method-for info)
    ('mua (if bone-open-message-function
              (funcall bone-open-message-function mid info)
            (user-error "No `bone-open-message-function' set")))
    ('gnus (bone--show-message-gnus mid info))
    ('notmuch (bone--show-message-notmuch mid info))
    ('mu4e (bone--show-message-mu4e mid info))
    ('web (bone--show-message-web mid info))
    (_ (if bone-open-message-function
           (funcall bone-open-message-function mid info)
         (bone--show-message-web mid info)))))

;;; Viewing and applying patches

(defcustom bone-apply-repo nil
  "Fallback git repository for `bone-apply-patches'.
A source's `:repo' in config.edn takes precedence; this is used only
when the source has none, before asking interactively."
  :type '(choice (const :tag "Ask each time" nil) directory)
  :group 'bone)

(defun bone--series-complete-p (info)
  "Return non-nil unless INFO has an explicitly incomplete `:series'."
  (let ((series (plist-get info :series)))
    (or (null series) (alist-get 'complete series))))

(defun bone--series-id (info)
  "Return the patch-series id of report INFO, or nil."
  (alist-get 'id (plist-get info :series)))

(defun bone--patch-seq-n (info)
  "Leading integer of INFO's `:patch-seq' (\"2/5\" -> 2), or 0."
  (let ((s (plist-get info :patch-seq)))
    (if (and s (string-match "\\`\\([0-9]+\\)" s))
        (string-to-number (match-string 1 s))
      0)))

(defun bone--cover-p (info)
  "Non-nil if report INFO is a series cover letter (patch-seq \"0/...\")."
  (string-prefix-p "0/" (or (plist-get info :patch-seq) "")))

(defun bone--series-summary (members)
  "Summarize series MEMBERS (INFO plists) like \"2 acked, 1 open\".
Cover letters are excluded from the tally."
  (let ((acked 0) (closed 0) (open 0))
    (dolist (m members)
      (unless (bone--cover-p m)
        (let ((f (or (plist-get m :flags) "---")))
          (cond ((and (>= (length f) 3) (not (eq (aref f 2) ?-))) (cl-incf closed))
                ((and (>= (length f) 1) (not (eq (aref f 0) ?-))) (cl-incf acked))
                (t (cl-incf open))))))
    (string-join (delq nil (list (and (> acked 0) (format "%d acked" acked))
                                 (and (> closed 0) (format "%d closed" closed))
                                 (and (> open 0) (format "%d open" open))))
                 ", ")))

(defun bone-view-patches (info)
  "Show the patches of report INFO in a `diff-mode' buffer."
  (let ((patches (plist-get info :patches)))
    (unless patches (user-error "This report has no patches"))
    (with-current-buffer (get-buffer-create "*bone-patches*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (p patches)
          (let ((f (bone-patch-file info p)))
            (when f
              (insert-file-contents f)
              (goto-char (point-max)))))
        (goto-char (point-min)))
      (diff-mode)
      (pop-to-buffer (current-buffer)))))

(defun bone--run-git-patches (info subcommand hint)
  "Apply INFO's patches in its repo via git SUBCOMMAND (\"apply\" or \"am\").
HINT is shown on failure."
  (let ((patches (plist-get info :patches)))
    (unless patches (user-error "This report has no patches"))
    (when (and (not (bone--series-complete-p info))
               (not (yes-or-no-p "Patch series looks incomplete; apply anyway? ")))
      (user-error "Aborted"))
    (let* ((repo (or (bone--source-repo info)
                     bone-apply-repo
                     (read-directory-name "Apply in git repo: ")))
           (files (delq nil (mapcar (lambda (p) (bone-patch-file info p)) patches)))
           (default-directory (file-name-as-directory repo)))
      (with-current-buffer (get-buffer-create "*bone-git*")
        (let ((inhibit-read-only t)) (erase-buffer))
        (let ((status (apply #'call-process "git" nil t nil subcommand "--" files)))
          (if (zerop status)
              (message "bone: git %s applied %d patch(es) in %s"
                       subcommand (length files) repo)
            (display-buffer (current-buffer))
            (message "bone: git %s failed in %s (%s)" subcommand repo hint)))))))

(defun bone-apply-patches (info)
  "Apply INFO's patches to the working tree with `git apply'."
  (bone--run-git-patches info "apply" "rejects left as .rej"))

(defun bone-am-patches (info)
  "Apply INFO's patches as commits with `git am'."
  (bone--run-git-patches info "am" "run `git am --abort' to undo"))

;;; Query filter (subset of the BARK web search syntax)

(defvar bone-list--query nil
  "Active `bone-list' filter query string, or nil.  Buffer-local in use.")

(defvar bone-list--expanded nil
  "Series ids unfolded in the current `bone-list' buffer.  Buffer-local.")

(defun bone--query-text-match (needle hay)
  "Non-nil if HAY contains NEEDLE, case-insensitively.
NEEDLE `*' matches any non-empty HAY; empty NEEDLE matches anything."
  (let ((hay (or hay "")))
    (cond ((equal needle "*") (not (string-empty-p hay)))
          ((or (null needle) (string-empty-p needle)) t)
          (t (let ((case-fold-search t))
               (and (string-match-p (regexp-quote needle) hay) t))))))

(defun bone--query-ymd->days (s)
  "Absolute day number for the YYYY-MM-DD prefix of S, or nil."
  (when (and s (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" s))
    (time-to-days (encode-time 0 0 0
                               (string-to-number (match-string 3 s))
                               (string-to-number (match-string 2 s))
                               (string-to-number (match-string 1 s))))))

(defun bone--query-duration->days (s)
  "Number of days for a duration S like 3d, 2w or 2m, or nil."
  (when (and s (string-match "\\`\\([0-9]+\\)\\([dwm]\\)\\'" s))
    (* (string-to-number (match-string 1 s))
       (pcase (match-string 2 s) ("d" 1) ("w" 7) ("m" 30)))))

(defun bone--query-date-match (spec field forward)
  "Non-nil if FIELD's date satisfies SPEC; FORWARD looks ahead from today.
SPEC is a duration (3d/2w/2m), a YYYY-MM-DD date, or an A..B range whose
ends are dates or durations and may be empty."
  (let ((fd (bone--query-ymd->days field))
        (today (time-to-days (current-time))))
    (when fd
      (if (string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" spec)
          (let* ((a (match-string 1 spec)) (b (match-string 2 spec))
                 (lo (or (bone--query-ymd->days a)
                         (let ((d (bone--query-duration->days a))) (and d (- today d)))))
                 (hi (or (bone--query-ymd->days b)
                         (let ((d (bone--query-duration->days b))) (and d (+ today d))))))
            (and (or (null lo) (>= fd lo)) (or (null hi) (<= fd hi))))
        (let ((d (bone--query-duration->days spec))
              (single (bone--query-ymd->days spec)))
          (cond (d (if forward (and (>= fd today) (<= fd (+ today d)))
                     (and (<= fd today) (>= fd (- today d)))))
                (single (= fd single))))))))

(defun bone--query-flag (info pos ch)
  "Non-nil if INFO's :flags has character CH at position POS."
  (let ((f (or (plist-get info :flags) "")))
    (and (> (length f) pos) (eq (aref f pos) ch))))

(defun bone--query-closed-p (info)
  "Non-nil if INFO has a close-reason flag set."
  (let ((f (or (plist-get info :flags) "")))
    (and (>= (length f) 3) (not (eq (aref f 2) ?-)))))

(defun bone--query-token-match (token mid info)
  "Non-nil if search TOKEN matches report MID with INFO."
  (let* ((i (string-search ":" token))
         (key (and i (substring token 0 i)))
         (val (and i (substring token (1+ i))))
         (prio (or (plist-get info :priority) 0)))
    (if (null key)
        (bone--query-text-match token (plist-get info :subject))
      (pcase key
        ((or "from" "f")
         (or (bone--query-text-match val (plist-get info :from))
             (bone--query-text-match val (plist-get info :from-name))))
        ((or "subject" "s") (bone--query-text-match val (plist-get info :subject)))
        ((or "topic" "t") (bone--query-text-match val (plist-get info :topic)))
        ("type" (and val (equal (downcase val)
                                (downcase (or (plist-get info :type) "")))))
        ((or "priority" "p") (equal val (number-to-string prio)))
        ((or "mid" "m") (bone--query-text-match val mid))
        ((or "acked" "a") (bone--query-flag info 0 ?A))
        ((or "owned" "o") (bone--query-flag info 1 ?O))
        ((or "closed" "c") (bone--query-closed-p info))
        ((or "urgent" "u") (= (logand prio 2) 2))
        ((or "important" "i") (= (logand prio 1) 1))
        ((or "date" "d") (bone--query-date-match val (plist-get info :date) nil))
        ((or "deadline" "D") (bone--query-date-match val (plist-get info :deadline) t))
        ((or "expired" "e") (bone--query-date-match val (plist-get info :expiry) t))
        (_ (bone--query-text-match token (plist-get info :subject)))))))

(defun bone--query-match (query mid info)
  "Non-nil if report MID/INFO matches QUERY (`|' = OR, space = AND)."
  (seq-some
   (lambda (grp)
     (seq-every-p (lambda (tok) (bone--query-token-match tok mid info))
                  (split-string grp "[ \t]+" t)))
   (split-string query "|" t)))

(defconst bone--query-keys
  '("from:" "subject:" "topic:" "type:" "priority:" "mid:" "acked:"
    "owned:" "closed:" "urgent:" "important:" "date:" "deadline:" "expired:")
  "Long-form query keys completed in `bone-list-filter'.")

(defun bone--filter-completion (string pred action)
  "Completion table completing the query key of STRING's last token.
PRED and ACTION are the usual `completing-read' arguments; the rest of
the query (earlier tokens) is left untouched."
  (let ((beg (progn (string-match "[^ \t|]*\\'" string) (match-beginning 0))))
    (if (eq (car-safe action) 'boundaries)
        (let ((suffix (cdr action)))
          `(boundaries ,beg . ,(or (string-match "[ \t|]" suffix) (length suffix))))
      (complete-with-action action bone--query-keys (substring string beg) pred))))

(defconst bone-report-types
  '("bug" "patch" "request" "announcement" "change" "release")
  "BARK report types, offered when filtering by type.")

(defun bone-list-filter-by (key)
  "Limit the list to reports whose KEY field matches a read value.
Read the value (completing types, `*' for flag fields), then set the
query to `KEY:value'; an empty value clears the filter."
  (let* ((flag (member key '("acked" "owned" "closed" "urgent" "important")))
         (val (cond (flag "*")
                    ((equal key "type") (completing-read "Type: " bone-report-types))
                    (t (read-string (format "%s: " key))))))
    (setq-local bone-list--query
                (and val (not (string-empty-p val)) (format "%s:%s" key val)))
    (bone-list-refresh)
    (if bone-list--query
        (message "bone: filter %s" bone-list--query)
      (message "bone: filter cleared"))))

(defvar bone-list-filter-map
  (let ((map (make-sparse-keymap)))
    (dolist (pair '(("f" . "from") ("t" . "type") ("T" . "topic")
                    ("s" . "subject") ("p" . "priority") ("m" . "mid")
                    ("d" . "date") ("D" . "deadline") ("e" . "expired")
                    ("a" . "acked") ("o" . "owned") ("c" . "closed")
                    ("u" . "urgent") ("i" . "important")))
      (let ((field (cdr pair)))
        (define-key map (car pair)
                    (lambda () (interactive) (bone-list-filter-by field)))))
    map)
  "Keymap bound to `f' in `bone-list-mode' to filter by one field.")

;;; Report browser (bone-list)

(defvar bone-list-columns
  '(("Mark"      5 t :mark)
    ("Type"      8 t :type)
    ("Flags"     5 t :flags)
    ("Pri"       4 t :priority)
    ("Votes"     5 t :votes)
    ("From"     18 t :from-name)
    ("Subject"  50 t :subject)
    ("Created"  11 t :date)
    ("Activity" 11 t :last-activity)
    ("Topic"    16 t :topic))
  "Columns for `bone-list-mode': (NAME WIDTH SORT KEY) tuples.
The Subject width is recomputed to fill the window by `bone--list-format'.")

(defun bone--list-cell (key info mid state)
  "Return the display string for column KEY of report MID (INFO, STATE)."
  (pcase key
    (:mark (concat (cond ((plist-get info :series-summary) "+")
                         ((plist-get info :series-child) " ")
                         (t ""))
                   (if (bone-action-on-p state mid :todo) "T" "")
                   (if (bone-action-on-p state mid :sticky) "S" "")
                   (if (bone-action-on-p state mid :read) "" "*")
                   (if (plist-get info :patches) "P" "")))
    (:priority (number-to-string (or (plist-get info :priority) 0)))
    (:date (let ((d (plist-get info :date)))   ; keep the YYYY-MM-DD part only
             (if d (substring d 0 (min 10 (length d))) "")))
    (:subject (let ((s (or (plist-get info :subject) "")))
                (cond ((plist-get info :series-child) (concat "  " s))
                      ((plist-get info :series-summary)
                       (format "%s  (%s)" s (plist-get info :series-summary)))
                      (t s))))
    (_ (let ((v (plist-get info key))) (if v (format "%s" v) "")))))

(defun bone--active-columns ()
  "Return `bone-list-columns' minus those named in config `:skip-columns'."
  (let ((skip (mapcar #'downcase (plist-get (bone-load-config) :skip-columns))))
    (cl-remove-if (lambda (c) (member (downcase (car c)) skip)) bone-list-columns)))

(defun bone--list-format ()
  "Return the `tabulated-list-format' vector for the active columns.
Grow the Subject column to fill the window; other columns keep their
width, so Topic stays at the right edge."
  (let* ((cols (bone--active-columns))
         (others (cl-remove-if (lambda (c) (eq (nth 3 c) :subject)) cols))
         (used (apply #'+ tabulated-list-padding 1
                      (mapcar (lambda (c) (1+ (nth 1 c))) others)))
         (subj (max 20 (- (window-body-width) used))))
    (vconcat (mapcar (lambda (c)
                       (list (nth 0 c)
                             (if (eq (nth 3 c) :subject) subj (nth 1 c))
                             (nth 2 c)))
                     cols))))

(defun bone--list-entries ()
  "Return `tabulated-list-entries', folding patch series unless expanded.
A series is shown as one representative row (cover letter or first
patch) with a status summary; unfolded series (in `bone-list--expanded')
list each patch.  The query in `bone-list--query' filters the result."
  (let ((state (bone-read-state))
        (cols (bone--active-columns))
        (query bone-list--query)
        (pairs (bone-reports))
        (groups (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (rows nil))
    (dolist (p pairs)
      (let ((sid (bone--series-id (cdr p))))
        (when sid (push p (gethash sid groups)))))
    (cl-flet ((row (pair)
                (when (or (null query) (bone--query-match query (car pair) (cdr pair)))
                  (push (list pair
                              (vconcat
                               (mapcar (lambda (c)
                                         (bone--list-cell (nth 3 c) (cdr pair) (car pair) state))
                                       cols)))
                        rows))))
      (dolist (p pairs)
        (let ((sid (bone--series-id (cdr p))))
          (cond
           ((null sid) (row p))
           ((gethash sid seen) nil)
           (t
            (puthash sid t seen)
            (let* ((members (sort (nreverse (gethash sid groups))
                                  (lambda (a b)
                                    (< (bone--patch-seq-n (cdr a))
                                       (bone--patch-seq-n (cdr b))))))
                   (multi (cdr members)))
              (if (and multi (member sid bone-list--expanded))
                  (dolist (m members)
                    (row (cons (car m)
                               (plist-put (copy-sequence (cdr m)) :series-child t))))
                (let* ((cover (seq-find (lambda (m) (bone--cover-p (cdr m))) members))
                       (rep (or cover (car members))))
                  (row (cons (car rep)
                             (if multi
                                 (plist-put (copy-sequence (cdr rep)) :series-summary
                                            (bone--series-summary (mapcar #'cdr members)))
                               (cdr rep)))))))))))
      (nreverse rows))))

(defvar bone-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'bone-list-open)
    (define-key map (kbd "TAB") #'bone-list-toggle-fold)
    (define-key map "p" #'bone-list-view-patches)
    (define-key map "a" #'bone-list-apply-patches)
    (define-key map "A" #'bone-list-am-patches)
    (define-key map "g" #'bone-list-refresh)
    (define-key map "G" #'bone-list-update)
    (define-key map "/" #'bone-list-filter)
    (define-key map "f" bone-list-filter-map)
    (define-key map "t" #'bone-list-limit-type)
    (define-key map "r" #'bone-list-toggle-read)
    (define-key map "!" #'bone-list-toggle-todo)
    (define-key map "*" #'bone-list-toggle-sticky)
    map)
  "Keymap for `bone-list-mode'.")

(define-derived-mode bone-list-mode tabulated-list-mode "Bone"
  "Major mode listing open BARK reports."
  (setq tabulated-list-format (bone--list-format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun bone-list-refresh ()
  "Reload reports and local state into the current list buffer."
  (interactive)
  (setq tabulated-list-format (bone--list-format))
  (tabulated-list-init-header)
  (setq mode-line-process (and bone-list--query
                               (format " [%s]" bone-list--query)))
  (setq tabulated-list-entries (bone--list-entries))
  (tabulated-list-print t)
  (force-mode-line-update))

(defun bone-list-filter (query)
  "Filter the report list by QUERY; an empty QUERY clears the filter.
QUERY combines `key:value' tokens with spaces (AND) and `|' (OR)."
  (interactive
   (list (completing-read "Filter: " #'bone--filter-completion nil nil bone-list--query)))
  (setq-local bone-list--query
              (and (not (string-empty-p (string-trim query))) query))
  (bone-list-refresh)
  (if bone-list--query
      (message "bone: filter %s" bone-list--query)
    (message "bone: filter cleared")))

(defun bone-list-update ()
  "Refresh the remote cache, then reload the list."
  (interactive)
  (bone-update)
  (bone-list-refresh))

(defun bone-list-limit-type ()
  "Limit the list to a chosen report type."
  (interactive)
  (bone-list-filter-by "type"))

(defun bone-list--current ()
  "Return the (MID . INFO) pair at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No report at point")))

(defun bone-list-open ()
  "Open the email of the report at point."
  (interactive)
  (let ((p (bone-list--current)))
    (bone-read-message (car p) (cdr p))))

(defun bone-list-view-patches ()
  "View the patches of the report at point."
  (interactive)
  (bone-view-patches (cdr (bone-list--current))))

(defun bone-list-apply-patches ()
  "Apply the patches of the report at point with `git apply'."
  (interactive)
  (bone-apply-patches (cdr (bone-list--current))))

(defun bone-list-am-patches ()
  "Apply the patches of the report at point with `git am'."
  (interactive)
  (bone-am-patches (cdr (bone-list--current))))

(defun bone-list-toggle-fold ()
  "Fold or unfold the patch series of the report at point."
  (interactive)
  (let* ((pair (bone-list--current))
         (sid (bone--series-id (cdr pair)))
         (mid (car pair)))
    (unless sid (user-error "Not part of a patch series"))
    (setq-local bone-list--expanded
                (if (member sid bone-list--expanded)
                    (remove sid bone-list--expanded)
                  (cons sid bone-list--expanded)))
    (bone-list-refresh)
    (goto-char (point-min))
    (while (and (not (eobp))
                (let ((p (tabulated-list-get-id)))
                  (not (and p (equal (car p) mid)))))
      (forward-line 1))))

(defun bone-list--toggle (action)
  "Toggle local mark ACTION on the report at point, then refresh."
  (let ((p (bone-list--current)))
    (bone-toggle-mark (car p) (cdr p) action)
    (bone-list-refresh)))

(defun bone-list-toggle-read ()
  "Toggle the read mark on the report at point."
  (interactive)
  (bone-list--toggle :read))

(defun bone-list-toggle-todo ()
  "Toggle the todo mark on the report at point."
  (interactive)
  (bone-list--toggle :todo))

(defun bone-list-toggle-sticky ()
  "Toggle the sticky mark on the report at point."
  (interactive)
  (bone-list--toggle :sticky))

(defun bone--resolve-reports-dir (input)
  "Return the reports directory URL (ending in /) for user INPUT.
A `.json' (including `meta.json') URL yields its directory; a URL
ending in /reports yields that directory; otherwise /reports/ is
appended."
  (let ((u (replace-regexp-in-string "/+\\'" "" (string-trim input))))
    (cond ((string-suffix-p ".json" u) (file-name-directory u))
          ((string-suffix-p "/reports" u) (concat u "/"))
          (t (concat u "/reports/")))))

(defun bone--read-config-raw ()
  "Read config.edn and return its raw alist, or nil."
  (let ((file (expand-file-name "config.edn" bone-config-dir)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (bone-edn-read-buffer))))))

(defun bone--config-add-source (config urls name repo)
  "Return CONFIG alist with a source (URLS, NAME, REPO) added or updated.
URLS is a list of reports.json URLs.  An existing source sharing any
URL or the NAME is replaced."
  (let* ((src (append (list (cons :urls urls))
                      (and name (list (cons :name name)))
                      (and repo (list (cons :repo repo)))))
         (sources (alist-get :sources config))
         (others (cl-remove-if
                  (lambda (s) (or (seq-intersection urls (alist-get :urls s))
                                  (and name (equal name (alist-get :name s)))))
                  sources)))
    (cons (cons :sources (append others (list src)))
          (assq-delete-all :sources (copy-alist config)))))

(defun bone--write-config (config)
  "Write CONFIG alist to config.edn as UTF-8."
  (let ((file (expand-file-name "config.edn" bone-config-dir)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert (bone--edn-write config) "\n")))))

(declare-function completing-read-multiple "crm")

;;;###autoload
(defun bone-add-source (urls &optional name repo)
  "Add a source (URLS, NAME, REPO) to config.edn.
URLS is a list of reports.json URLs.  Interactively, give a base,
meta.json or reports.json URL; the report files listed in meta.json's
`reports-files' are offered for selection (default all-open.json), the
source NAME (from meta.json) and its local git REPO are requested."
  (interactive
   (let* ((input (read-string "Report source URL (base, meta.json or .json): "))
          (_ (when (string-empty-p (string-trim input)) (user-error "No URL given")))
          (dir (bone--resolve-reports-dir input))
          (meta (ignore-errors (bone-source-meta dir)))
          (files (or (alist-get 'reports-files meta) '("all.json")))
          (def (cond ((member "all-open.json" files) "all-open.json")
                     ((member "all.json" files) "all.json")
                     (t (car files))))
          (chosen (or (completing-read-multiple
                       (format "Report file(s) (default %s): " def)
                       files nil t nil nil def)
                      (list def)))
          (sname (alist-get 'source meta))
          (name (read-string
                 (format "Source name%s: " (if sname (format " (default %s)" sname) ""))
                 nil nil sname))
          (repo (when (y-or-n-p "Link a local git repo (for git am)? ")
                  (expand-file-name (read-directory-name "Git repo: ")))))
     (list (mapcar (lambda (f) (concat dir f)) chosen) name repo)))
  (let ((urls (if (listp urls) urls (list urls)))
        (name (and name (not (string-empty-p (string-trim name))) name)))
    (unless urls (user-error "No report file selected"))
    (bone--write-config (bone--config-add-source (bone--read-config-raw) urls name repo))
    (message "bone: added to config.edn: %s%s"
             (string-join urls ", ") (if name (format " (%s)" name) ""))
    urls))

(defun bone--setup-sources ()
  "Interactively add sources to config.edn until the user declines."
  (while (y-or-n-p (if (bone-sources) "Add another source? " "Add a source? "))
    (call-interactively #'bone-add-source)))

;;;###autoload
(defun bone ()
  "Browse open BARK reports in a tabulated list filling the frame.
Prompt to add a source when none is configured."
  (interactive)
  (unless (bone-sources)
    (bone--setup-sources))
  (let ((buf (get-buffer-create "*bone*")))
    (switch-to-buffer buf)
    (delete-other-windows)
    (bone-list-mode)
    (bone-list-refresh)))

(provide 'bone)
;;; bone.el ends here
