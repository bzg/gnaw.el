;;; gnaw.el --- Browse and manage BONE reports in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail, news
;; URL: https://codeberg.org/bzg/gnaw.el
;; Version: 0.17.1
;; Package-Requires: ((emacs "28.1") (transient "0.3.7"))

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
;;   3==3  GNAW -- GNAW is Not Another Workflow
;;
;; Browser and shared data layer for BONE/gnaw in Emacs.  `M-x gnaw'
;; opens a report browser; the same data layer backs the mail front-ends
;; gnus-gnaw, notmuch-gnaw and mu4e-gnaw.  This library:
;;
;; - reads the gnaw configuration (`config.edn') and report sources,
;; - manages the local cache of remote `reports.json' files,
;; - parses and serializes the EDN config and `state.edn' files shared
;;   with the gnaw CLI,
;; - exposes the report list, the local-mark API and the MUA-independent
;;   presentation helpers (annotation string, topic filtering) the
;;   front-ends use.
;;
;; Front-ends provide the message metadata (an INFO plist with keys
;; :type :subject :date :from :from-name, plus display keys :flags
;; :priority :votes :deadline :expiry :last-activity :replies :topic) and
;; the presentation; this library provides everything independent of
;; the mail user agent.
;;
;; Entry points:
;;
;;   `gnaw'              browse reports (interactive)
;;   `gnaw-reports'      collect open reports from all sources
;;   `gnaw-update'       force-refresh the local cache (interactive)
;;   `gnaw-toggle-mark'  toggle :sticky/:dismiss for a message-id
;;   `gnaw-read-state' / `gnaw-write-state'   state.edn I/O
;;   `gnaw-annotation'   fixed-width report annotation for MUA lines
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'transient)

(defvar url-http-response-status)

(defgroup gnaw nil
  "Read and manage BONE reports shared with the gnaw CLI."
  :group 'mail)

(defconst gnaw-version (or (package-get-version) "0.17.1")
  "Version of gnaw.el, read from its package header.")

;;;###autoload
(defun gnaw-version ()
  "Display the version of gnaw.el."
  (interactive)
  (message "gnaw.el %s" gnaw-version))

(defcustom gnaw-config-dir "~/.config/gnaw"
  "Directory containing gnaw configuration and state/cache files."
  :type 'directory
  :group 'gnaw)

(defcustom gnaw-after-update-hook nil
  "Functions run after `gnaw-update' refreshes the local cache.
Front-ends can use this to re-apply their display."
  :type 'hook
  :group 'gnaw)

(defconst gnaw-supported-bone-format "0.9.2"
  "Minimum reports.json bone-format gnaw.el reads without warning.")

;;; EDN reader/writer (the subset emitted by gnaw's config.edn and state.edn)

(defun gnaw-edn--skip-ws ()
  "Skip EDN whitespace, commas and line comments at point."
  (skip-chars-forward " \t\n\r,")
  (while (eq (char-after) ?\;)
    (forward-line 1)
    (skip-chars-forward " \t\n\r,")))

(defun gnaw-edn--read ()
  "Read one EDN value at point."
  (gnaw-edn--skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "EDN: unexpected EOF"))
     ((eq c ?\") (read (current-buffer)))
     ((eq c ?:)  (gnaw-edn--read-keyword))
     ((eq c ?\{) (gnaw-edn--read-map))
     ((eq c ?\[) (gnaw-edn--read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                           (and d (>= d ?0) (<= d ?9)))))
      (gnaw-edn--read-number))
     (t (gnaw-edn--read-symbol)))))

(defun gnaw-edn--read-keyword ()
  "Read an EDN keyword at point."
  (forward-char 1)
  (let ((start (1- (point))))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (intern (buffer-substring-no-properties start (point)))))

(defun gnaw-edn--read-symbol ()
  "Read an EDN symbol at point.
Signal an error on a character that starts no known EDN value (such
as dispatch forms like #inst), which would otherwise loop forever."
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (when (= (point) start)
      (error "EDN: unexpected character %c" (char-after)))
    (pcase (buffer-substring-no-properties start (point))
      ("nil"   nil)
      ("true"  t)
      ("false" nil)
      (s       (intern s)))))

(defun gnaw-edn--read-number ()
  "Read an EDN number at point."
  (let ((start (point)))
    (skip-chars-forward "0-9.eE+-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun gnaw-edn--read-map ()
  "Read an EDN map at point."
  (forward-char 1)
  (let ((acc nil))
    (gnaw-edn--skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (gnaw-edn--read)))
        (gnaw-edn--skip-ws)
        (push (cons k (gnaw-edn--read)) acc))
      (gnaw-edn--skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun gnaw-edn--read-vector ()
  "Read an EDN vector at point."
  (forward-char 1)
  (let ((acc nil))
    (gnaw-edn--skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (gnaw-edn--read) acc)
      (gnaw-edn--skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun gnaw-edn--map-p (x)
  "Non-nil if list X is an alist to write as an EDN map (not a vector).
True when every element is a cons whose car is an atom (a key)."
  (and (consp x) (cl-every (lambda (e) (and (consp e) (atom (car e)))) x)))

(defun gnaw--edn-write (v)
  "Serialize EDN value V (maps, vectors and scalars) to a string."
  (cond
   ((stringp v)  (format "%S" v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((symbolp v)  (symbol-name v))
   ((gnaw-edn--map-p v)
    (concat "{" (mapconcat (lambda (kv)
                             (concat (gnaw--edn-write (car kv)) " "
                                     (gnaw--edn-write (cdr kv))))
                           v " ")
            "}"))
   ((listp v) (concat "[" (mapconcat #'gnaw--edn-write v " ") "]"))
   (t (error "EDN: cannot serialize %S" v))))

(defun gnaw-edn-read-buffer ()
  "Read one EDN map from the current buffer if it starts with `{'.
Return nil on parse failure or when no map is present."
  (goto-char (point-min))
  (gnaw-edn--skip-ws)
  (when (eq (char-after) ?\{)
    (gnaw-edn--read-map)))

(defun gnaw--read-edn-file (file)
  "Read FILE and return its top-level EDN map, or nil.
Return nil when FILE is unreadable or does not parse, reporting parse
errors as a message."
  (when (file-readable-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (gnaw-edn-read-buffer))
      (error
       (message "gnaw: cannot parse %s: %s" file (error-message-string err))
       nil))))

(defun gnaw--read-edn-map-or-signal (file)
  "Return FILE's top-level EDN map, or nil when FILE is missing or empty.
Unlike `gnaw--read-edn-file', signal a `user-error' when FILE exists
but cannot be parsed: callers rewrite FILE, and a state derived from a
misread file would silently drop its existing contents."
  (when (file-readable-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (gnaw-edn--skip-ws)
          (cond ((eobp) nil)
                ((eq (char-after) ?\{) (gnaw-edn--read-map))
                (t (error "Not an EDN map"))))
      (error (user-error "Not updating unreadable %s (%s)"
                         file (error-message-string err))))))

;;; Configuration and report sources

(defun gnaw--uri-to-path (uri)
  "Convert file:// URI to local path, otherwise return URI."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defvar gnaw--config-cache nil
  "Cons (MTIME . PLIST) caching the last `gnaw-load-config' result.")

(defun gnaw-load-config ()
  "Load `config.edn' and return a plist.
Keys: :addresses :skip-columns :source-configs (raw `:sources' maps)
and :sources (their URLs).  The result is cached until config.edn
changes."
  (let* ((file (expand-file-name "config.edn" gnaw-config-dir))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (if (and gnaw--config-cache (equal (car gnaw--config-cache) mtime))
        (cdr gnaw--config-cache)
      (let* ((cfg (gnaw--read-edn-file file))
             (addresses (alist-get :my-addresses cfg))
             (skip      (alist-get :skip-columns cfg))
             (src-cfgs  (alist-get :sources cfg))
             (sources   (mapcan (lambda (s) (append (alist-get :urls s) nil)) src-cfgs))
             (plist (list :addresses addresses :skip-columns skip
                          :source-configs src-cfgs :sources sources)))
        (setq gnaw--config-cache (cons mtime plist))
        plist))))

(defun gnaw-sources ()
  "Return report sources from config.edn as URLs or absolute local paths.
Relative paths are resolved against `gnaw-config-dir'."
  (mapcar #'gnaw--resolve-source
          (plist-get (gnaw-load-config) :sources)))

(defun gnaw--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun gnaw--resolve-source (source)
  "Resolve SOURCE to an HTTP(S) URL or an absolute local path.
HTTP(S) URLs are returned unchanged; other sources are file paths,
relative ones resolved against `gnaw-config-dir'."
  (if (gnaw--http-url-p source)
      source
    (expand-file-name (gnaw--uri-to-path source)
                      (expand-file-name gnaw-config-dir))))

(defun gnaw--source-config-entry (info)
  "Return the config.edn source entry matching report INFO, or nil.
Matched by URL or by `:name'."
  (let ((url  (plist-get info :source))
        (name (plist-get info :source-name)))
    (seq-find
     (lambda (s)
       (or (and url (member url (mapcar #'gnaw--resolve-source
                                        (alist-get :urls s))))
           (and name (equal name (alist-get :name s)))))
     (plist-get (gnaw-load-config) :source-configs))))

(defun gnaw--source-repo (info)
  "Return the local git repo for report INFO's source, or nil.
Reads `:repo' from the matching config.edn `:sources' entry."
  (when-let* ((repo (alist-get :repo (gnaw--source-config-entry info))))
    (expand-file-name repo)))

(defun gnaw--java-hash (str)
  "Calculate Java String hashCode of STR as an unsigned 32-bit integer."
  (let ((h 0)
        (len (length str)))
    (dotimes (i len)
      (setq h (logand (+ (* h 31) (aref str i)) #xffffffff)))
    h))

(defun gnaw--source-to-cache-file (src)
  "Return cache file path for remote source SRC."
  (let* ((h (format "%08x" (gnaw--java-hash src)))
         (safe (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" src))
         (prefix (substring safe 0 (min 80 (length safe)))))
    (expand-file-name
     (concat "cache/reports/" prefix "-" h ".json")
     gnaw-config-dir)))

(defcustom gnaw-http-timeout 30
  "Seconds before a synchronous HTTP fetch gives up, nil to wait forever."
  :type '(choice (const :tag "No timeout" nil) (integer :tag "Seconds"))
  :group 'gnaw)

(defun gnaw--http-body (url)
  "Return the raw HTTP body bytes of URL.
Signal an error after `gnaw-http-timeout' seconds without a response."
  (let* ((coding-system-for-read 'binary)
         (buf (url-retrieve-synchronously url t nil gnaw-http-timeout)))
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

(defun gnaw--fetch-json-from-url (url)
  "Synchronously fetch JSON from URL.
The body is read as raw bytes and decoded as UTF-8."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil))
    (json-read-from-string
     (decode-coding-string (gnaw--http-body url) 'utf-8))))

(defun gnaw--write-json-to-file (data file)
  "Write JSON DATA to FILE as UTF-8."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert (json-encode data)))))

(defun gnaw--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil))
    (if (gnaw--http-url-p source)
        (let ((cache-file (gnaw--source-to-cache-file source)))
          (if (file-exists-p cache-file)
              (json-read-file cache-file)
            (let ((data (gnaw--fetch-json-from-url source)))
              (gnaw--write-json-to-file data cache-file)
              data)))
      (json-read-file source))))

(defun gnaw-normalize-mid (mid)
  "Ensure MID has angle brackets."
  (if (string-match-p "\\`<.*>\\'" mid)
      mid
    (concat "<" mid ">")))

(defconst gnaw--relation-keys
  '(resolves resolved-by
    supersedes superseded-by
    duplicates duplicated-by
    related-to)
  "Per-kind relation fields of a report, as emitted by BONE.
`related-to' comes last: BONE poses a `related-to' companion edge
next to each closure relation, and `gnaw--report-related' dedups
by message-id keeping the first entry seen -- the specific kind
must win over the companion.")

(defun gnaw--report-related (r)
  "Merge report R's per-kind relation entries, deduped by message-id.
Each entry is annotated with a `kind' key naming the relation field
it came from."
  (let (mids entries)
    (dolist (k gnaw--relation-keys (nreverse entries))
      (dolist (e (alist-get k r))
        (when-let* ((mid (alist-get 'message-id e)))
          (unless (member mid mids)
            (push mid mids)
            (push (cons (cons 'kind k) e) entries)))))))

(defun gnaw--relation-kind-label (kind)
  "Describe a related report of relation KIND, seen from the origin.
The phrasing follows the BONE JSON: under a report, the `supersedes'
field lists the reports superseding it, `resolved-by' the patches
resolving it, and so on."
  (pcase kind
    ('related-to    "related to it")
    ('supersedes    "supersedes it")
    ('superseded-by "superseded by it")
    ('resolves      "resolved by it")
    ('resolved-by   "resolves it")
    ('duplicates    "duplicated by it")
    ('duplicated-by "duplicates it")
    (_ (and kind (format "%s" kind)))))

(defun gnaw--extract-open-reports (source)
  "Extract published reports from SOURCE as (MID . INFO) pairs.
A report is kept when its status is at least 4, which includes closed
reports (flags R, C, E or S) still present in reports.json."
  (let* ((data (gnaw--read-json source))
         (fv (alist-get 'bone-format data))
         (sname (alist-get 'source data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv gnaw-supported-bone-format))
      (message "gnaw: %s has format %s, min supported is %s"
               source fv gnaw-supported-bone-format))
    (dolist (r reports)
      (let ((mid          (alist-get 'message-id r))
            (status       (alist-get 'status r))
            (type         (alist-get 'type r))
            (acked        (alist-get 'acked r))
            (owned        (alist-get 'owned r))
            (closed       (alist-get 'closed r))
            (owned-name   (alist-get 'owned-name r))
            (close-reason (alist-get 'close-reason r))
            (priority     (alist-get 'priority r))
            (votes        (alist-get 'votes r))
            (deadline     (alist-get 'deadline r))
            (expiry       (alist-get 'expiry r))
            (last-activity (alist-get 'last-activity r))
            (replies      (alist-get 'replies r))
            (topic        (alist-get 'topic r))
            (subject      (alist-get 'subject r))
            (from         (alist-get 'from r))
            (from-name    (alist-get 'from-name r))
            (date         (alist-get 'date r))
            (archived-at  (alist-get 'archived-at r))
            (patches      (alist-get 'patches r))
            (events       (alist-get 'events r))
            (texts        (alist-get 'texts r))
            (awaiting     (alist-get 'awaiting r))
            (related      (gnaw--report-related r))
            (series       (alist-get 'series r))
            (patch-seq    (alist-get 'patch-seq r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-")))))
                (norm-mid (gnaw-normalize-mid mid)))
            (push (cons norm-mid (list :type (or type "bug")
                                       :flags flags
                                       :priority (or priority 0)
                                       :votes votes
                                       :deadline deadline
                                       :expiry expiry
                                       :last-activity last-activity
                                       :replies replies
                                       :topic topic
                                       :subject subject
                                       :from from
                                       :from-name from-name
                                       :date date
                                       :source source
                                       :source-name sname
                                       :archived-at archived-at
                                       :patches patches
                                       :events events
                                       :texts texts
                                       :awaiting awaiting
                                       :related related
                                       :series series
                                       :patch-seq patch-seq
                                       :acked acked
                                       :owned owned
                                       :owned-name owned-name
                                       :closed closed))
                  result)))))
    (nreverse result)))

(defun gnaw-reports ()
  "Collect open report pairs from all sources, tolerating failures."
  (mapcan
   (lambda (source)
     (condition-case err
         (gnaw--extract-open-reports source)
       (error
        (message "gnaw: failed loading source %s: %s"
                 source (error-message-string err))
        nil)))
   (gnaw-sources)))

(defun gnaw-update ()
  "Force-refresh the local cache from remote JSON sources.
Run `gnaw-after-update-hook' when finished."
  (interactive)
  (let ((sources (gnaw-sources))
        (count 0))
    (dolist (source sources)
      (when (gnaw--http-url-p source)
        (message "gnaw: updating cache for %s..." source)
        (condition-case err
            (let ((data (gnaw--fetch-json-from-url source))
                  (cache-file (gnaw--source-to-cache-file source)))
              (gnaw--write-json-to-file data cache-file)
              (setq count (1+ count))
              (message "gnaw: cache updated for %s" source))
          (error
           (message "gnaw: failed updating %s: %s"
                    source (error-message-string err))))))
    (run-hooks 'gnaw-after-update-hook)
    (message "gnaw: cache update finished (%d updated)." count)))

;;; State file (state.edn)

(defun gnaw-read-state ()
  "Read and return the gnaw state alist, or nil."
  (gnaw--read-edn-file (expand-file-name "state.edn" gnaw-config-dir)))

(defun gnaw--read-state-for-update ()
  "Return the state alist for a read-modify-write cycle.
Unlike `gnaw-read-state', signal a `user-error' when state.edn exists
but cannot be parsed: writing a state derived from a misread file
would silently drop every existing mark, including the gnaw CLI's."
  (gnaw--read-edn-map-or-signal
   (expand-file-name "state.edn" gnaw-config-dir)))

(defun gnaw-write-state (state)
  "Write STATE to the state file as UTF-8, one entry per line.
Write to a temporary sibling then rename it, so the gnaw CLI, which
shares the file, never reads a half-written state.edn."
  (let* ((file (expand-file-name "state.edn" gnaw-config-dir))
         (coding-system-for-write 'utf-8))
    (make-directory (file-name-directory file) t)
    (let ((tmp (make-temp-file (concat file ".tmp"))))
      (with-temp-file tmp
        (if (null state)
            (insert "{}\n")
          (insert "{"
                  (mapconcat (lambda (kv)
                               (concat (gnaw--edn-write (car kv)) " "
                                       (gnaw--edn-write (cdr kv))))
                             state "\n ")
                  "}\n")))
      (set-file-modes tmp (or (file-modes file) #o644))
      (rename-file tmp file t))))

;;; Local marks (sticky / dismiss)

(defun gnaw--iso-now ()
  "Return the current time as an ISO-8601 UTC string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun gnaw--author-string (info)
  "Build author string from INFO, or nil when it has no usable field."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (when (equal n "") (setq n nil))
    (when (equal e "") (setq e nil))
    (cond ((and n e) (concat n " <" e ">"))
          (e e)
          (n n))))

(defun gnaw--enrich-entry (existing info)
  "Refresh metadata from INFO in EXISTING state entry."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (let ((v (plist-get info (car pair))))
        (when v
          (setf (alist-get (cdr pair) entry) v))))
    (let ((author (gnaw--author-string info)))
      (when author
        (setf (alist-get :author entry) author)))
    entry))

(defun gnaw--state-put (state mid entry)
  "Set MID to ENTRY in STATE, keeping order."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun gnaw--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun gnaw--alist-dissoc (alist key)
  "Remove KEY from ALIST copy."
  (assq-delete-all key (copy-alist alist)))

(defun gnaw--alist-assoc (alist key value)
  "Set KEY to VALUE in ALIST copy."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defconst gnaw--own-entry-keys '(:sticky :dismiss :subject :type :created :author)
  "State entry keys written by gnaw.el; any other key belongs to the CLI.")

(defun gnaw--entry-removable-p (entry)
  "Non-nil when state ENTRY holds no mark and no key foreign to gnaw.el.
Entries with foreign keys (such as the CLI's `:skip-since') must
survive mark removal."
  (and (null (alist-get :sticky entry))
       (null (alist-get :dismiss entry))
       (cl-every (lambda (kv) (memq (car kv) gnaw--own-entry-keys)) entry)))

(defun gnaw--apply-transition (state action mid info)
  "Apply ACTION (:sticky or :dismiss) for MID in STATE using metadata INFO.
The marks are mutually exclusive: setting one clears the other, and
re-applying the active mark returns to neutral.  Each mark holds the
ISO timestamp at which it was set."
  (let* ((base (gnaw--enrich-entry (cdr (assoc mid state)) info))
         (other (if (eq action :sticky) :dismiss :sticky))
         (new (if (alist-get action base)
                  (gnaw--alist-dissoc base action)
                (gnaw--alist-assoc (gnaw--alist-dissoc base other)
                                   action (gnaw--iso-now)))))
    (if (gnaw--entry-removable-p new)
        (gnaw--state-delete state mid)
      (gnaw--state-put state mid new))))

(defun gnaw-action-on-p (state mid action)
  "Return non-nil if ACTION (:sticky or :dismiss) is set for MID in STATE."
  (let ((entry (cdr (assoc mid state))))
    (pcase action
      (:sticky (and (cdr (assq :sticky entry)) t))
      (:dismiss (and (cdr (assq :dismiss entry)) t)))))

(defun gnaw-toggle-mark (mid info action)
  "Toggle ACTION (:sticky or :dismiss) for MID using metadata INFO.
Persist the new state and return non-nil if ACTION is now on."
  (let* ((state (gnaw--read-state-for-update))
         (new   (gnaw--apply-transition state action mid info)))
    (gnaw-write-state new)
    (gnaw-action-on-p new mid action)))

(defun gnaw-remove-marks (mid)
  "Remove any sticky or dismiss mark for MID, persisting the new state.
Return non-nil if a mark was actually cleared."
  (let* ((state (gnaw--read-state-for-update))
         (entry (cdr (assoc mid state))))
    (when (and entry
               (or (alist-get :sticky entry)
                   (alist-get :dismiss entry)))
      (let ((new (gnaw--alist-dissoc (gnaw--alist-dissoc entry :sticky)
                                     :dismiss)))
        (gnaw-write-state
         (if (gnaw--entry-removable-p new)
             (gnaw--state-delete state mid)
           (gnaw--state-put state mid new))))
      t)))

;;; Presentation helpers shared by the MUA front-ends

(defvar gnaw-annotation-votes-width 7
  "Fixed width of the votes column in `gnaw-annotation'.")

(defvar gnaw-annotation-deadline-width 5
  "Fixed width of the deadline column in `gnaw-annotation'.")

(defvar gnaw-annotation-expiry-width 5
  "Fixed width of the expiry column in `gnaw-annotation'.")

(defun gnaw-type-letter (type)
  "Return the one-letter abbreviation of report TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun gnaw-priority-letter (priority)
  "Return the letter for PRIORITY: A (3), B (2), C (1), space otherwise."
  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))

(defun gnaw-mark-prefix (entry)
  "Return the mark character for state ENTRY: `!' sticky, `d' dismissed."
  (let ((sticky (cdr (assq :sticky entry)))
        (dismiss (cdr (assq :dismiss entry))))
    (cond (sticky  "!")
          (dismiss "d")
          (t       " "))))

(defun gnaw-days-until (date)
  "Days from now until YYYY-MM-DD DATE, or nil when DATE is nil or invalid."
  (when date
    (ignore-errors
      (let* ((d (date-to-time (concat date " 00:00:00")))
             (diff (float-time (time-subtract d (current-time)))))
        (ceiling (/ diff 86400.0))))))

(defun gnaw-annotation (info &optional entry)
  "Build the fixed-width annotation string for report INFO and state ENTRY.
Columns: local mark, type letter, flags, priority letter, deadline
\(D±n days), expiry (E±n days) and votes ([score/total])."
  (let* ((mark      (gnaw-mark-prefix entry))
         (type      (gnaw-type-letter (plist-get info :type)))
         (flags     (plist-get info :flags))
         (priority  (plist-get info :priority))
         (votes     (plist-get info :votes))
         (dl-days   (gnaw-days-until (plist-get info :deadline)))
         (ex-days   (gnaw-days-until (plist-get info :expiry)))
         (pri-str   (gnaw-priority-letter priority))
         (dl-pad    (string-pad (if dl-days (format "D%+d" dl-days) "")
                                gnaw-annotation-deadline-width))
         (ex-pad    (string-pad (if ex-days (format "E%+d" ex-days) "")
                                gnaw-annotation-expiry-width))
         (votes-pad (string-pad (if votes (format "[%s]" votes) "")
                                gnaw-annotation-votes-width)))
    (concat mark " " type " " flags " " pri-str " " dl-pad ex-pad votes-pad)))

(defun gnaw-topics (reports)
  "Return the sorted list of topics in REPORTS ((MID . INFO) pairs).
A report's :topic may hold several space-separated topics."
  (let ((topics nil))
    (dolist (r reports)
      (dolist (topic (split-string (or (plist-get (cdr r) :topic) "")))
        (cl-pushnew topic topics :test #'equal)))
    (sort topics #'string<)))

(defun gnaw-filter-by-topic (reports topic)
  "Return the REPORTS pairs one of whose :topic tokens equals TOPIC."
  (cl-remove-if-not
   (lambda (r) (member topic (split-string (or (plist-get (cdr r) :topic) ""))))
   reports))

;;; Source metadata (meta.json) and attachment files

(defun gnaw--fetch-url-to-file (url file)
  "Fetch URL synchronously and write its raw body bytes to FILE."
  (let ((body (gnaw--http-body url)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert body)))))

(defun gnaw-source-meta (source)
  "Return the parsed meta.json sibling of reports SOURCE, or nil."
  (condition-case nil
      (gnaw--read-json (concat (file-name-directory source) "meta.json"))
    (error nil)))

(defun gnaw--attachments-base (source subdir)
  "Return the SUBDIR base URL or directory for reports SOURCE.
SUBDIR is one of BONE's attachment directories -- \"patches/\",
\"events/\" or \"text/\" -- exported as siblings of reports/."
  (concat (file-name-directory (directory-file-name (file-name-directory source)))
          subdir))

(defun gnaw--attachment-subdir (type)
  "Return the BONE export subdir for attachment TYPE.
TYPE is `patch', `event' or `text'."
  (pcase type ('patch "patches/") ('event "events/") ('text "text/")))

(defun gnaw--sanitize-path-component (s)
  "Return S reduced to a single safe path component, or nil.
Strips any directory part and rejects empty, `.' and `..' names, so
remote reports.json values cannot escape the cache via path traversal."
  (let ((base (and s (file-name-nondirectory s))))
    (and base (not (member base '("" "." ".."))) base)))

(defun gnaw--sanitize-attachment-path (s)
  "Return S when it is a safe relative attachment path, else nil.
An attachment `file' has one or two components: an optional
per-report hash directory, then the file name (BONE's
<subdir>/<mid-hash>/<file> layout).  Reject empty, `.', `..',
absolute or backslashed components, so a remote reports.json
cannot escape the cache."
  (when (and s (not (string-prefix-p "/" s)) (not (string-prefix-p "~" s)))
    (let ((parts (split-string s "/")))
      (and (<= 1 (length parts) 2)
           (cl-every (lambda (p)
                       (and (not (member p '("" "." "..")))
                            (not (string-match-p "\\\\" p))))
                     parts)
           s))))

(defun gnaw-attachment-file (info entry &optional type)
  "Return a local file for ENTRY of report INFO, fetching it if absent.
ENTRY is an element of the report `:patches', `:events' or `:texts'
list; TYPE is `patch' (the default), `event' or `text'."
  (let* ((subdir (gnaw--attachment-subdir (or type 'patch)))
         (file   (gnaw--sanitize-attachment-path (alist-get 'file entry)))
         (source (plist-get info :source))
         (sname  (or (gnaw--sanitize-path-component (plist-get info :source-name))
                     "unknown"))
         (cache  (and file
                      (expand-file-name (concat "cache/" subdir sname "/" file)
                                        gnaw-config-dir))))
    (unless file
      (user-error "Attachment entry has no usable file name"))
    (unless (file-exists-p cache)
      (let ((loc (concat (gnaw--attachments-base source subdir) file)))
        (cond ((gnaw--http-url-p source) (gnaw--fetch-url-to-file loc cache))
              ((file-exists-p loc)
               (make-directory (file-name-directory cache) t)
               (copy-file loc cache t)))))
    (and (file-exists-p cache) cache)))

(defun gnaw-patch-file (info patch)
  "Return a local file for PATCH of report INFO, fetching it if absent.
PATCH is an entry of the report `:patches' list."
  (gnaw-attachment-file info patch 'patch))

(defun gnaw--attachments (info)
  "Return report INFO's attachments as (TYPE . ENTRY) pairs.
TYPE is `patch', `event' or `text'; ENTRY the report list entry."
  (append (mapcar (lambda (e) (cons 'patch e)) (plist-get info :patches))
          (mapcar (lambda (e) (cons 'event e)) (plist-get info :events))
          (mapcar (lambda (e) (cons 'text e)) (plist-get info :texts))))

;;; Reading the report message

(defcustom gnaw-open-message-method '((t . auto))
  "How `gnaw-read-message' opens a report's email, per source.
An alist mapping a source name (the `:name' of a config.edn `:sources'
entry) to a method; the entry keyed by t is the default.  Methods:
`auto' uses `gnaw-open-message-function' if set, else the web archive;
`mua' forces that function; `gnus', `notmuch' and `mu4e' open in that
MUA by message-id (Gnus also reads `gnaw-gnus-group'); `web' forces
the web archive."
  :type '(alist :key-type (choice (const :tag "Default (any source)" t)
                                  (string :tag "Source name"))
                :value-type (choice (const :tag "Auto" auto)
                                    (const :tag "MUA function" mua)
                                    (const :tag "Gnus" gnus)
                                    (const :tag "Notmuch" notmuch)
                                    (const :tag "mu4e" mu4e)
                                    (const :tag "Web archive" web)))
  :group 'gnaw)

(defcustom gnaw-gnus-group nil
  "Alist mapping a source name to the Gnus group holding its mails.
Used by the `gnus' open method; the entry keyed by t is the default.
Without a group, the Gnus registry is tried, then completion."
  :type '(alist :key-type (choice (const :tag "Default (any source)" t)
                                  (string :tag "Source name"))
                :value-type (string :tag "Gnus group"))
  :group 'gnaw)

(defvar gnaw-open-message-function nil
  "Function (MID INFO) a front-end sets to open a message in its MUA.")

(defconst gnaw--open-message-method-choices
  '("auto" "mua" "gnus" "notmuch" "mu4e" "web")
  "Open-message methods offered during interactive configuration.")

(defun gnaw--method-for (info)
  "Return the open method for report INFO's source."
  (let* ((m gnaw-open-message-method)
         (name (plist-get info :source-name))
         (cfg-name (alist-get :name (gnaw--source-config-entry info)))
         (entry (or (and name (assoc name m))
                    (and cfg-name (assoc cfg-name m))
                    (assq t m))))
    (if entry (cdr entry) 'auto)))

(defun gnaw--gnus-group-for (info)
  "Return the `gnaw-gnus-group' entry for report INFO's source, or nil."
  (let* ((name (plist-get info :source-name))
         (cfg-name (alist-get :name (gnaw--source-config-entry info))))
    (or (and name (cdr (assoc name gnaw-gnus-group)))
        (and cfg-name (cdr (assoc cfg-name gnaw-gnus-group)))
        (cdr (assq t gnaw-gnus-group)))))

(defvar-local gnaw--message-archive-url nil
  "Web archive URL of the message shown in the current buffer.")

(defun gnaw--strip-mid (mid)
  "Return message-id MID without surrounding angle brackets."
  (replace-regexp-in-string "\\`<\\|>\\'" "" mid))

(defun gnaw-message-archive-url (mid info)
  "Return the web archive URL for MID using INFO, or nil."
  (or (plist-get info :archived-at)
      (let* ((source (plist-get info :source))
             (meta (and source (gnaw-source-meta source)))
             (fmt (and meta (alist-get 'archive-format-string meta))))
        (and fmt (format fmt (gnaw--strip-mid mid))))))

(defvar gnaw-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'gnaw-message-browse)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `gnaw-message-mode'.")

(define-derived-mode gnaw-message-mode special-mode "Gnaw-Message"
  "Major mode for a fetched BONE report message.")

(defun gnaw-message-browse ()
  "Open the current message's web archive page in a browser."
  (interactive)
  (if gnaw--message-archive-url
      (browse-url gnaw--message-archive-url)
    (user-error "No archive URL for this message")))

(declare-function quoted-printable-decode-string "qp")
(declare-function rfc2047-decode-string "rfc2047")

(defun gnaw--decode-message (raw)
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

(defun gnaw--show-message-web (mid info)
  "Fetch and display the message for MID using INFO, decoded for reading."
  (let ((url (gnaw-message-archive-url mid info)))
    (unless url (error "No web archive URL for %s" mid))
    (let ((raw (gnaw--http-body
                (concat (replace-regexp-in-string "/+\\'" "" url) "/raw"))))
      (with-current-buffer (get-buffer-create "*gnaw-message*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (gnaw--decode-message raw))
          (goto-char (point-min)))
        (gnaw-message-mode)
        (setq gnaw--message-archive-url url)
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

(defun gnaw--read-gnus-group (prompt)
  "Read a Gnus group with PROMPT, completing over all known groups.
Start Gnus first when needed so the group list is populated."
  (require 'gnus)
  (unless (gnus-alive-p) (gnus))
  (gnus-group-completing-read prompt))

(defun gnaw--gnus-return-to (buffer)
  "Switch back to BUFFER once the next Gnus summary is exited."
  (when (buffer-live-p buffer)
    (letrec ((fn (lambda ()
                   (remove-hook 'gnus-summary-exit-hook fn)
                   (run-at-time 0 nil
                                (lambda ()
                                  (when (buffer-live-p buffer)
                                    (switch-to-buffer buffer)))))))
      (add-hook 'gnus-summary-exit-hook fn))))

(defun gnaw--show-message-gnus (mid info)
  "Open MID in Gnus using INFO's source group, the registry, or a prompt.
Enter the group limited to one article, select MID by message-id, and
return to the calling buffer on summary exit."
  (require 'gnus)
  (let ((origin (current-buffer)))
    (unless (gnus-alive-p) (gnus))
    (let* ((id (gnaw-normalize-mid mid))
           (cfg (gnaw--gnus-group-for info))
           (group (or (and cfg (not (string-empty-p cfg)) cfg)
                      (and (bound-and-true-p gnus-registry-enabled)
                           (fboundp 'gnus-registry-get-id-key)
                           (car (gnus-registry-get-id-key id 'group)))
                      (gnaw--read-gnus-group "Gnus group for this message: "))))
      (gnus-activate-group group)
      (gnus-group-read-group 1 t group)
      (gnus-summary-goto-article id nil t)
      (gnaw--gnus-return-to origin))))

(defun gnaw--show-message-notmuch (mid info)
  "Open MID in Notmuch by message-id (INFO is unused)."
  (ignore info)
  (require 'notmuch)
  (notmuch-show (concat "id:" (gnaw--strip-mid mid))))

(defun gnaw--show-message-mu4e (mid info)
  "Open MID in mu4e by message-id (INFO is unused)."
  (ignore info)
  (require 'mu4e)
  (let ((id (gnaw--strip-mid mid)))
    (cond
     ((fboundp 'mu4e-view-message-with-message-id)
      (mu4e-view-message-with-message-id id))
     ((fboundp 'mu4e-search) (mu4e-search (concat "msgid:" id)))
     ((fboundp 'mu4e-headers-search) (mu4e-headers-search (concat "msgid:" id)))
     (t (user-error "Cannot open by message-id in this mu4e version")))))

(declare-function customize-save-variable "cus-edit")

(defun gnaw--alist-put (alist key val)
  "Return ALIST with KEY set to VAL, KEY compared with `equal'."
  (cons (cons key val)
        (assoc-delete-all key (copy-alist (if (listp alist) alist nil)))))

(defun gnaw--read-open-message-method ()
  "Read an open-message method interactively."
  (intern (completing-read
           "Open messages with: "
           gnaw--open-message-method-choices nil t nil nil "auto")))

(defun gnaw--configured-source-names ()
  "Return source names from config.edn without fetching metadata."
  (delete-dups
   (delq nil (mapcar (lambda (s) (alist-get :name s))
                     (plist-get (gnaw-load-config) :source-configs)))))

(defun gnaw--save-source-open-method (source method &optional group)
  "Persist METHOD, and optional Gnus GROUP, for SOURCE via Customize.
SOURCE is a source name string, or t for the default entry."
  (customize-save-variable
   'gnaw-open-message-method
   (gnaw--alist-put gnaw-open-message-method source method))
  (when (and (eq method 'gnus) group (not (string-empty-p group)))
    (customize-save-variable
     'gnaw-gnus-group
     (gnaw--alist-put gnaw-gnus-group source group))))

(defun gnaw--known-source-names ()
  "Return known source names from config.edn and each source's meta.json."
  (delete-dups
   (delq nil (append (gnaw--configured-source-names)
                     (mapcar (lambda (s) (alist-get 'source (gnaw-source-meta s)))
                             (gnaw-sources))))))

(defun gnaw--read-email-client-source ()
  "Read a source name for message opening, or t for the default entry."
  (let ((s (completing-read "Source name (empty = global default): "
                            (gnaw--known-source-names))))
    (if (string-empty-p s) t s)))

;;;###autoload
(defun gnaw-configure-email-client (source method &optional group)
  "Configure how SOURCE messages are opened.
SOURCE is a configured source name, or t for the global default.  METHOD is
one of `auto', `mua', `gnus', `notmuch', `mu4e' or `web'.  When METHOD is
`gnus', GROUP stores the Gnus group to search first."
  (interactive
   (let* ((source (gnaw--read-email-client-source))
          (method (gnaw--read-open-message-method))
          (group (when (eq method 'gnus)
                   (gnaw--read-gnus-group "Gnus group (empty = ask each time): "))))
     (list source method group)))
  (gnaw--save-source-open-method source method group)
  (message "gnaw: %s messages open with %s%s"
           (if (eq source t) "all sources" source)
           method
           (if (and (eq method 'gnus) group (not (string-empty-p group)))
               (format " in %s" group)
             ""))
  method)

(defun gnaw-read-message (mid info)
  "Open the email for MID using INFO per `gnaw-open-message-method'."
  (pcase (gnaw--method-for info)
    ('mua (if gnaw-open-message-function
              (funcall gnaw-open-message-function mid info)
            (user-error "No `gnaw-open-message-function' set")))
    ('gnus (gnaw--show-message-gnus mid info))
    ('notmuch (gnaw--show-message-notmuch mid info))
    ('mu4e (gnaw--show-message-mu4e mid info))
    ('web (gnaw--show-message-web mid info))
    (_ (if gnaw-open-message-function
           (funcall gnaw-open-message-function mid info)
         (gnaw--show-message-web mid info)))))

;;; Viewing and applying patches

(defcustom gnaw-apply-repo nil
  "Fallback git repository for `gnaw-apply-patches'.
Used when the source has no `:repo' in config.edn, before asking."
  :type '(choice (const :tag "Ask each time" nil) directory)
  :group 'gnaw)

(defcustom gnaw-git-apply-options '("--3way")
  "Extra arguments passed to `git apply' by `gnaw-apply-patches'.
The default `--3way' 3-way-merges patches that do not apply cleanly
\(possibly leaving conflict markers) instead of failing outright."
  :type '(repeat string)
  :group 'gnaw)

(defcustom gnaw-git-am-options '("--3way")
  "Extra arguments passed to `git am' by `gnaw-am-patches'.
The default `--3way' 3-way-merges patches that do not apply cleanly
\(possibly leaving conflict markers) instead of failing outright."
  :type '(repeat string)
  :group 'gnaw)

(defcustom gnaw-checkout-base 'ask
  "Whether to check out a patch's recorded base commit before applying.
When a patch carries a `base-commit:' trailer (`git format-patch
--base') that exists in the target repo, gnaw can check it out first.
Values: `ask' prompts, nil never, t always."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Never" nil)
                 (const :tag "Always" t))
  :group 'gnaw)

(defun gnaw--series-complete-p (info)
  "Return non-nil unless INFO has an explicitly incomplete `:series'."
  (let ((series (plist-get info :series)))
    (or (null series) (alist-get 'complete series))))

(defun gnaw--series-id (info)
  "Return the patch-series id of report INFO, or nil."
  (alist-get 'id (plist-get info :series)))

(defun gnaw--patch-seq-n (info)
  "Leading integer of INFO's `:patch-seq' (\"2/5\" -> 2), or 0."
  (let ((s (plist-get info :patch-seq)))
    (if (and s (string-match "\\`\\([0-9]+\\)" s))
        (string-to-number (match-string 1 s))
      0)))

(defun gnaw--cover-p (info)
  "Non-nil if report INFO is a series cover letter (patch-seq \"0/...\")."
  (string-prefix-p "0/" (or (plist-get info :patch-seq) "")))

(defun gnaw--closed-p (info)
  "Non-nil if report INFO carries a close flag (R, C, E or S)."
  (let ((f (or (plist-get info :flags) "---")))
    (and (>= (length f) 3) (not (eq (aref f 2) ?-)))))

(defun gnaw--series-summary (members)
  "Summarize series MEMBERS (INFO plists) like \"2 acked, 1 open\".
Cover letters are excluded from the tally."
  (let ((acked 0) (closed 0) (open 0))
    (dolist (m members)
      (unless (gnaw--cover-p m)
        (let ((f (or (plist-get m :flags) "---")))
          (cond ((gnaw--closed-p m) (cl-incf closed))
                ((and (>= (length f) 1) (not (eq (aref f 0) ?-))) (cl-incf acked))
                (t (cl-incf open))))))
    (string-join (delq nil (list (and (> acked 0) (format "%d acked" acked))
                                 (and (> closed 0) (format "%d closed" closed))
                                 (and (> open 0) (format "%d open" open))))
                 ", ")))

(defun gnaw--patch-files (info verb &optional patches)
  "Return the local files of INFO's patches, fetching them as needed.
PATCHES restricts the operation to these `:patches' entries.
Signal a `user-error' when INFO has no patches or some cannot be
fetched; VERB names the aborted operation in the error message."
  (let* ((patches (or patches (plist-get info :patches)
                      (user-error "This report has no patches")))
         (files (mapcar (lambda (p) (gnaw-patch-file info p)) patches)))
    (when (memq nil files)
      (user-error "Cannot fetch %d of %d patch file(s); not %s"
                  (seq-count #'null files) (length files) verb))
    files))

(defun gnaw-view-patches (info &optional patches)
  "Show the patches of report INFO in a `diff-mode' buffer.
PATCHES restricts the display to these `:patches' entries."
  (let ((files (gnaw--patch-files info "viewing" patches)))
    (with-current-buffer (get-buffer-create "*gnaw-patches*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (f files)
          (insert-file-contents f)
          (goto-char (point-max)))
        (goto-char (point-min)))
      (diff-mode)
      (pop-to-buffer (current-buffer)))))

(defun gnaw--patch-base-commit (files)
  "Return the first `base-commit:' trailer found in patch FILES, or nil."
  (with-temp-buffer
    (catch 'found
      (dolist (f files)
        (when (and f (file-readable-p f))
          (erase-buffer)
          (insert-file-contents f)
          (goto-char (point-min))
          (when (re-search-forward "^base-commit: \\([0-9a-f]\\{7,40\\}\\)$" nil t)
            (throw 'found (match-string 1))))))))

(defun gnaw--maybe-checkout-base (files)
  "Offer to check out the base commit recorded in patch FILES.
Runs in `default-directory' (the target repo) per `gnaw-checkout-base',
when the commit exists locally.  Leaves the repo on a detached HEAD;
signals a `user-error' if the checkout fails."
  (when gnaw-checkout-base
    (let ((base (gnaw--patch-base-commit files)))
      (when (and base
                 (zerop (call-process "git" nil nil nil
                                      "cat-file" "-e" (concat base "^{commit}")))
                 (or (eq gnaw-checkout-base t)
                     (y-or-n-p
                      (format "Check out base commit %s (detached HEAD) first? "
                              (substring base 0 (min 12 (length base)))))))
        ;; An existing *gnaw-git* buffer keeps the default-directory of
        ;; its first use; re-point it at the current repo.
        (let ((dir default-directory))
          (with-current-buffer (get-buffer-create "*gnaw-git*")
            (setq default-directory dir)
            (let ((inhibit-read-only t)) (erase-buffer))
            (unless (zerop (call-process "git" nil t nil "checkout" base))
              (display-buffer (current-buffer))
              (user-error "Git checkout %s failed" base))))))))

(defun gnaw--run-git-patches (info subcommand options hint &optional patches)
  "Apply INFO's patches in its repo via git SUBCOMMAND (\"apply\" or \"am\").
OPTIONS is a list of extra arguments inserted before the patch files.
HINT is shown on failure.  PATCHES restricts the operation to these
`:patches' entries."
  (let ((files (gnaw--patch-files info "applying" patches)))
    ;; PATCHES is a deliberate subset: no incomplete-series warning then.
    (when (and (not patches)
               (not (gnaw--series-complete-p info))
               (not (yes-or-no-p "Patch series looks incomplete; apply anyway? ")))
      (user-error "Aborted"))
    (let* ((repo (or (gnaw--source-repo info)
                     gnaw-apply-repo
                     (read-directory-name "Apply in git repo: ")))
           (default-directory (file-name-as-directory repo)))
      (gnaw--maybe-checkout-base files)
      (let ((args (append (list subcommand) options (list "--") files)))
        (with-current-buffer (get-buffer-create "*gnaw-git*")
          ;; An existing buffer keeps the default-directory of its first
          ;; use; re-point it at the current repo.
          (setq default-directory (file-name-as-directory repo))
          (let ((inhibit-read-only t)) (erase-buffer))
          (let ((status (apply #'call-process "git" nil t nil args)))
            (if (zerop status)
                (message "gnaw: git %s applied %d patch(es) in %s"
                         subcommand (length files) repo)
              (display-buffer (current-buffer))
              (message "gnaw: git %s failed in %s (%s)" subcommand repo hint))))))))

(defun gnaw-apply-patches (info &optional patches)
  "Apply INFO's patches to the working tree with `git apply'.
PATCHES restricts the operation to these `:patches' entries."
  (gnaw--run-git-patches info "apply" gnaw-git-apply-options
                         "rejects left as .rej" patches))

(defun gnaw-am-patches (info &optional patches)
  "Apply INFO's patches as commits with `git am'.
PATCHES restricts the operation to these `:patches' entries."
  (gnaw--run-git-patches info "am" gnaw-git-am-options
                         "run `git am --abort' to undo" patches))

(defun gnaw-save-patches (info &optional no-confirm patches)
  "Save INFO's patch files to a directory.
Prompt for the target directory, proposing the source's `:repo'
\(or `gnaw-apply-repo') when one is configured.  A single patch is
saved to the configured repo without prompting; so are several when
NO-CONFIRM is non-nil, which also overwrites existing files
silently.  PATCHES restricts the operation to these `:patches'
entries."
  (let* ((files (gnaw--patch-files info "saving" patches))
         (repo (or (gnaw--source-repo info) gnaw-apply-repo))
         (dir (if (and repo (or no-confirm (null (cdr files))))
                  (file-name-as-directory repo)
                (read-directory-name "Save patch(es) in: " repo))))
    (dolist (f files)
      (copy-file f (expand-file-name (file-name-nondirectory f) dir)
                 (if no-confirm t 1)))
    (message "gnaw: saved %d patch(es) in %s" (length files) dir)))

;;; Query filter (subset of the BONE web search syntax)

(defvar gnaw-list--query nil
  "Active `gnaw-list' filter query string, or nil.  Buffer-local in use.")

(defvar gnaw-list--expanded nil
  "Series ids unfolded in the current `gnaw-list' buffer.  Buffer-local.")

(defvar gnaw-list--show-dismissed nil
  "When non-nil, show reports marked dismissed.  Buffer-local in `gnaw-list'.")

(defvar gnaw-list--related-mids nil
  "Message-ids the list is narrowed to, the origin report first, or nil.
Set by `gnaw-list-related-narrow'.  Buffer-local in use.")

(defvar gnaw-list--related-entries nil
  "Alist of (MID . RELATION-ENTRY) for the related-reports view.
RELATION-ENTRY is the relation alist exported by BONE (type,
subject, archived-at...), used to build placeholder rows for
related reports absent from the loaded sources.  Set by
`gnaw-list-related-narrow'.  Buffer-local in use.")

(defvar-local gnaw-list--flagged nil
  "Message-ids flagged for dismissal (d), executed by x.")

(defvar-local gnaw-list--below-mid nil
  "Message-id of the mail currently shown below the list, or nil.")

(defvar gnaw-list--reports nil
  "Cached (MID . INFO) pairs for the current `gnaw-list' buffer.
Set by `gnaw-list-reload'.  Buffer-local in use.")

(defvar gnaw-list--mark-index nil
  "Index of the Mark column among the active columns, nil when hidden.
Set by `gnaw--list-format'.  Buffer-local in use.")

(defface gnaw-sticky '((t :weight bold))
  "Face for sticky reports in the report list."
  :group 'gnaw)

(defface gnaw-dismissed '((t :slant italic))
  "Face for dismissed reports when they are shown in the report list."
  :group 'gnaw)

(defface gnaw-closed '((t :slant italic))
  "Face for closed reports in the related-reports view."
  :group 'gnaw)

(defface gnaw-missing '((t :inherit shadow :slant italic))
  "Face for related reports absent from the loaded sources.
Their placeholder rows are built from the relation metadata only."
  :group 'gnaw)

(defun gnaw--query-delimited (needle delim)
  "Return NEEDLE's content when delimited by character DELIM, else nil.
NEEDLE is delimited when it starts and ends with DELIM and holds at
least the two delimiters."
  (and needle (>= (length needle) 2)
       (eq (aref needle 0) delim)
       (eq (aref needle (1- (length needle))) delim)
       (substring needle 1 -1)))

(defun gnaw--query-regexp (needle)
  "Return the regexp of a slash-delimited query NEEDLE, or nil.
NEEDLE is slash-delimited when it starts and ends with a `/', as
in /^\\[PATCH/."
  (gnaw--query-delimited needle ?/))

(defun gnaw--query-quoted (needle)
  "Return the text of a double-quoted query NEEDLE, or nil.
Quotes make NEEDLE literal: spaces and the operator characters
lose their meaning inside."
  (gnaw--query-delimited needle ?\"))

(defun gnaw--query-text-matcher (needle)
  "Compile query NEEDLE into a predicate on a text field.
The predicate takes the field string (or nil) and is non-nil when
it contains NEEDLE, case-insensitively.  NEEDLE `*' matches any
non-empty field; a slash-delimited NEEDLE (/regexp/) is matched as
an Emacs regexp instead of a literal substring; a double-quoted
NEEDLE is matched literally, `*' and slashes included.  An empty
NEEDLE, even quoted or slashed, matches nothing, and so does an
invalid regexp."
  (let* ((lit (gnaw--query-quoted needle))
         (re (and (not lit) (gnaw--query-regexp needle))))
    (cond
     ((equal needle "*")
      (lambda (hay) (and hay (not (string-empty-p hay)))))
     ((string-empty-p (or lit re needle)) #'ignore)
     ((and re (not (ignore-errors (string-match-p re "") t)))
      #'ignore)                         ; invalid regexp
     (t (let ((rx (or re (regexp-quote (or lit needle)))))
          (lambda (hay)
            (let ((case-fold-search t))
              (string-match-p rx (or hay "")))))))))

(defun gnaw--query-subject-matcher (needle)
  "Compile NEEDLE into a predicate on (MID INFO) searching the subject.
The whole NEEDLE is matched as `gnaw--query-text-matcher' does,
commas included: this is the bare-word (and unknown-key) search."
  (let ((m (gnaw--query-text-matcher needle)))
    (lambda (_mid info) (funcall m (plist-get info :subject)))))

(defun gnaw--query-ymd->days (s)
  "Absolute day number for the YYYY-MM-DD prefix of S, or nil."
  (when (and s (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" s))
    (time-to-days (encode-time 0 0 0
                               (string-to-number (match-string 3 s))
                               (string-to-number (match-string 2 s))
                               (string-to-number (match-string 1 s))))))

(defun gnaw--query-duration->days (s)
  "Number of days for a duration S like 3d, 2w or 2m, or nil."
  (when (and s (string-match "\\`\\([0-9]+\\)\\([dwm]\\)\\'" s))
    (* (string-to-number (match-string 1 s))
       (pcase (match-string 2 s) ("d" 1) ("w" 7) ("m" 30)))))

(defun gnaw--query-bound (s forward today)
  "Day number for a range bound S (YYYY-MM-DD or duration), or nil.
A duration is TODAY plus or minus its days, per FORWARD."
  (or (gnaw--query-ymd->days s)
      (let ((d (gnaw--query-duration->days s)))
        (and d (if forward (+ today d) (- today d))))))

(defun gnaw--query-date-matcher (spec key forward)
  "Compile date SPEC into a predicate on INFO's KEY date field.
SPEC is a duration (3d/2w/2m), a YYYY-MM-DD date, or an A..B range
whose ends are dates or durations and may be empty (the order is
normalized); FORWARD durations look ahead from today.  The bounds
are resolved once, here; an unparseable SPEC matches nothing."
  (let ((today (time-to-days (current-time)))
        (lo nil) (hi nil) (none nil))
    (if (string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" spec)
        ;; Read both ends before calling `gnaw--query-bound', which
        ;; clobbers the match data via its own `string-match'.
        (let* ((sa (match-string 1 spec))
               (sb (match-string 2 spec))
               (va (gnaw--query-bound sa forward today))
               (vb (gnaw--query-bound sb forward today)))
          (setq lo (if (and va vb) (min va vb) va)
                hi (if (and va vb) (max va vb) vb)))
      (let ((d (gnaw--query-duration->days spec))
            (single (gnaw--query-ymd->days spec)))
        (cond (d (if forward
                     (setq lo today hi (+ today d))
                   (setq lo (- today d) hi today)))
              (single (setq lo single hi single))
              (t (setq none t)))))
    (if none
        #'ignore
      (lambda (_mid info)
        (let ((fd (gnaw--query-ymd->days (plist-get info key))))
          (and fd
               (or (null lo) (>= fd lo))
               (or (null hi) (<= fd hi))))))))

(defun gnaw--query-actor-matcher (needle)
  "Compile query NEEDLE into a predicate on an actor (identity) string.
`*' or `true' matches any set actor; otherwise NEEDLE matches as
`gnaw--query-text-matcher' does, so /regexp/ and \"quoted\" values
work here too -- but never against an unset actor."
  (if (member needle '("*" "true"))
      (lambda (a) (and a (not (string-empty-p a))))
    (let ((m (gnaw--query-text-matcher needle)))
      (lambda (a)
        (and a (not (string-empty-p a)) (funcall m a))))))

(defun gnaw--query-flag-matcher (val bit)
  "Compile VAL into a predicate testing priority BIT on INFO.
`*' and `true' require the flag, `false' its absence; any other VAL
matches nothing."
  (let ((want (and (member val '("*" "true")) t)))
    (if (and (not want) (not (equal val "false")))
        #'ignore
      (lambda (_mid info)
        (eq want (= (logand (or (plist-get info :priority) 0) bit) bit))))))

(defvar gnaw--subject-words-cache (make-hash-table :test 'equal)
  "Memoized `gnaw--subject-words' results, keyed by subject.
Tokenizing every subject anew dominates the cost of a similar:
filter pass; the words only change when the reports do, so
`gnaw-list-reload' resets the table and re-warms it when idle.")

(defvar gnaw--subject-words-timer nil
  "Idle timer re-warming `gnaw--subject-words-cache', or nil.")

(defun gnaw--subject-words (subject)
  "Return the significant words of SUBJECT for similarity matching.
Downcased words of four letters or more, without duplicates;
bracketed tags like [PATCH v2 1/3] are dropped first.  A hyphenated
name such as org-element--cache counts as one word, which singles
out such identifiers instead of scattering them into fragments.
The result is memoized in `gnaw--subject-words-cache'."
  (let ((cached (gethash subject gnaw--subject-words-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (puthash
       subject
       (let ((s (replace-regexp-in-string "\\[[^]]*\\]" " "
                                          (downcase (or subject "")))))
         (seq-filter (lambda (w) (>= (length w) 4))
                     (delete-dups
                      (mapcar (lambda (w) (string-trim w "-+" "-+"))
                              (split-string s "[^[:alnum:]-]+" t)))))
       gnaw--subject-words-cache))))

(defun gnaw--query-similar-matcher (val)
  "Compile similar: VAL (words joined by `+') into a subject predicate.
A subject matches when it shares at least three of VAL's words --
all of them when VAL has fewer than three (see `gnaw--subject-words'
for what counts as a word)."
  (let* ((words (mapcar #'downcase (split-string (or val "") "\\+" t)))
         (need (min 3 (length words))))
    (lambda (subject)
      (let ((mine (gnaw--subject-words subject)))
        (and (> need 0)
             (>= (seq-count (lambda (w) (member w mine)) words) need))))))

(defun gnaw--query-vals (val)
  "Split VAL into its comma-separated (OR) alternatives.
A slash-delimited (/regexp/) or double-quoted VAL stays whole: each
is always the whole value, and may thus contain commas."
  (if (or (gnaw--query-regexp val) (gnaw--query-quoted val))
      (list val)
    (split-string val "," t)))

(defun gnaw--query-getter (key)
  "Return a (MID INFO) accessor for the INFO plist KEY."
  (lambda (_mid info) (plist-get info key)))

(defun gnaw--query-field-matcher (val make-matcher &rest getters)
  "Compile VAL into a predicate on the fields read by GETTERS.
MAKE-MATCHER turns each comma (OR) alternative of VAL into a
predicate on one string; each of GETTERS takes (MID INFO) and
returns a field the alternatives are tried against."
  (let ((ms (mapcar make-matcher (gnaw--query-vals val))))
    (lambda (mid info)
      (seq-some (lambda (m)
                  (seq-some (lambda (g) (funcall m (funcall g mid info)))
                            getters))
                ms))))

(defun gnaw--query-glyph-matcher (val glyphs)
  "Compile VAL into a predicate on a glyph string returned by GLYPHS.
GLYPHS maps an INFO plist to the string; every character of a VAL
alternative must be present in it (a comma is OR, as elsewhere)."
  (let ((alts (seq-remove #'string-empty-p (gnaw--query-vals val))))
    (lambda (_mid info)
      (let ((s (funcall glyphs info)))
        (seq-some (lambda (v)
                    (seq-every-p (lambda (ch) (seq-contains-p s ch)) v))
                  alts)))))

(defun gnaw--query-compile-token (token)
  "Compile search TOKEN into a predicate taking (MID INFO).
A token starting with `-' matches when the rest of the token does
not: -type:patch, -acked:*, -subject:/^re:/ or -latex.  The
negation covers the whole token, comma (OR) alternatives included."
  (cond ((and (> (length token) 1) (eq (aref token 0) ?-))
         (let ((inner (gnaw--query-compile-token (substring token 1))))
           (lambda (mid info) (not (funcall inner mid info)))))
        ;; A fully quoted token is a literal subject search: a colon
        ;; inside the quotes is not a key separator.
        ((gnaw--query-quoted token)
         (gnaw--query-subject-matcher token))
        (t (gnaw--query-compile-key token))))

(defun gnaw--query-compile-key (token)
  "Compile unquoted, unnegated TOKEN into a predicate taking (MID INFO).
A known key with an empty value matches nothing: each branch below
then compiles a predicate with no alternative left to satisfy.  A
token without a colon, or whose prefix is no known key, searches
the whole token in the subjects."
  (let* ((i (string-search ":" token))
         (key (and i (substring token 0 i)))
         (val (and i (substring token (1+ i)))))
    (pcase key
      ((or "from" "f")
       (gnaw--query-field-matcher val #'gnaw--query-text-matcher
                                  (gnaw--query-getter :from)
                                  (gnaw--query-getter :from-name)))
      ((or "subject" "s")
       (gnaw--query-field-matcher val #'gnaw--query-text-matcher
                                  (gnaw--query-getter :subject)))
      ((or "topic" "t" "T")
       (gnaw--query-field-matcher val #'gnaw--query-text-matcher
                                  (gnaw--query-getter :topic)))
      ((or "mid" "m")
       (gnaw--query-field-matcher val #'gnaw--query-text-matcher
                                  (lambda (mid _info) mid)))
      ((or "acked" "a")
       (gnaw--query-field-matcher val #'gnaw--query-actor-matcher
                                  (gnaw--query-getter :acked)))
      ((or "owned" "o")
       (gnaw--query-field-matcher val #'gnaw--query-actor-matcher
                                  (gnaw--query-getter :owned)
                                  (gnaw--query-getter :owned-name)))
      ((or "closed" "c")
       (gnaw--query-field-matcher val #'gnaw--query-actor-matcher
                                  (gnaw--query-getter :closed)))
      ("type"
       ;; A closed set compared whole: no *, regexp or quotes here,
       ;; and type:* matches nothing.
       (let ((vals (mapcar #'downcase (gnaw--query-vals val))))
         (lambda (_mid info)
           (member (downcase (or (plist-get info :type) "")) vals))))
      ((or "priority" "p")
       (let ((vals (gnaw--query-vals val)))
         (lambda (_mid info)
           (member (number-to-string (or (plist-get info :priority) 0))
                   vals))))
      ((or "urgent" "u") (gnaw--query-flag-matcher val 2))
      ((or "important" "i") (gnaw--query-flag-matcher val 1))
      ;; Glyph matches, mirroring the Flags and Att columns:
      ;; flags:AO = acked and owned; flags:S = superseded;
      ;; att:~+ = related and a single patch.
      ((or "flags" "F")
       (gnaw--query-glyph-matcher
        (upcase val) (lambda (info) (or (plist-get info :flags) ""))))
      ((or "att" "attributes" "A")
       ;; downcase: x is the only cased glyph, and the neighbor
       ;; flags: alphabet is uppercase.
       (gnaw--query-glyph-matcher (downcase val) #'gnaw--att-string))
      ("similar"
       (let ((m (gnaw--query-similar-matcher val)))
         (lambda (_mid info) (funcall m (plist-get info :subject)))))
      ((or "date" "d") (gnaw--query-date-matcher val :date nil))
      ((or "deadline" "D") (gnaw--query-date-matcher val :deadline t))
      ((or "expired" "e") (gnaw--query-date-matcher val :expiry t))
      (_ (gnaw--query-subject-matcher token)))))

(defun gnaw--query-parse (query)
  "Parse QUERY into OR-groups of AND-token lists (`|' = OR, space = AND).
Double quotes keep a token together: spaces and `|' between them
split nothing, so subject:\"a b\" stays one token.  An unbalanced
quote swallows the rest of QUERY into the current token."
  (let (groups tokens tok in-quote)
    (cl-flet ((end-token ()
                (when tok
                  (push (concat (nreverse tok)) tokens)
                  (setq tok nil)))
              (end-group ()
                (when tokens
                  (push (nreverse tokens) groups)
                  (setq tokens nil))))
      (dolist (ch (append query nil))
        (cond
         ((eq ch ?\")
          (setq in-quote (not in-quote))
          (push ch tok))
         ((and (not in-quote) (memq ch '(?\s ?\t)))
          (end-token))
         ((and (not in-quote) (eq ch ?|))
          (end-token)
          (end-group))
         (t (push ch tok))))
      (end-token)
      (end-group))
    (nreverse groups)))

(defun gnaw--query-compile (query)
  "Compile QUERY into OR-groups of AND predicate lists.
Each predicate takes (MID INFO); tokens are dissected once, here,
instead of once per report and per refresh."
  (mapcar (lambda (toks) (mapcar #'gnaw--query-compile-token toks))
          (gnaw--query-parse query)))

(defun gnaw--query-match-p (groups mid info)
  "Non-nil if report MID/INFO matches compiled GROUPS.
GROUPS comes from `gnaw--query-compile'."
  (seq-some (lambda (preds)
              (seq-every-p (lambda (p) (funcall p mid info)) preds))
            groups))

(defconst gnaw--query-keys
  '("from:" "subject:" "similar:" "topic:" "type:" "priority:" "mid:"
    "acked:" "owned:" "closed:" "urgent:" "important:" "flags:" "att:"
    "date:" "deadline:" "expired:")
  "Long-form query keys completed in `gnaw-list-filter'.")

(defconst gnaw-report-types
  '("bug" "patch" "request" "announcement" "change" "release")
  "BONE report types, offered when filtering by type.")

(defun gnaw--filter-value-candidates (key)
  "Return completion candidates for the value of filter KEY, or nil.
Topics come from the reports of the list buffer the minibuffer was
entered from."
  (pcase key
    ("type" gnaw-report-types)
    ((or "topic" "t")
     (gnaw-topics (buffer-local-value
                   'gnaw-list--reports
                   (window-buffer (minibuffer-selected-window)))))))

(defun gnaw--filter-token-start (string)
  "Return the position where STRING's trailing query token starts.
Like the tokenizer of `gnaw--query-parse', ignore separators inside
double quotes, so typing a quoted phrase is not completed as if the
text after a space started a new token."
  (let ((start 0) (in-quote nil))
    (dotimes (i (length string))
      (let ((ch (aref string i)))
        (cond ((eq ch ?\") (setq in-quote (not in-quote)))
              ((and (not in-quote) (memq ch '(?\s ?\t ?|)))
               (setq start (1+ i))))))
    start))

(defun gnaw--filter-completion (string pred action)
  "Completion table for the last token of filter query STRING.
Complete the query key and, after keys like `type:' or `topic:', its
value (starting after the last comma, commas being OR).  PRED and
ACTION are the usual completion-table arguments; earlier tokens are
left untouched."
  (let* ((beg (gnaw--filter-token-start string))
         ;; Step over a leading negation so -ty completes to -type:.
         (beg (if (and (< beg (length string)) (eq (aref string beg) ?-))
                  (1+ beg)
                beg))
         (tok (substring string beg))
         (colon (string-search ":" tok))
         (cands (if colon
                    (gnaw--filter-value-candidates (substring tok 0 colon))
                  gnaw--query-keys))
         (vbeg (if colon
                   (+ beg colon 1
                      (let ((val (substring tok (1+ colon))))
                        (if (string-match ".*," val) (match-end 0) 0)))
                 beg)))
    (cond
     ((eq (car-safe action) 'boundaries)
      (let ((suffix (cdr action)))
        `(boundaries ,vbeg . ,(or (string-match "[ \t|,]" suffix)
                                  (length suffix)))))
     ((null cands) nil)
     (t
      (let ((res (complete-with-action action cands
                                       (substring string vbeg) pred)))
        ;; `try-completion' (ACTION nil) must return the whole string,
        ;; prefix included; the other actions use the last token alone.
        (if (and (null action) (stringp res))
            (concat (substring string 0 vbeg) res)
          res))))))

(defun gnaw--query-quote-val (val)
  "Return VAL protected for use as a literal query value.
Quote VAL when it contains a character the tokenizer or the value
splitting would interpret (whitespace, `|', a comma), or when it
would read as a /regexp/ or \"quoted\" value.  The syntax cannot
escape a double quote: a VAL containing one is emitted as a
whole-value regexp with `.' standing for each character quotes
cannot carry."
  (cond ((string-match-p "\"" val)
         (concat "/"
                 (replace-regexp-in-string "[\"| \t]" "."
                                           (regexp-quote val))
                 "/"))
        ((or (string-match-p "[ \t|,]" val)
             (gnaw--query-regexp val)
             (gnaw--query-quoted val))
         (concat "\"" val "\""))
        (t val)))

(defun gnaw-list--query-add (query add)
  "Return QUERY, appended to the active query when ADD is non-nil.
Appending uses the space (AND) operator of the query syntax."
  (if (and add gnaw-list--query)
      (concat gnaw-list--query " " query)
    query))

(defun gnaw-list-filter-by (key &optional add)
  "Limit the list to reports whose KEY field matches a read value.
Read the value (completing types and topics, `*' for flag fields),
then set the query to `KEY:value'; an empty value clears the filter.
With ADD non-nil, add the condition to the active filter (AND)
instead of replacing it."
  (let* ((flag (member key '("acked" "owned" "closed" "urgent" "important")))
         (val (cond (flag "*")
                    ((equal key "type") (completing-read "Type: " gnaw-report-types))
                    ((equal key "topic")
                     (completing-read "Topic: " (gnaw-topics gnaw-list--reports)))
                    ((equal key "flags")
                     (read-string "Flags letters, all required (A O C R E S): "))
                    ((equal key "att")
                     (read-string "Att glyphs, all required (. ~ + x @ #): "))
                    (t (read-string (format "%s: " key))))))
    (setq-local gnaw-list--related-mids nil) ; filtering leaves the related view
    (setq-local gnaw-list--query
                (and val (not (string-empty-p val))
                     (gnaw-list--query-add
                      (format "%s:%s" key (gnaw--query-quote-val val)) add)))
    (gnaw-list-refresh)
    (if gnaw-list--query
        (message "gnaw: filter %s" gnaw-list--query)
      (message "gnaw: filter cleared"))))

(defmacro gnaw--define-filter-commands (&rest fields)
  "Define a `gnaw-list-filter-FIELD' command for each of FIELDS."
  `(progn
     ,@(mapcar (lambda (f)
                 `(defun ,(intern (concat "gnaw-list-filter-" f)) (&optional add)
                    ,(concat "Filter the report list by the " f " field.\n"
                             "With a prefix argument ADD, add the condition to\n"
                             "the active filter (AND) instead of replacing it.")
                    (interactive "P")
                    (gnaw-list-filter-by ,f add)))
               fields)))

(gnaw--define-filter-commands
 "from" "subject" "topic" "priority" "mid"
 "date" "deadline" "expired"
 "acked" "owned" "closed" "urgent" "important"
 "flags" "att")

(transient-define-prefix gnaw-list-filter-transient ()
  "Filter the report list by one field."
  [["Field"
    ("f" "From"       gnaw-list-filter-from)
    ("s" "Subject"    gnaw-list-filter-subject)
    ("t" "Type"       gnaw-list-limit-type)
    ("T" "Topic"      gnaw-list-filter-topic)
    ("p" "Priority"   gnaw-list-filter-priority)
    ("m" "Message-id" gnaw-list-filter-mid)]
   ["Date"
    ("d" "Created"    gnaw-list-filter-date)
    ("D" "Deadline"   gnaw-list-filter-deadline)
    ("e" "Expiring"   gnaw-list-filter-expired)]
   ["Flag is set"
    ("a" "Acked"      gnaw-list-filter-acked)
    ("o" "Owned"      gnaw-list-filter-owned)
    ("c" "Closed"     gnaw-list-filter-closed)
    ("u" "Urgent"     gnaw-list-filter-urgent)
    ("i" "Important"  gnaw-list-filter-important)]
   ["Glyphs"
    ("F" "Flags letters" gnaw-list-filter-flags)
    ("A" "Att glyphs"    gnaw-list-filter-att)]])

;;; Report browser (gnaw-list)

(defcustom gnaw-list-columns
  '(("Mark"      5 gnaw--mark-sort :mark)
    ("Type"      8 t :type)
    ("Pri"       4 gnaw--priority-sort :priority)
    ("Flags"     5 t :flags)
    ("Att"       4 t :att)
    ("Msgs"      5 gnaw--msgs-sort :msgs)
    ("From"     18 t :from-name)
    ("Subject"  50 gnaw--subject-sort :subject)
    ("Created"  11 t :date))
  "Columns for `gnaw-list-mode' as (HEADER WIDTH SORT KEY) tuples.
SORT is t (sort on the printed string), nil, or a predicate function;
KEY is the INFO key (or :mark / :att / :msgs) the cell displays.  The Subject
width is recomputed to fill the window by `gnaw--list-format', so it
flexes while the trailing Created column stays pinned to the right edge.

The default follows a left-to-right priority order: the mark you act on,
the high-signal short codes (priority, flags, type), then identity, then
the flexible subject, then the creation date as the rightmost time
anchor.  The Votes, Activity (last activity) and Topic columns are left
out by default to reduce noise; re-add their tuples here to show them —
  (\"Votes\"    5 gnaw--votes-sort :votes)
  (\"Activity\" 11 t :last-activity)
  (\"Topic\"   16 t :topic)
which also re-enables their `gnaw-sort-by-*' commands."
  :type '(repeat (list (string   :tag "Header")
                       (integer  :tag "Width")
                       (choice   :tag "Sort"
                                 (const :tag "By string" t)
                                 (const :tag "None" nil)
                                 (function :tag "Predicate"))
                       (symbol   :tag "Field key")))
  :group 'gnaw)

(defcustom gnaw-list-sort-key '("Created" . t)
  "Default sort key for `gnaw-list-mode', nil for natural order.
A cons (COLUMN . FLIP): a header name from `gnaw-list-columns', and
whether to sort in descending order.  The default shows the most
recently created reports first."
  :type '(choice (const :tag "Natural order" nil)
                 (cons (string  :tag "Column header")
                       (boolean :tag "Descending")))
  :group 'gnaw)

(defcustom gnaw-preset-filters nil
  "Predefined filter queries for `gnaw-select-preset-filter'.
A list of query strings in the syntax of `gnaw-list-filter' (for
example \"type:patch\" or \"priority:3 urgent:\").  When nil, no presets
are offered."
  :type '(repeat string)
  :group 'gnaw)

(defun gnaw--flags-help (info)
  "Spell out INFO's flags for the Flags column help echo, or nil."
  (let ((f (or (plist-get info :flags) "")))
    (when-let* ((parts (delq nil
                             (list (and (> (length f) 0) (eq (aref f 0) ?A)
                                        "acked")
                                   (and (> (length f) 1) (eq (aref f 1) ?O)
                                        "owned")
                                   (and (> (length f) 2)
                                        ;; Keep in sync with `gnaw--closed-p':
                                        ;; any non-dash means closed.
                                        (pcase (aref f 2)
                                          (?C "canceled") (?R "resolved")
                                          (?E "expired") (?S "superseded")
                                          (?- nil) (_ "closed")))))))
      (string-join parts ", "))))

(defun gnaw--att-string (info)
  "Return the three-position Att column string for report INFO.
Positions: awaiting (.), related (~), then one attachment glyph --
+ one patch, x several, @ calendar events, # plain-text files.
Also matched, sans spaces, by the att: query key."
  (let ((patches (plist-get info :patches)))
    (concat (if (plist-get info :awaiting) "." " ")
            (if (plist-get info :related) "~" " ")
            (cond ((cdr patches)            "x")
                  (patches                  "+")
                  ((plist-get info :events) "@")
                  ((plist-get info :texts)  "#")
                  (t " ")))))

(defun gnaw--att-help (info)
  "Spell out INFO's Att column for its help echo, or nil."
  (cl-flet ((tally (key one many)
              (let ((n (length (plist-get info key))))
                (and (> n 0) (format "%d %s" n (if (= n 1) one many))))))
    (let ((parts (delq nil
                       (list (and (plist-get info :awaiting)
                                  "awaiting a reply")
                             (and (plist-get info :related)
                                  "related reports (TAB, or C-u TAB on a series)")
                             (tally :patches "patch" "patches")
                             (tally :events "calendar file" "calendar files")
                             (tally :texts "text file" "text files")))))
      (and parts (string-join parts ", ")))))

(defun gnaw--list-cell (key info entry &optional mid)
  "Return the display string for column KEY of a report.
INFO is the report plist; ENTRY its state.edn alist and MID its
message-id (both used for the mark: D flags a pending dismissal).
The Flags and Att cells carry a help echo spelling them out, which
`tabulated-list-print-col' preserves.  Every cell carries a
mouse-face, so mouse-1 opens the report (see the follow-link entry
of `gnaw-list-mode-map')."
  (propertize
   (gnaw--list-cell-1 key info entry mid)
   'mouse-face 'highlight))

(defun gnaw--list-cell-1 (key info entry mid)
  "Return the bare display string for column KEY (see `gnaw--list-cell')."
  (pcase key
    (:mark (if (and mid (member mid gnaw-list--flagged))
               "D"
             (gnaw-mark-prefix entry)))
    (:att (let ((s (gnaw--att-string info)))
            (if-let* ((help (gnaw--att-help info)))
                (propertize s 'help-echo (concat "Att: " help))
              s)))
    (:flags (let ((f (or (plist-get info :flags) "")))
              (if-let* ((help (gnaw--flags-help info)))
                  (propertize f 'help-echo (concat "Flags: " help))
                f)))
    (:msgs (let ((n (plist-get info :replies)))   ; thread size, initial mail included
             (if n (number-to-string (1+ n)) "")))
    (:priority (gnaw-priority-letter (plist-get info :priority)))
    (:date (let ((d (plist-get info :date)))   ; keep the YYYY-MM-DD part only
             (if d (substring d 0 (min 10 (length d))) "")))
    (:subject (let ((s (or (plist-get info :subject) "")))
                (cond ((plist-get info :series-head) (concat "▾ " s))
                      ((plist-get info :series-child) (concat "  " s))
                      ((plist-get info :series-summary)
                       (format "▸ %s  (%s)" s (plist-get info :series-summary)))
                      (t s))))
    (_ (let ((v (plist-get info key))) (if v (format "%s" v) "")))))

(defun gnaw--mark-rank (cell)
  "Sort rank for a Mark-column CELL string, depending on the view.
Dismissed hidden: dismiss < normal < sticky; dismissed shown: sticky <
normal < dismiss."
  (let ((ch (and (> (length cell) 0) (aref cell 0))))
    (cond ((eq ch ?!) (if gnaw-list--show-dismissed 0 2))
          ((eq ch ?d) (if gnaw-list--show-dismissed 2 0))
          (t 1))))

(defun gnaw--mark-sort (a b)
  "Sort tabulated-list entries A and B by their Mark column."
  (let ((i (or gnaw-list--mark-index 0)))
    (< (gnaw--mark-rank (aref (cadr a) i))
       (gnaw--mark-rank (aref (cadr b) i)))))

(defun gnaw--priority-sort (a b)
  "Sort tabulated-list entries A and B by numeric report priority."
  (< (or (plist-get (cdr (car a)) :priority) 0)
     (or (plist-get (cdr (car b)) :priority) 0)))

(defun gnaw--votes-number (v)
  "Return the numeric score of a report `:votes' field V, or 0.
V may be a number or a \"score/total\" string."
  (cond ((numberp v) v)
        ((stringp v) (string-to-number v))
        (t 0)))

(defun gnaw--votes-sort (a b)
  "Sort tabulated-list entries A and B by numeric vote score."
  (< (gnaw--votes-number (plist-get (cdr (car a)) :votes))
     (gnaw--votes-number (plist-get (cdr (car b)) :votes))))

(defun gnaw--msgs-sort (a b)
  "Sort tabulated-list entries A and B by thread message count."
  (< (or (plist-get (cdr (car a)) :replies) 0)
     (or (plist-get (cdr (car b)) :replies) 0)))

(defun gnaw--subject-sort (a b)
  "Sort tabulated-list entries A and B by raw subject.
Sorting the printed cell instead would sort the ▸/▾ and indentation
prefixes of series rows, not their subjects."
  (string-lessp (or (plist-get (cdr (car a)) :subject) "")
                (or (plist-get (cdr (car b)) :subject) "")))

(defun gnaw--active-columns ()
  "Return `gnaw-list-columns' minus those named in config `:skip-columns'."
  (let ((skip (mapcar #'downcase (plist-get (gnaw-load-config) :skip-columns))))
    (cl-remove-if (lambda (c) (member (downcase (car c)) skip)) gnaw-list-columns)))

(defun gnaw--list-format ()
  "Return the `tabulated-list-format' vector for the active columns.
Grow the Subject column to fill the window; other columns keep their
width, so the trailing Created column stays at the right edge."
  (let* ((cols (gnaw--active-columns))
         (others (cl-remove-if (lambda (c) (eq (nth 3 c) :subject)) cols))
         (used (apply #'+ tabulated-list-padding 1
                      (mapcar (lambda (c) (1+ (nth 1 c))) others)))
         (subj (max 20 (- (window-body-width) used))))
    (setq-local gnaw-list--mark-index
                (cl-position :mark cols :key (lambda (c) (nth 3 c))))
    (vconcat (mapcar (lambda (c)
                       (list (nth 0 c)
                             (if (eq (nth 3 c) :subject) subj (nth 1 c))
                             (nth 2 c)))
                     cols))))

(defun gnaw--list-entries-related ()
  "Return `tabulated-list-entries' for the related-reports view.
Show the reports of `gnaw-list--related-mids' only, closed ones in
italic, ignoring the filter query and the dismissed filter.  Related
reports absent from the loaded sources (e.g. closed ones when the
source is an open-reports JSON) get a placeholder row built from the
relation metadata, shown in `gnaw-missing' face."
  (let ((state (gnaw-read-state))
        (cols (gnaw--active-columns))
        found rows)
    (dolist (p gnaw-list--reports)
      (when (member (car p) gnaw-list--related-mids)
        (push (car p) found)
        (let* ((entry (cdr (assoc (car p) state)))
               (faces (delq nil
                            (list (cond ((assq :sticky entry) 'gnaw-sticky)
                                        ((assq :dismiss entry) 'gnaw-dismissed))
                                  (and (gnaw--closed-p (cdr p)) 'gnaw-closed))))
               (cells (mapcar (lambda (c)
                                (gnaw--list-cell (nth 3 c) (cdr p)
                                                 entry (car p)))
                              cols)))
          (when faces
            (setq cells (mapcar (lambda (s) (propertize s 'face faces))
                                cells)))
          (push (list p (vconcat cells)) rows))))
    ;; Placeholder rows for the related mids the sources do not carry.
    ;; RET still works when the mail client can look the mid up (local
    ;; mailbox) or through the archive URL carried by the relation.
    ;; The Date column shows when the relation was posed; the help echo
    ;; spells out the relation kind, its setter and the message-id.
    (let ((origin (cdr (assoc (car gnaw-list--related-mids)
                              gnaw-list--reports))))
      (dolist (mid (cdr gnaw-list--related-mids))
        (unless (member mid found)
          (let* ((e (cdr (assoc mid gnaw-list--related-entries)))
                 (setter   (alist-get 'setter e))
                 (posed-at (alist-get 'posed-at e))
                 (info (list :type (or (alist-get 'type e) "?")
                             :subject (or (alist-get 'subject e) mid)
                             :priority 0
                             :date posed-at
                             :source (plist-get origin :source)
                             :source-name (plist-get origin :source-name)
                             :archived-at (alist-get 'archived-at e)
                             :missing t))
                 (help (mapconcat
                        #'identity
                        (delq nil
                              (list (gnaw--relation-kind-label
                                     (alist-get 'kind e))
                                    (when setter (format "set by %s" setter))
                                    (when posed-at (format "on %s" posed-at))
                                    mid
                                    "not in the loaded sources"))
                        "; "))
                 (cells (mapcar (lambda (c)
                                  (propertize
                                   (gnaw--list-cell (nth 3 c) info nil mid)
                                   'face 'gnaw-missing
                                   'help-echo help))
                                cols)))
            (push (list (cons mid info) (vconcat cells)) rows)))))
    (nreverse rows)))

(defun gnaw--list-entries ()
  "Return `tabulated-list-entries', folding patch series unless expanded.
A series is shown as one representative row (cover letter or first
patch) prefixed with ▸ and a status summary; unfolded series (in
`gnaw-list--expanded') list each patch, ▾ marking the first row.
The query in `gnaw-list--query' filters the result.  When the list
is narrowed to related reports, delegate to
`gnaw--list-entries-related'."
  (if gnaw-list--related-mids
      (gnaw--list-entries-related)
    (gnaw--list-entries-full)))

(defun gnaw--list-entries-full ()
  "Return `tabulated-list-entries' for the full report list."
  (let ((state (gnaw-read-state))
        (cols (gnaw--active-columns))
        (qgroups (and gnaw-list--query (gnaw--query-compile gnaw-list--query)))
        (pairs gnaw-list--reports)
        (groups (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (rows nil))
    (dolist (p pairs)
      (let ((sid (gnaw--series-id (cdr p))))
        (when sid (push p (gethash sid groups)))))
    (cl-flet ((row (pair &optional match-pairs)
                ;; Dismiss is judged on the displayed PAIR; the query may
                ;; match any of MATCH-PAIRS, so a folded series stays
                ;; visible when any member matches.  Unfolding it then
                ;; filters each member individually.
                (let* ((entry   (cdr (assoc (car pair) state)))
                       (sticky  (and (assq :sticky entry) t))
                       (dismiss (and (assq :dismiss entry) t)))
                  (when (and (or gnaw-list--show-dismissed (not dismiss))
                             (or (null qgroups)
                                 (seq-some
                                  (lambda (mp)
                                    (gnaw--query-match-p qgroups (car mp) (cdr mp)))
                                  (or match-pairs (list pair)))))
                    (let ((cells (mapcar (lambda (c)
                                           (gnaw--list-cell (nth 3 c) (cdr pair)
                                                            entry (car pair)))
                                         cols)))
                      (cond
                       (sticky  (setq cells (mapcar (lambda (s) (propertize s 'face 'gnaw-sticky))
                                                    cells)))
                       (dismiss (setq cells (mapcar (lambda (s) (propertize s 'face 'gnaw-dismissed))
                                                    cells))))
                      (push (list pair (vconcat cells)) rows))))))
      (dolist (p pairs)
        (let ((sid (gnaw--series-id (cdr p))))
          (cond
           ((null sid) (row p))
           ((gethash sid seen) nil)
           (t
            (puthash sid t seen)
            (let* ((members (sort (nreverse (gethash sid groups))
                                  (lambda (a b)
                                    (< (gnaw--patch-seq-n (cdr a))
                                       (gnaw--patch-seq-n (cdr b))))))
                   (multi (cdr members)))
              (if (and multi (member sid gnaw-list--expanded))
                  (let ((head t))
                    (dolist (m members)
                      (row (cons (car m)
                                 (plist-put (copy-sequence (cdr m))
                                            (if head :series-head :series-child)
                                            t)))
                      (setq head nil)))
                (let* ((cover (seq-find (lambda (m) (gnaw--cover-p (cdr m))) members))
                       (rep (or cover (car members))))
                  (row (cons (car rep)
                             (if multi
                                 (plist-put (copy-sequence (cdr rep)) :series-summary
                                            (gnaw--series-summary (mapcar #'cdr members)))
                               (cdr rep)))
                       members))))))))
      (nreverse rows))))

(defvar gnaw-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gnaw-list-open)
    (define-key map (kbd "SPC") #'gnaw-list-open-other-window)
    (define-key map "b" #'gnaw-list-browse)
    (define-key map [mouse-2] #'gnaw-list-mouse-open)
    ;; Let mouse-1 follow the rows, which carry mouse-face (the
    ;; package-menu convention).
    (define-key map [follow-link] 'mouse-face)
    (define-key map "f" #'gnaw-list-follow-mode)
    (define-key map (kbd "TAB") #'gnaw-list-tab)
    (define-key map "v" #'gnaw-list-attachment-view)
    (define-key map "S" #'gnaw-list-attachment-save)
    (define-key map "A" #'gnaw-list-patch-apply)
    (define-key map ":" #'gnaw-list-attachments)
    (define-key map "g" #'gnaw-list-reload)
    (define-key map "G" #'gnaw-list-update)
    (define-key map "/" #'gnaw-list-filter)
    (define-key map "=" #'gnaw-list-filter-transient)
    (define-key map "t" #'gnaw-list-limit-type)
    (define-key map "T" #'gnaw-list-filter-topic)
    (define-key map "a" #'gnaw-list-filter-acked)
    (define-key map "o" #'gnaw-list-filter-owned)
    (define-key map "c" #'gnaw-list-limit-closed)
    (define-key map "." #'gnaw-list-limit-awaiting)
    (define-key map "?" #'gnaw-show-help)
    (define-key map "~" #'gnaw-list-limit-related)
    (define-key map "+" #'gnaw-list-limit-attachments)
    (define-key map (kbd "<C-return>") #'gnaw-list-filter-cell)
    (define-key map "!" #'gnaw-list-toggle-sticky)
    (define-key map "d" #'gnaw-list-flag-dismiss)
    (define-key map "D" #'gnaw-list-toggle-dismiss)
    (define-key map "x" #'gnaw-list-execute-flags)
    (define-key map "u" #'gnaw-list-remove-marks)
    (define-key map "h" #'gnaw-show-help)
    (define-key map "s" #'tabulated-list-sort)
    (define-key map "^" #'gnaw-sort)
    (define-key map "_" #'gnaw-list-toggle-dismissed)
    (define-key map "\\" #'gnaw-select-preset-filter)
    (define-key map "q" #'gnaw-list-quit)
    map)
  "Keymap for `gnaw-list-mode'.")

(define-derived-mode gnaw-list-mode tabulated-list-mode "Gnaw"
  "Major mode listing open BONE reports.
\\<gnaw-list-mode-map>Press \\[describe-mode] for the full list of key bindings."
  (setq tabulated-list-format (gnaw--list-format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key
        (and gnaw-list-sort-key
             (assoc (car gnaw-list-sort-key) (gnaw--active-columns))
             gnaw-list-sort-key))
  ;; The native refresh entry points: C-x x g, auto-revert...
  (setq-local revert-buffer-function
              (lambda (&rest _) (gnaw-list-reload)))
  ;; C-x r m bookmarks the current view (filter, sort, position).
  (setq-local bookmark-make-record-function
              #'gnaw-list--bookmark-make-record)
  (tabulated-list-init-header))

(declare-function bookmark-prop-get "bookmark")

(defun gnaw-list--bookmark-make-record ()
  "Return a `bookmark.el' record for the current gnaw list view."
  `(,(concat "gnaw" (and gnaw-list--query (format ": %s" gnaw-list--query)))
    (handler . gnaw-list-bookmark-jump)
    (query . ,gnaw-list--query)
    (sort-key . ,tabulated-list-sort-key)
    (mid . ,(car (tabulated-list-get-id)))))

;;;###autoload
(defun gnaw-list-bookmark-jump (bookmark)
  "Restore the gnaw list view saved in BOOKMARK."
  (gnaw)
  (setq-local gnaw-list--query (bookmark-prop-get bookmark 'query))
  (when-let* ((key (bookmark-prop-get bookmark 'sort-key))
              ((assoc (car key) (gnaw--active-columns))))
    (setq-local tabulated-list-sort-key key))
  (gnaw-list-refresh)
  (when-let* ((mid (bookmark-prop-get bookmark 'mid)))
    (gnaw-list--goto-mid mid)))

(defun gnaw-list--update-mode-line ()
  "Reflect the view (related or query) and pending flags in the mode line."
  (let ((s (concat
            (cond (gnaw-list--related-mids " [related]")
                  (gnaw-list--query (format " [%s]" gnaw-list--query)))
            (and gnaw-list--flagged
                 (format " [%d flagged]" (length gnaw-list--flagged))))))
    (setq mode-line-process (and (not (string-empty-p s)) s))
    (force-mode-line-update)))

(defun gnaw-list-refresh ()
  "Re-render the list from the in-memory reports and current state.
Does not re-read the report cache; use `gnaw-list-reload' for that."
  (interactive)
  ;; Guard every list command funneling through here: `tabulated-list-print'
  ;; would erase whatever buffer is current.
  (unless (derived-mode-p 'gnaw-list-mode)
    (user-error "Not in a gnaw report list"))
  ;; Restore `window-start': `tabulated-list-print' reprints the buffer,
  ;; which would otherwise recenter the window on each refresh.  When
  ;; the rows shift so much that the old start leaves point off-screen
  ;; (setting or clearing a filter), keep point on the same window line
  ;; instead of letting redisplay center it.
  (let* ((win (and (eq (current-buffer) (window-buffer)) (selected-window)))
         (start (and win (window-start win)))
         ;; From bol to bol: `count-lines' counts a partial line as one.
         (wline (and win (count-lines start (line-beginning-position)))))
    (setq tabulated-list-format (gnaw--list-format))
    (tabulated-list-init-header)
    (gnaw-list--update-mode-line)
    (setq tabulated-list-entries (gnaw--list-entries))
    (tabulated-list-print t)
    (force-mode-line-update)
    (when win
      (set-window-start win start t)
      (unless (pos-visible-in-window-p (point) win)
        (recenter wline)))))

(defun gnaw-list-reload ()
  "Re-read reports from the local cache, then re-render.
Reset the subject-words cache, then re-warm it once Emacs has been
idle for a second, so the first similar: filter finds it filled."
  (interactive)
  (setq-local gnaw-list--reports (gnaw-reports))
  (clrhash gnaw--subject-words-cache)
  (when (timerp gnaw--subject-words-timer)
    (cancel-timer gnaw--subject-words-timer))
  (setq gnaw--subject-words-timer
        (run-with-idle-timer
         1 nil
         (lambda (reports)
           (setq gnaw--subject-words-timer nil)
           (dolist (p reports)
             (gnaw--subject-words (plist-get (cdr p) :subject))))
         gnaw-list--reports))
  (gnaw-list-refresh))

(defun gnaw-list-filter-clear ()
  "Clear the active `gnaw-list' filter query."
  (interactive)
  (setq-local gnaw-list--related-mids nil)  ; filtering leaves the related view
  (setq-local gnaw-list--query nil)
  (gnaw-list-refresh)
  (message "gnaw: filter cleared"))

(defun gnaw-list-filter (query)
  "Filter the report list by QUERY; an empty QUERY clears the filter.
QUERY combines `key:value' tokens with spaces (AND) and `|' (OR).
With a prefix argument, clear the active filter without prompting."
  (interactive
   (list (if current-prefix-arg
             ""
           (let ((minibuffer-local-completion-map
                  (let ((m (copy-keymap minibuffer-local-completion-map)))
                    (define-key m " " nil)   ; let SPACE separate tokens
                    (define-key m "?" nil)   ; and `?' self-insert
                    m)))
             (completing-read "Filter: " #'gnaw--filter-completion
                              nil nil gnaw-list--query)))))
  (setq-local gnaw-list--related-mids nil)  ; filtering leaves the related view
  (setq-local gnaw-list--query
              (and (not (string-empty-p (string-trim query))) query))
  (gnaw-list-refresh)
  (if gnaw-list--query
      (message "gnaw: filter %s" gnaw-list--query)
    (message "gnaw: filter cleared")))

(defun gnaw-list-update ()
  "Refresh the remote cache, then reload the list."
  (interactive)
  (gnaw-update)
  (gnaw-list-reload))

(defun gnaw-list-limit-type (&optional add)
  "Limit the list to a chosen report type.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter-by "type" add))

(defun gnaw-list-limit-closed (&optional add)
  "Limit the list to closed reports, whatever the close reason.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter (gnaw-list--query-add "flags:C,R,E,S" add)))

(defun gnaw-list-limit-awaiting (&optional add)
  "Limit the list to reports awaiting a reply.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter (gnaw-list--query-add "att:." add)))

(defun gnaw-list-limit-related (&optional add)
  "Limit the list to reports with related reports.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter (gnaw-list--query-add "att:~" add)))

(defun gnaw-list-limit-attachments (&optional add)
  "Limit the list to reports carrying at least one attachment.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter (gnaw-list--query-add "att:+,x,@,#" add)))

(defvar-local gnaw-list--cell-filter nil
  "State of the `gnaw-list-filter-cell' toggle, or nil.
A list (QUERY PREV-QUERY PREV-RELATED-MIDS PREV-RELATED-ENTRIES):
the query the command set, then the view it replaced, restored when
the command is called again while QUERY is still active.")

(defun gnaw-list-filter-cell (&optional add)
  "Toggle a filter built from the value of the cell at point.
On the From column, keep the author's reports; on Type, the reports
of that type; on Created, the reports created on or after that date;
on Subject, the reports with a similar subject (at least three
significant words in common, see `gnaw--query-similar-matcher') --
signaling a `user-error' instead of filtering when no other report
has a similar subject.
Outside any cell (the leading padding or past the last column, where
point commonly rests), fall back on the Subject column.  While the
filter set by this command is active, calling it again restores the
view the filter replaced (a query, a related-reports narrowing, or
the full list).  With a prefix argument ADD, add the condition to
the active filter (AND) instead of replacing it."
  (interactive "P")
  (if (and gnaw-list--cell-filter
           (equal gnaw-list--query (car gnaw-list--cell-filter)))
      (pcase-let ((`(,_ ,query ,mids ,entries) gnaw-list--cell-filter))
        (setq gnaw-list--cell-filter nil)
        (setq-local gnaw-list--query query)
        (setq-local gnaw-list--related-mids mids)
        (setq-local gnaw-list--related-entries entries)
        (gnaw-list-refresh)
        (message "gnaw: %s" (cond (mids "back to the related view")
                                  (query (format "filter %s" query))
                                  (t "filter cleared"))))
    (let ((info (cdr (gnaw-list--current)))
          (col (or (get-text-property (point) 'tabulated-list-column-name)
                   ;; End of line: the cell just before point.
                   (and (> (point) (line-beginning-position))
                        (get-text-property (1- (point))
                                           'tabulated-list-column-name))
                   "Subject"))
          (prev (list gnaw-list--query gnaw-list--related-mids
                      gnaw-list--related-entries)))
      (gnaw-list-filter
       (gnaw-list--query-add
        (pcase col
          ("From"
           (let ((from (or (plist-get info :from)
                           (plist-get info :from-name))))
             (when (member from '(nil ""))
               (user-error "No author on this row"))
             (format "from:%s" (gnaw--query-quote-val from))))
          ("Type" (format "type:%s" (or (plist-get info :type) "bug")))
          ("Created"
           (let ((d (plist-get info :date)))
             (unless d (user-error "No creation date on this row"))
             (format "date:%s.." (substring d 0 (min 10 (length d))))))
          ("Subject"
           (let ((words (gnaw--subject-words (plist-get info :subject))))
             (unless words (user-error "No significant word in this subject"))
             (let* ((val (string-join words "+"))
                    (m (gnaw--query-similar-matcher val)))
               ;; The report always matches its own words: fewer than
               ;; two matches means filtering would leave it alone.
               (when (< (seq-count
                         (lambda (p)
                           (funcall m (plist-get (cdr p) :subject)))
                         gnaw-list--reports)
                        2)
                 (user-error "No other report with a similar subject"))
               (format "similar:%s" val))))
          (_ (user-error "No cell filter for the %s column" col)))
        add))
      (setq gnaw-list--cell-filter (cons gnaw-list--query prev)))))

(defun gnaw-list--current ()
  "Return the (MID . INFO) pair at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No report at point")))

(defun gnaw-list-open ()
  "Open the email of the report at point."
  (interactive)
  (let ((p (gnaw-list--current)))
    (gnaw-read-message (car p) (cdr p))))

(defun gnaw-list-mouse-open (event)
  "Open the email of the report clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (gnaw-list-open))

(defun gnaw-list-browse ()
  "Browse the archived web page of the report at point."
  (interactive)
  (pcase-let ((`(,mid . ,info) (gnaw-list--current)))
    (let ((url (gnaw-message-archive-url mid info)))
      (unless url (user-error "No archive URL for this report"))
      (browse-url url))))

(defun gnaw-list--open-below (pair)
  "Open PAIR's mail and arrange it below the list, point staying there.
Let the mail client display the message as usual, then rearrange the
frame: list in the top half, message below, point back in the list.
With Gnus the bottom window shows the article buffer.  Best effort
with clients that display asynchronously (mu4e): when no buffer has
appeared yet, the frame is left for the client to fill.  An existing
two-window split is reused rather than rebuilt."
  (let ((list-buf (current-buffer)))
    (gnaw-read-message (car pair) (cdr pair))
    (let* ((selected (window-buffer (selected-window)))
           (mail-buf (or (and (provided-mode-derived-p
                               (buffer-local-value 'major-mode selected)
                               'gnus-summary-mode)
                              (bound-and-true-p gnus-article-buffer)
                              (get-buffer gnus-article-buffer))
                         selected))
           (list-win (get-buffer-window list-buf)))
      (unless (eq mail-buf list-buf)
        (if (and list-win (= (length (window-list)) 2))
            (let ((other (seq-find (lambda (w) (not (eq w list-win)))
                                   (window-list))))
              (set-window-buffer other mail-buf)
              (select-window list-win))
          (delete-other-windows)
          (switch-to-buffer list-buf)
          (set-window-buffer (split-window-below) mail-buf))
        (with-current-buffer list-buf
          (setq gnaw-list--below-mid (car pair)))))))

(defun gnaw-list-open-other-window ()
  "Toggle the report's email below the list, point staying in the list.
Open it as `gnaw-list--open-below' does; when the report's mail is
already the one shown, close the other windows instead."
  (interactive)
  (let ((p (gnaw-list--current)))
    (if (and (equal (car p) gnaw-list--below-mid)
             (cdr (window-list)))
        (progn (delete-other-windows)
               (setq gnaw-list--below-mid nil))
      (gnaw-list--open-below p))))

(defcustom gnaw-list-follow-delay 0.3
  "Idle seconds before `gnaw-list-follow-mode' shows the report at point."
  :type 'number
  :group 'gnaw)

(defvar-local gnaw-list--follow-last-mid nil
  "Message-id last shown by `gnaw-list-follow-mode'.")

(defvar-local gnaw-list--follow-timer nil
  "Pending idle timer of `gnaw-list-follow-mode'.")

(defun gnaw-list--follow-show (buffer)
  "Show the mail of the report at point in BUFFER, as SPC would.
Do nothing unless BUFFER is still shown in the selected window."
  (when (and (buffer-live-p buffer)
             (eq (window-buffer (selected-window)) buffer))
    (with-current-buffer buffer
      (setq gnaw-list--follow-timer nil)
      (let ((p (tabulated-list-get-id)))
        (when (and p (not (equal (car p) gnaw-list--follow-last-mid)))
          (setq gnaw-list--follow-last-mid (car p))
          (condition-case err
              (gnaw-list--open-below p)
            (error (message "gnaw: %s" (error-message-string err)))))))))

(defun gnaw-list--follow-schedule ()
  "Debounce: show the report at point once Emacs has been idle."
  (let ((mid (car (tabulated-list-get-id))))
    (when (and mid (not (equal mid gnaw-list--follow-last-mid)))
      (when gnaw-list--follow-timer (cancel-timer gnaw-list--follow-timer))
      (setq gnaw-list--follow-timer
            (run-with-idle-timer gnaw-list-follow-delay nil
                                 #'gnaw-list--follow-show
                                 (current-buffer))))))

(define-minor-mode gnaw-list-follow-mode
  "Show the mail of the report at point as point moves in the list.
Moving point displays, after `gnaw-list-follow-delay' idle seconds,
the corresponding mail below the list, as
\\<gnaw-list-mode-map>\\[gnaw-list-open-other-window] would."
  :lighter " Follow"
  (unless (derived-mode-p 'gnaw-list-mode)
    (setq gnaw-list-follow-mode nil)
    (user-error "Not in a gnaw report list"))
  (if gnaw-list-follow-mode
      (add-hook 'post-command-hook #'gnaw-list--follow-schedule nil t)
    (remove-hook 'post-command-hook #'gnaw-list--follow-schedule t)
    (when gnaw-list--follow-timer
      (cancel-timer gnaw-list--follow-timer)
      (setq gnaw-list--follow-timer nil))
    (setq gnaw-list--follow-last-mid nil)
    ;; Close the mail preview: back to the list alone.
    (when-let* ((w (get-buffer-window (current-buffer))))
      (with-selected-window w (delete-other-windows)))
    (setq gnaw-list--below-mid nil)))

(defun gnaw-list--follow-off ()
  "Turn off `gnaw-list-follow-mode' when it is on."
  (when gnaw-list-follow-mode
    (gnaw-list-follow-mode -1)))

(defun gnaw--patch-target ()
  "Return (INFO . PATCHES) for the patch command being invoked.
The scope of `gnaw-list-patch-transient' when called from the menu,
else the report at point with all its patches (PATCHES nil)."
  (or (and transient-current-prefix
           (eq (oref transient-current-prefix command) 'gnaw-list-patch-transient)
           (oref transient-current-prefix scope))
      (cons (cdr (gnaw-list--current)) nil)))

(defun gnaw-list-patch-view ()
  "View the target patches in a `diff-mode' buffer."
  (interactive)
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (gnaw-view-patches info patches)))

(defun gnaw-list-patch-apply (&optional am)
  "Apply the target patches with `git apply'.
With a prefix argument AM, apply them as commits with `git am'."
  (interactive "P")
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (if am
        (gnaw-am-patches info patches)
      (gnaw-apply-patches info patches))))

(defun gnaw-list-patch-am ()
  "Apply the target patches with `git am'."
  (interactive)
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (gnaw-am-patches info patches)))

(defun gnaw-list-patch-save (&optional no-confirm)
  "Save the target patch files to a directory.
With a prefix argument NO-CONFIRM, save into the source's
configured repo without asking."
  (interactive "P")
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (gnaw-save-patches info no-confirm patches)))

(transient-define-prefix gnaw-list-patch-transient (info &optional patches)
  "Act on PATCHES of report INFO -- all of its patches when nil."
  [["Patches"
    ("v" "View in diff-mode" gnaw-list-patch-view)
    ("a" "Apply (git apply)" gnaw-list-patch-apply)
    ("m" "Apply as commits (git am)" gnaw-list-patch-am)
    ("s" "Save to a directory" gnaw-list-patch-save)]]
  (interactive (list (cdr (gnaw-list--current))))
  (transient-setup 'gnaw-list-patch-transient nil nil
                   :scope (cons info patches)))

(defun gnaw--attachment-act (info att)
  "Act on ATT, a (TYPE . ENTRY) attachment of report INFO.
A patch opens the patch menu, TYPE `all-patches' opens it on every
patch of the report; other files are displayed."
  (pcase (car att)
    ('patch (gnaw-list-patch-transient info (list (cdr att))))
    ('all-patches (gnaw-list-patch-transient info nil))
    (_ (gnaw--show-attachment info att))))

(defun gnaw--show-attachment (info att)
  "Display ATT, a (TYPE . ENTRY) attachment of report INFO."
  (pcase-let* ((`(,type . ,entry) att)
               (file (or (gnaw-attachment-file info entry type)
                         (user-error "Cannot fetch attachment %s"
                                     (alist-get 'file entry)))))
    (with-current-buffer (get-buffer-create
                          (format "*gnaw %s*" (file-name-nondirectory file)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min)))))

(defun gnaw--choose-attachment (info prompt)
  "Choose one attachment of report INFO, prompting with PROMPT.
Return a (TYPE . ENTRY) pair; a single attachment is returned
without prompting.  On a multi-patch report, an extra candidate
returns (all-patches) to act on every patch at once."
  (let* ((atts (gnaw--attachments info))
         (patches (plist-get info :patches))
         (cands (append
                 (when (cdr patches)
                   (list (cons (format "all patches (%d)" (length patches))
                               '(all-patches))))
                 (mapcar (lambda (att)
                           (cons (format "%s: %s"
                                         (pcase (car att)
                                           ('patch "patch")
                                           ('event "ics")
                                           ('text  "text"))
                                         (file-name-nondirectory
                                          (or (alist-get 'file (cdr att)) "")))
                                 att))
                         atts))))
    (pcase atts
      ('() (user-error "This report has no attachments"))
      (`(,att) att)
      (_ (cdr (assoc (completing-read prompt cands nil t) cands))))))

(defun gnaw-list-attachment-view ()
  "View an attachment of the report at point, asking which when several.
Patches are shown in `diff-mode', other files in a read-only buffer."
  (interactive)
  (let* ((info (cdr (gnaw-list--current)))
         (att (gnaw--choose-attachment info "View attachment: ")))
    (pcase (car att)
      ('patch       (gnaw-view-patches info (list (cdr att))))
      ('all-patches (gnaw-view-patches info nil))
      (_            (gnaw--show-attachment info att)))))

(defun gnaw-list-attachment-save (&optional no-confirm)
  "Save an attachment of the report at point, asking which when several.
The target directory prompt proposes the source's configured `:repo'
\(or `gnaw-apply-repo').  With a prefix argument NO-CONFIRM, save
there without asking and overwrite silently.  Patches go through
`gnaw-save-patches'."
  (interactive "P")
  (let* ((info (cdr (gnaw-list--current)))
         (att (gnaw--choose-attachment info "Save attachment: ")))
    (pcase (car att)
      ('patch       (gnaw-save-patches info no-confirm (list (cdr att))))
      ('all-patches (gnaw-save-patches info no-confirm nil))
      (_ (let* ((entry (cdr att))
                (file (or (gnaw-attachment-file info entry (car att))
                          (user-error "Cannot fetch attachment %s"
                                      (alist-get 'file entry))))
                (repo (or (gnaw--source-repo info) gnaw-apply-repo))
                (dir (if (and repo no-confirm)
                         (file-name-as-directory repo)
                       (read-directory-name "Save attachment in: " repo))))
           (copy-file file (expand-file-name (file-name-nondirectory file) dir)
                      (if no-confirm t 1))
           (message "gnaw: saved %s in %s"
                    (file-name-nondirectory file) dir))))))

(defun gnaw-list-attachments ()
  "Act on the attachments of the report at point.
A single patch opens the patch menu; a single calendar or text
attachment is displayed right away; several attachments are listed
in a buffer where + acts again on the attachment at point."
  (interactive)
  (let* ((info (cdr (gnaw-list--current)))
         (atts (gnaw--attachments info)))
    (pcase atts
      ('() (user-error "This report has no attachments"))
      (`(,att) (gnaw--attachment-act info att))
      (_ (gnaw--list-attachments info atts)))))

(defvar-local gnaw-attachments--info nil
  "Report plist whose attachments the current buffer lists.
A snapshot from when the buffer was built: it is not refreshed when
the report list is updated.")

(defvar gnaw-attachments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" #'gnaw-attachments-act)
    (define-key map (kbd "RET") #'gnaw-attachments-act)
    map)
  "Keymap for `gnaw-attachments-mode'.")

(define-derived-mode gnaw-attachments-mode tabulated-list-mode "Gnaw-Attachments"
  "Major mode listing the attachments of a report."
  (setq tabulated-list-format [("Type" 6 t) ("File" 60 t)])
  (tabulated-list-init-header))

(defun gnaw--list-attachments (info atts)
  "List ATTS, the (TYPE . ENTRY) attachments of report INFO, in a buffer.
When the report has several patches, a first row acts on all of them
at once."
  (with-current-buffer (get-buffer-create "*gnaw-attachments*")
    (gnaw-attachments-mode)
    (setq gnaw-attachments--info info)
    (setq tabulated-list-entries
          (append
           (when-let* ((patches (plist-get info :patches))
                       ((cdr patches)))
             (list (list '(all-patches)
                         (vector "patch" (format "all patches (%d)"
                                                 (length patches))))))
           (mapcar (lambda (att)
                     (list att
                           (vector (pcase (car att)
                                     ('patch "patch") ('event "ics") ('text "text"))
                                   (file-name-nondirectory
                                    (or (alist-get 'file (cdr att)) "")))))
                   atts)))
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))
    (message "gnaw: type + or RET to act on the attachment at point")))

(defun gnaw-attachments-act ()
  "Act on the attachment at point: patch menu, or display the file."
  (interactive)
  (let ((att (tabulated-list-get-id)))
    (unless att (user-error "No attachment at point"))
    (gnaw--attachment-act gnaw-attachments--info att)))

(defun gnaw-list-patch-series-fold ()
  "Fold or unfold the patch series of the report at point."
  (interactive)
  (let* ((pair (gnaw-list--current))
         (sid (gnaw--series-id (cdr pair)))
         (mid (car pair)))
    (unless sid (user-error "Not part of a patch series"))
    (when (< (cl-count sid gnaw-list--reports
                       :key (lambda (p) (gnaw--series-id (cdr p)))
                       :test #'equal)
             2)
      (user-error "This series has no other patch to unfold"))
    (setq-local gnaw-list--expanded
                (if (member sid gnaw-list--expanded)
                    (remove sid gnaw-list--expanded)
                  (cons sid gnaw-list--expanded)))
    (gnaw-list-refresh)
    ;; Return to the same report; if it is gone (folded from a sub-patch),
    ;; fall back to the series' representative row.
    (unless (gnaw-list--goto-mid mid)
      (gnaw-list--goto
       (lambda (p) (equal (gnaw--series-id (cdr p)) sid))))))

(defun gnaw-list-related-narrow ()
  "Narrow the list to the report at point and its related reports.
Closed related reports are shown in italic.  \\<gnaw-list-mode-map>\
\\[gnaw-list-tab] restores the full list."
  (interactive)
  (let* ((pair (gnaw-list--current))
         (related (or (plist-get (cdr pair) :related)
                      (user-error "This report has no related reports")))
         (mids (cons (car pair)
                     (mapcar (lambda (e)
                               (gnaw-normalize-mid (alist-get 'message-id e)))
                             related)))
         (missing (cl-count-if-not (lambda (mid) (assoc mid gnaw-list--reports))
                                   (cdr mids))))
    (setq-local gnaw-list--related-mids mids)
    (setq-local gnaw-list--related-entries
                (mapcar (lambda (e)
                          (cons (gnaw-normalize-mid (alist-get 'message-id e))
                                e))
                        related))
    (gnaw-list-refresh)
    (gnaw-list--goto-mid (car mids))
    (message "gnaw: %d related report(s)%s; TAB or q to come back"
             (1- (length mids))
             (if (> missing 0)
                 (format " (%d not in the sources, greyed out)" missing)
               ""))))

(defun gnaw-list-related-restore ()
  "Restore the full list after `gnaw-list-related-narrow'."
  (interactive)
  (let ((mid (car gnaw-list--related-mids)))
    (setq-local gnaw-list--related-mids nil)
    (setq-local gnaw-list--related-entries nil)
    (gnaw-list-refresh)
    (when mid (gnaw-list--goto-mid mid))))

(defun gnaw-list-quit ()
  "Leave the related-reports view, clear the filter, or quit the window.
When the list is narrowed to related reports, restore the full
list (like \\<gnaw-list-mode-map>\\[gnaw-list-tab]); when a filter
query is active, clear it; otherwise quit the window as
`quit-window' does."
  (interactive)
  (cond (gnaw-list--related-mids (gnaw-list-related-restore))
        (gnaw-list--query (gnaw-list-filter-clear))
        (t (quit-window))))

(defun gnaw-list-tab (&optional related)
  "Fold or unfold the series at point, or narrow to related reports.
On a report of a multi-patch series, fold or unfold the series; on
a report with related reports, narrow the list to them.  A prefix
argument RELATED narrows to the related reports even on a series
member.  When the list is already narrowed, restore it."
  (interactive "P")
  (if gnaw-list--related-mids
      (gnaw-list-related-restore)
    (let* ((info (cdr (gnaw-list--current)))
           (sid (gnaw--series-id info)))
      (cond ((and related (plist-get info :related))
             (gnaw-list-related-narrow))
            ((and sid (>= (cl-count sid gnaw-list--reports
                                    :key (lambda (p) (gnaw--series-id (cdr p)))
                                    :test #'equal)
                          2))
             (gnaw-list-patch-series-fold))
            ((plist-get info :related) (gnaw-list-related-narrow))
            (t (user-error
                "This report has no related reports or no series to unfold"))))))

(defun gnaw-list--goto (pred)
  "Move point to the first row whose (MID . INFO) pair satisfies PRED.
Return non-nil if found, else return nil with point at end of buffer."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((p (tabulated-list-get-id)))
        (if (and p (funcall pred p))
            (setq found t)
          (forward-line 1))))
    found))

(defun gnaw-list--goto-mid (mid)
  "Move point to the row whose report id is MID.
Return non-nil if found, else return nil with point at end of buffer."
  (gnaw-list--goto (lambda (p) (equal (car p) mid))))

(defun gnaw-list--restore-point (mid line)
  "Move to MID's row, unless it moved away from LINE; then stay at LINE.
Changing a mark can re-sort the list (e.g. under the Mark sort);
following the acted-on row across the buffer would drag the cursor
away from where the user is working."
  (unless (and (gnaw-list--goto-mid mid)
               (= (line-number-at-pos) line))
    (gnaw-list--goto-line line)))

(defun gnaw-list--goto-line (line)
  "Move to LINE, stepping back onto the last row when past the end."
  (goto-char (point-min))
  (forward-line (1- line))
  (when (and (eobp) (not (bobp)))
    (forward-line -1)))

(defun gnaw-list--toggle (action)
  "Toggle local mark ACTION on the report at point, then refresh.
Keep the cursor in place (see `gnaw-list--restore-point')."
  (let ((p (gnaw-list--current))
        (line (line-number-at-pos)))
    (gnaw-toggle-mark (car p) (cdr p) action)
    (gnaw-list-refresh)
    (gnaw-list--restore-point (car p) line)))

(defun gnaw-list-toggle-sticky ()
  "Toggle the sticky mark on the report at point.
Sticky reports are shown in bold and exported to todo.org by the gnaw CLI."
  (interactive)
  (gnaw-list--toggle :sticky))

(defun gnaw-list-toggle-dismiss ()
  "Toggle the dismiss mark (hide) on the report at point, immediately.
Turn off `gnaw-list-follow-mode' first.  Then move to the following
report, so a run of reports can be dismissed without chasing point.
See `gnaw-list-flag-dismiss' for deferred dismissal."
  (interactive)
  (gnaw-list--follow-off)
  (let* ((p (gnaw-list--current))
         (line (line-number-at-pos))
         (next (save-excursion
                 (forward-line 1)
                 (car (tabulated-list-get-id))))
         (on (gnaw-toggle-mark (car p) (cdr p) :dismiss)))
    (gnaw-list-refresh)
    ;; Land on the report that followed; without one (last row), stay in
    ;; place like the other mark commands.
    (unless (and next (gnaw-list--goto-mid next))
      (gnaw-list--restore-point (car p) line))
    (message (if on
                 "gnaw: dismissed (type _ to include dismissed reports to the view)"
               "gnaw: dismiss mark removed"))))

(defun gnaw-list--set-mark-cell (mid)
  "Redraw MID's Mark cell on the current row, when the column is shown.
Much cheaper than `gnaw-list-refresh', which rebuilds and reprints
every row: a flag toggle only changes this one cell."
  (when gnaw-list--mark-index
    (tabulated-list-set-col
     gnaw-list--mark-index
     (if (member mid gnaw-list--flagged)
         "D"
       (gnaw-mark-prefix (cdr (assoc mid (gnaw-read-state)))))
     t)))

(defun gnaw-list-flag-dismiss ()
  "Flag the report at point for dismissal (or unflag it), then move down.
Flagged reports show D in the Mark column and are dismissed all at
once by \\<gnaw-list-mode-map>\\[gnaw-list-execute-flags].  Flags live
in memory only; nothing is written until then."
  (interactive)
  (let ((mid (car (gnaw-list--current))))
    (setq-local gnaw-list--flagged
                (if (member mid gnaw-list--flagged)
                    (delete mid gnaw-list--flagged)
                  (cons mid gnaw-list--flagged)))
    (gnaw-list--set-mark-cell mid)
    (gnaw-list--update-mode-line)
    (forward-line 1)
    (unless (tabulated-list-get-id) (forward-line -1))))

(defun gnaw-list-execute-flags ()
  "Dismiss the reports flagged by \\<gnaw-list-mode-map>\\[gnaw-list-flag-dismiss].
Turn off `gnaw-list-follow-mode' first; write state.edn only once."
  (interactive)
  (unless gnaw-list--flagged
    (user-error "No report flagged for dismissal"))
  (gnaw-list--follow-off)
  (let ((mid (car (tabulated-list-get-id)))
        (line (line-number-at-pos))
        (state (gnaw--read-state-for-update))
        (count 0))
    (dolist (fmid gnaw-list--flagged)
      (let ((pair (assoc fmid gnaw-list--reports)))
        (when (and pair (not (gnaw-action-on-p state fmid :dismiss)))
          (setq state (gnaw--apply-transition state :dismiss fmid (cdr pair)))
          (setq count (1+ count)))))
    (gnaw-write-state state)
    (setq-local gnaw-list--flagged nil)
    (gnaw-list-refresh)
    (gnaw-list--restore-point mid line)
    (message "gnaw: dismissed %d report(s) (type _ to include dismissed reports to the view)"
             count)))

(defun gnaw-list-remove-marks ()
  "Remove the mark or dismissal flag from the report at point, then refresh.
Keep the cursor in place (see `gnaw-list--restore-point')."
  (interactive)
  (let ((p (gnaw-list--current))
        (line (line-number-at-pos)))
    (cond
     ((member (car p) gnaw-list--flagged)
      (setq-local gnaw-list--flagged (delete (car p) gnaw-list--flagged))
      (gnaw-list--set-mark-cell (car p))
      (gnaw-list--update-mode-line))
     ((gnaw-remove-marks (car p))
      (gnaw-list-refresh)
      (gnaw-list--restore-point (car p) line))
     (t (message "gnaw: no mark on this report")))))

(defun gnaw-list-toggle-dismissed ()
  "Toggle whether dismissed reports are shown."
  (interactive)
  (setq-local gnaw-list--related-mids nil) ; the toggle leaves the related view
  (setq-local gnaw-list--show-dismissed (not gnaw-list--show-dismissed))
  (gnaw-list-refresh)
  (message "gnaw: dismissed reports %s"
           (if gnaw-list--show-dismissed "shown" "hidden")))

(defun gnaw-list--sort (column default-descending flip)
  "Sort the report list by COLUMN, a header string in `gnaw-list-columns'.
DEFAULT-DESCENDING is the natural direction for that column; a non-nil
FLIP (typically the raw prefix argument) reverses it.  Refresh after."
  (unless (derived-mode-p 'gnaw-list-mode)
    (user-error "Not in a gnaw report list"))
  (unless (assoc column (gnaw--active-columns))
    (user-error "The %s column is not currently shown" column))
  (setq-local tabulated-list-sort-key
              (cons column (if flip (not default-descending) default-descending)))
  (gnaw-list-refresh)
  (message "gnaw: sorted by %s (%s)" column
           (if (cdr tabulated-list-sort-key) "descending" "ascending")))

(defun gnaw-sort-by-date (&optional flip)
  "Sort the report list by creation date, most recent first.
With a prefix argument FLIP, sort oldest first."
  (interactive "P")
  (gnaw-list--sort "Created" t flip))

(defun gnaw-sort-by-activity (&optional flip)
  "Sort the report list by last activity, most recent first.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Activity" t flip))

(defun gnaw-sort-by-author (&optional flip)
  "Sort the report list by author name, A to Z.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "From" nil flip))

(defun gnaw-sort-by-subject (&optional flip)
  "Sort the report list by subject, A to Z.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Subject" nil flip))

(defun gnaw-sort-by-type (&optional flip)
  "Sort the report list by report type.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Type" nil flip))

(defun gnaw-sort-by-topic (&optional flip)
  "Sort the report list by topic, A to Z.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Topic" nil flip))

(defun gnaw-sort-by-priority (&optional flip)
  "Sort the report list by priority, highest first.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Pri" t flip))

(defun gnaw-sort-by-votes (&optional flip)
  "Sort the report list by vote count, highest first.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Votes" t flip))

(defun gnaw-sort-by-msgs (&optional flip)
  "Sort the report list by thread size, largest first.
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Msgs" t flip))

(defun gnaw-sort-by-mark (&optional flip)
  "Sort the report list by local mark (sticky, then normal, then dismissed).
With a prefix argument FLIP, reverse the order."
  (interactive "P")
  (gnaw-list--sort "Mark" nil flip))

(defconst gnaw-sort-commands
  '(("date"     "Created"  gnaw-sort-by-date)
    ("activity" "Activity" gnaw-sort-by-activity)
    ("author"   "From"     gnaw-sort-by-author)
    ("subject"  "Subject"  gnaw-sort-by-subject)
    ("type"     "Type"     gnaw-sort-by-type)
    ("topic"    "Topic"    gnaw-sort-by-topic)
    ("priority" "Pri"      gnaw-sort-by-priority)
    ("votes"    "Votes"    gnaw-sort-by-votes)
    ("msgs"     "Msgs"     gnaw-sort-by-msgs)
    ("mark"     "Mark"     gnaw-sort-by-mark))
  "(NAME COLUMN COMMAND) entries for `gnaw-sort', COLUMN a header name.")

(defun gnaw-sort ()
  "Sort the report list by a criterion chosen interactively.
Only criteria whose column is currently shown are offered; a prefix
argument is forwarded to the sort command to reverse its natural
order."
  (interactive)
  (let* ((cols (mapcar #'car (gnaw--active-columns)))
         (cands (cl-remove-if-not (lambda (e) (member (nth 1 e) cols))
                                  gnaw-sort-commands))
         (name (completing-read "Sort by: " cands nil t)))
    (call-interactively (nth 2 (assoc name cands)))))

(defconst gnaw-list--help-sections
  '(("Read\n————"
     (gnaw-list-open "open the report's mail")
     (gnaw-list-open-other-window "toggle the mail below the list")
     (gnaw-list-browse "browse the report's archived web page")
     (gnaw-list-follow-mode "toggle follow mode (auto-show the mail at point)"))
    ("Marks\n—————"
     (gnaw-list-toggle-sticky "toggle the sticky mark (bold, exported to todo.org)")
     (gnaw-list-flag-dismiss "flag for dismissal (D in the Mark column)")
     (gnaw-list-execute-flags "dismiss the flagged reports")
     (gnaw-list-toggle-dismiss "dismiss immediately (toggle)")
     (gnaw-list-remove-marks "remove the mark or flag at point")
     (gnaw-list-toggle-dismissed "show / hide dismissed reports"))
    ("Filter and sort\n———————————————"
     (gnaw-list-filter "filter with key:value tokens (C-u: clear the filter)")
     (gnaw-list-filter-transient "filter by one field (menu)")
     (gnaw-select-preset-filter "apply a preset filter")
     (gnaw-list-limit-type "limit to a report type")
     (gnaw-list-filter-topic "filter by a topic, with completion")
     (gnaw-list-filter-acked "only the acked reports")
     (gnaw-list-filter-owned "only the owned reports")
     (gnaw-list-limit-closed "only the closed reports (canceled, resolved...)")
     (gnaw-list-limit-awaiting "only the reports awaiting a reply")
     (gnaw-list-limit-related "only the reports with related reports")
     (gnaw-list-limit-attachments "only the reports with attachments")
     (gnaw-list-filter-cell "toggle a filter on the cell at point (author, type, date, subject)")
     "C-u on the keys above (also in the = menu) adds the condition"
     "to the active filter (AND)"
     (tabulated-list-sort "sort by the column at point")
     (gnaw-sort "sort by a criterion"))
    ("Patches and attachments\n———————————————————————"
     (gnaw-list-tab "fold / unfold the series, or narrow to related reports")
     (gnaw-list-quit "leave the related view, clear the filter, or quit the window")
     (gnaw-list-attachment-view "view an attachment (patches in diff-mode)")
     (gnaw-list-attachment-save "save an attachment (proposes the configured repo)")
     (gnaw-list-patch-apply "apply the patches with git apply (C-u: git am)")
     (gnaw-list-attachments "menu acting on patches and attachments"))
    ("Refresh\n———————"
     (gnaw-list-reload "re-read the local cache")
     (gnaw-list-update "refresh the remote cache, then reload"))
    ("Help\n————"
     (gnaw-show-help "this help")
     (describe-mode "full mode description")))
  "Sections shown by `gnaw-show-help': (TITLE (COMMAND DESCRIPTION)...).
A plain string among the entries is printed as a note line.")

(defun gnaw-show-help ()
  "List the report list's key bindings, grouped by theme."
  (interactive)
  (with-help-window "*gnaw help*"
    (princ "gnaw report list\n")
    (dolist (section gnaw-list--help-sections)
      (princ (format "\n%s\n" (car section)))
      (dolist (cmd (cdr section))
        (if (stringp cmd)
            (princ (format "  %s\n" cmd))
          ;; FIRSTONLY: commands unbound in the list map (describe-mode)
          ;; would otherwise list every global binding, menus included.
          (let ((key (where-is-internal (car cmd) gnaw-list-mode-map t)))
            (princ (format "  %-12s %s\n"
                           (if key (key-description key) "M-x")
                           (cadr cmd)))))))))

(defun gnaw-select-preset-filter ()
  "Apply a filter chosen from `gnaw-preset-filters'.
Each preset is a query string in the syntax of `gnaw-list-filter'."
  (interactive)
  (unless gnaw-preset-filters
    (user-error "No preset filters defined; customize `gnaw-preset-filters'"))
  (gnaw-list-filter
   (completing-read "Preset filter: " gnaw-preset-filters nil t)))

(defun gnaw--resolve-reports-dir (input)
  "Return the reports directory URL (ending in /) for user INPUT.
A `.json' (including `meta.json') URL yields its directory; a URL
ending in /reports yields that directory; otherwise /reports/ is
appended."
  (let ((u (replace-regexp-in-string "/+\\'" "" (string-trim input))))
    (cond ((string-suffix-p ".json" u) (file-name-directory u))
          ((string-suffix-p "/reports" u) (concat u "/"))
          (t (concat u "/reports/")))))

(defun gnaw--read-config-raw ()
  "Return config.edn's raw alist for a read-modify-write cycle, or nil.
Signal a `user-error' when the file exists but cannot be parsed, since
rewriting a misread config would drop its other settings."
  (gnaw--read-edn-map-or-signal
   (expand-file-name "config.edn" gnaw-config-dir)))

(defun gnaw--config-add-source (config urls name repo)
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

(defun gnaw--write-config (config)
  "Write CONFIG alist to config.edn as UTF-8."
  (let ((file (expand-file-name "config.edn" gnaw-config-dir)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert (gnaw--edn-write config) "\n")))
    (setq gnaw--config-cache nil)))

(declare-function completing-read-multiple "crm")

;;;###autoload
(defun gnaw-add-source (urls &optional name repo)
  "Add a source (URLS, NAME, REPO) to config.edn.
URLS is a list of reports.json URLs.  Interactively, give a base,
meta.json or reports.json URL, then pick the report files listed in
meta.json, the source NAME and its local git REPO for patches."
  (interactive
   (let* ((input (read-string "Report source URL (base, meta.json or .json): "))
          (_ (when (string-empty-p (string-trim input)) (user-error "No URL given")))
          (dir (gnaw--resolve-reports-dir input))
          (meta (ignore-errors (gnaw-source-meta dir)))
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
          (repo (when (y-or-n-p "Link a local git repo (for patches)? ")
                  (expand-file-name (read-directory-name "Git repo: ")))))
     (list (mapcar (lambda (f) (concat dir f)) chosen) name repo)))
  (let ((urls (if (listp urls) urls (list urls)))
        (name (and name (not (string-empty-p (string-trim name))) name)))
    (unless urls (user-error "No report file selected"))
    (gnaw--write-config (gnaw--config-add-source (gnaw--read-config-raw) urls name repo))
    (message "gnaw: added to config.edn: %s%s"
             (string-join urls ", ") (if name (format " (%s)" name) ""))
    urls))

(defun gnaw--source-name-for-urls (urls)
  "Return the configured source name whose URL list intersects URLS."
  (let* ((urls (if (listp urls) urls (list urls)))
         (entry (seq-find
                 (lambda (s) (seq-intersection urls (alist-get :urls s)))
                 (plist-get (gnaw-load-config) :source-configs))))
    (alist-get :name entry)))

(defun gnaw--configure-email-client-for-source (source)
  "Read and save the message-opening setup for SOURCE.
SOURCE may be a source name string or t for the global default."
  (let* ((method (gnaw--read-open-message-method))
         (group (when (eq method 'gnus)
                  (gnaw--read-gnus-group "Gnus group (empty = ask each time): "))))
    (gnaw-configure-email-client source method group)))

(defun gnaw--configure-one-source ()
  "Add one source, then configure how its messages are opened."
  (let* ((urls (call-interactively #'gnaw-add-source))
         (source (or (gnaw--source-name-for-urls urls) t)))
    (gnaw--configure-email-client-for-source source)
    urls))

;;;###autoload
(defun gnaw-configure ()
  "Run interactive gnaw setup.
Configure a report source, its local patch repository and the mail
client used to open report messages; with existing sources, optionally
only the mail client."
  (interactive)
  (let ((added-source nil))
    (if (gnaw-sources)
        (if (y-or-n-p "Add or update a report source? ")
            (progn
              (gnaw--configure-one-source)
              (setq added-source t))
          (call-interactively #'gnaw-configure-email-client))
      (gnaw--configure-one-source)
      (setq added-source t))
    (when added-source
      (while (y-or-n-p "Add another source? ")
        (gnaw--configure-one-source)))))

;;;###autoload
(defun gnaw ()
  "Browse open BONE reports in a tabulated list filling the frame.
Prompt for full interactive configuration when no source is configured."
  (interactive)
  (unless (gnaw-sources)
    (gnaw-configure))
  (let ((buf (get-buffer-create "*gnaw*")))
    (switch-to-buffer buf)
    (delete-other-windows)
    (gnaw-list-mode)
    (gnaw-list-reload)
    (message "gnaw some bone! \"f\" to toggle the follow mode and \"h\" to get help")))

(provide 'gnaw)
;;; gnaw.el ends here
