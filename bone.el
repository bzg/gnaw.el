;;; bone.el --- Core library for BARK/bone in Emacs -*- lexical-binding: t; -*-

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
;; Shared core for the Emacs BARK/bone front-ends (gnus-bone, notmuch-bone,
;; mu4e-bone).  This library owns the data layer:
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

(defcustom bone-reports-source nil
  "Path or URL to a BARK reports.json file.
If nil, load sources configured in config.edn under `bone-config-dir'."
  :type '(choice (const :tag "Use config.edn sources" nil)
                 (string :tag "Local path or URL"))
  :group 'bone)

(defcustom bone-after-update-hook nil
  "Functions run after `bone-update' refreshes the local cache.
Front-ends can use this to re-apply their display."
  :type 'hook
  :group 'bone)

(defvar bone-addresses nil
  "List of user email addresses loaded from config.")

(defconst bone-supported-bark-format "0.9.1"
  "Minimum supported BONE reports.json bark-format.")

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

(defun bone-edn--write-string (s)
  "Format string S as an EDN string."
  (format "%S" s))

(defun bone-edn--write-value (v)
  "Serialize EDN value V to a string."
  (cond
   ((stringp v)  (bone-edn--write-string v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((consp v)    (bone-edn--write-entry v))
   (t (error "EDN: cannot serialize %S" v))))

(defun bone-edn--write-entry (entry)
  "Format ENTRY as an EDN map."
  (if (null entry) "{}"
    (concat "{"
            (mapconcat (lambda (kv)
                         (concat (bone-edn--write-value (car kv))
                                 " "
                                 (bone-edn--write-value (cdr kv))))
                       entry ", ")
            "}")))

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
  "Load `config.edn' and return a plist (:addresses :sources)."
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
         (addresses (alist-get :addresses cfg))
         (sources   (delq nil (mapcar (lambda (s) (alist-get :url s))
                                      (alist-get :sources cfg)))))
    (setq bone-addresses addresses)
    (list :addresses addresses :sources sources)))

(defun bone-sources ()
  "Return report sources as URLs or absolute local paths.
Relative paths are resolved against `bone-config-dir'."
  (mapcar #'bone--resolve-source
          (if bone-reports-source
              (list bone-reports-source)
            (plist-get (bone-load-config) :sources))))

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
                (json-array-type 'list))
            (json-read)))
      (kill-buffer buf))))

(defun bone--write-json-to-file (data file)
  "Write JSON DATA to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (json-encode data))))

(defun bone--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
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
  (if (string-match-p "^<.*>$" mid)
      mid
    (concat "<" mid ">")))

(defun bone--extract-open-reports (source)
  "Extract open reports from SOURCE as (MID . INFO) pairs."
  (let* ((data (bone--read-json source))
         (fv (alist-get 'bark-format data))
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
            (date         (alist-get 'date r)))
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
                                       :date date))
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
  "Write STATE to the state file."
  (let ((file (expand-file-name "state.edn" bone-config-dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{")
        (let ((first t))
          (dolist (kv state)
            (if first (setq first nil) (insert "\n "))
            (insert (bone-edn--write-string (car kv)))
            (insert " ")
            (insert (bone-edn--write-entry (cdr kv)))))
        (insert "}\n")))))

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

(provide 'bone)
;;; bone.el ends here
