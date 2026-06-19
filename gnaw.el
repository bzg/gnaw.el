;;; gnaw.el --- Browse and manage BONE reports in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail, news
;; URL: https://codeberg.org/bzg/gnaw.el
;; Version: 0.3.0
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
;; :priority :votes :deadline :expiry :last-activity :topic) and the
;; presentation; this library provides everything that is independent of
;; the mail user agent.
;;
;; Entry points:
;;
;;   `gnaw'              browse reports (interactive)
;;   `gnaw-reports'      collect open reports from all sources
;;   `gnaw-update'       force-refresh the local cache (interactive)
;;   `gnaw-toggle-mark'  toggle :sticky/:skip for a message-id
;;   `gnaw-read-state' / `gnaw-write-state'   state.edn I/O
;;   `gnaw-annotation'   fixed-width report annotation for MUA lines
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'transient)

(defvar url-http-response-status)

(defgroup gnaw nil
  "Read and manage BONE reports shared with the gnaw CLI."
  :group 'mail)

(defcustom gnaw-config-dir "~/.config/gnaw"
  "Directory containing gnaw configuration and state/cache files."
  :type 'directory
  :group 'gnaw)

(defcustom gnaw-after-update-hook nil
  "Functions run after `gnaw-update' refreshes the local cache.
Front-ends can use this to re-apply their display."
  :type 'hook
  :group 'gnaw)

(defvar gnaw-addresses nil
  "List of user email addresses loaded from config.")

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
  "Read an EDN symbol at point."
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
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

;;; Configuration and report sources

(defun gnaw--uri-to-path (uri)
  "Convert file:// URI to local path, otherwise return URI."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defvar gnaw--config-cache nil
  "Cons (MTIME . PLIST) caching the last `gnaw-load-config' result.
Invalidated when config.edn's modification time changes or when
`gnaw--write-config' rewrites it.")

(defun gnaw-load-config ()
  "Load `config.edn' and return a plist.
Keys: :addresses :skip-columns :source-configs (raw `:sources' maps)
and :sources (their URLs).  The result is cached and reused while
config.edn is unchanged, since a single list refresh queries it
several times."
  (let* ((file (expand-file-name "config.edn" gnaw-config-dir))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (if (and gnaw--config-cache (equal (car gnaw--config-cache) mtime))
        (cdr gnaw--config-cache)
      (let* ((cfg (when (file-readable-p file)
                    (condition-case err
                        (with-temp-buffer
                          (insert-file-contents file)
                          (gnaw-edn-read-buffer))
                      (error
                       (message "gnaw: cannot parse %s: %s"
                                file (error-message-string err))
                       nil))))
             (addresses (alist-get :my-addresses cfg))
             (skip      (alist-get :skip-columns cfg))
             (src-cfgs  (alist-get :sources cfg))
             (sources   (mapcan (lambda (s) (append (alist-get :urls s) nil)) src-cfgs))
             (plist (list :addresses addresses :skip-columns skip
                          :source-configs src-cfgs :sources sources)))
        (setq gnaw-addresses addresses)
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

(defun gnaw--source-repo (info)
  "Return the local git repo for report INFO's source, or nil.
Reads `:repo' from the matching config.edn `:sources' entry, matched by
URL or by `:name'."
  (let* ((url  (plist-get info :source))
         (name (plist-get info :source-name))
         (entry (seq-find
                 (lambda (s)
                   (or (and url (member url (mapcar #'gnaw--resolve-source
                                                    (alist-get :urls s))))
                       (and name (equal name (alist-get :name s)))))
                 (plist-get (gnaw-load-config) :source-configs))))
    (when-let* ((repo (alist-get :repo entry)))
      (expand-file-name repo))))

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

(defun gnaw--http-body (url &optional binary)
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

(defun gnaw--fetch-json-from-url (url)
  "Synchronously fetch JSON from URL.
The body is read as raw bytes and decoded as UTF-8."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil))
    (json-read-from-string
     (decode-coding-string (gnaw--http-body url t) 'utf-8))))

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

(defun gnaw--extract-open-reports (source)
  "Extract open reports from SOURCE as (MID . INFO) pairs."
  (let* ((data (gnaw--read-json source))
         (fv (alist-get 'bone-format data))
         (sname (alist-get 'source data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv gnaw-supported-bone-format))
      (message "gnaw: %s has format %s, min supported is %s"
               source fv gnaw-supported-bone-format))
    (dolist (r reports result)
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
                (norm-mid (gnaw-normalize-mid mid)))
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
                                       :superseded-by superseded-by
                                       :acked acked
                                       :owned owned
                                       :owned-name owned-name
                                       :closed closed))
                  result)))))))

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
  (let ((file (expand-file-name "state.edn" gnaw-config-dir)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (gnaw-edn-read-buffer))
        (error
         (message "gnaw: cannot parse %s: %s"
                  file (error-message-string err))
         nil)))))

(defun gnaw-write-state (state)
  "Write STATE to the state file as UTF-8, one entry per line."
  (let ((file (expand-file-name "state.edn" gnaw-config-dir))
        (coding-system-for-write 'utf-8))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{"
                (mapconcat (lambda (kv)
                             (concat (gnaw--edn-write (car kv)) " "
                                     (gnaw--edn-write (cdr kv))))
                           state "\n ")
                "}\n")))))

;;; Local marks (sticky / skip)

(defun gnaw--iso-now ()
  "Return the current time as an ISO-8601 UTC string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun gnaw--author-string (info)
  "Build author string from INFO."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string= n ""))) (concat n " <" e ">"))
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

(defun gnaw--apply-transition (state action mid info)
  "Apply ACTION (:sticky or :skip) for MID in STATE using metadata INFO.
The two marks are mutually exclusive: :sticky clears a skip and :skip
clears a flag; re-applying the active mark returns to neutral.  :skip is
stored as `:skip-since' (the gnaw CLI hides such reports by default)."
  (let* ((base (gnaw--enrich-entry (cdr (assoc mid state)) info))
         (sticky (eq (alist-get :flag base) :sticky))
         (skip (and (alist-get :skip-since base) t))
         (new
          (pcase action
            (:sticky (if sticky
                         (gnaw--alist-dissoc base :flag)
                       (gnaw--alist-assoc (gnaw--alist-dissoc base :skip-since)
                                          :flag :sticky)))
            (:skip (if skip
                       (gnaw--alist-dissoc base :skip-since)
                     (gnaw--alist-assoc (gnaw--alist-dissoc base :flag)
                                        :skip-since (gnaw--iso-now)))))))
    (if (and (null (alist-get :flag    new))
             (null (alist-get :skip-since new)))
        (gnaw--state-delete state mid)
      (gnaw--state-put state mid new))))

(defun gnaw-action-on-p (state mid action)
  "Return non-nil if ACTION (:sticky or :skip) is set for MID in STATE."
  (let ((entry (cdr (assoc mid state))))
    (pcase action
      (:sticky (eq (cdr (assq :flag entry)) :sticky))
      (:skip (and (cdr (assq :skip-since entry)) t)))))

(defun gnaw-toggle-mark (mid info action)
  "Toggle ACTION (:sticky or :skip) for MID using metadata INFO.
Persist the new state and return non-nil if ACTION is now on."
  (let* ((state (gnaw-read-state))
         (new   (gnaw--apply-transition state action mid info)))
    (gnaw-write-state new)
    (gnaw-action-on-p new mid action)))

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
  "Return the mark character for state ENTRY: `*' sticky, `_' skip."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond ((eq flag :sticky) "*")
          (skip              "_")
          (t                 " "))))

(defun gnaw-days-until (date)
  "Days from now until YYYY-MM-DD DATE, or nil when DATE is nil or invalid.
A malformed DATE returns nil rather than signaling, since this feeds the
per-line annotation rendered by the MUA front-ends."
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
  "Return the sorted list of topics in REPORTS ((MID . INFO) pairs)."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort topics #'string<)))

(defun gnaw-filter-by-topic (reports topic)
  "Return the REPORTS pairs whose info matches TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;; Source metadata (meta.json) and patch files

(defun gnaw--fetch-url-to-file (url file)
  "Fetch URL synchronously and write its raw body bytes to FILE."
  (let ((body (gnaw--http-body url t)))
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

(defun gnaw--patches-base (source)
  "Return the patches base URL or directory for reports SOURCE."
  (concat (file-name-directory (directory-file-name (file-name-directory source)))
          "patches/"))

(defun gnaw--sanitize-path-component (s)
  "Return S reduced to a single safe path component, or nil.
Strips any directory part and rejects empty or `.'/`..' names, so an
attacker-supplied value from a remote reports.json cannot escape the
cache directory via path traversal."
  (let ((base (and s (file-name-nondirectory s))))
    (and base (not (member base '("" "." ".."))) base)))

(defun gnaw-patch-file (info patch)
  "Return a local file for PATCH of report INFO, fetching it if absent.
PATCH is an entry of the report `:patches' list."
  (let* ((file   (gnaw--sanitize-path-component (alist-get 'file patch)))
         (source (plist-get info :source))
         (sname  (or (gnaw--sanitize-path-component (plist-get info :source-name))
                     "unknown"))
         (cache  (and file
                      (expand-file-name (concat "cache/patches/" sname "/" file)
                                        gnaw-config-dir))))
    (unless file
      (user-error "Patch entry has no usable file name"))
    (unless (file-exists-p cache)
      (let ((loc (concat (gnaw--patches-base source) file)))
        (cond ((gnaw--http-url-p source) (gnaw--fetch-url-to-file loc cache))
              ((file-exists-p loc)
               (make-directory (file-name-directory cache) t)
               (copy-file loc cache t)))))
    (and (file-exists-p cache) cache)))

;;; Reading the report message

(defcustom gnaw-open-message-method '((t . auto))
  "How `gnaw-read-message' opens a report's email, per source.
An alist mapping a source name to a method; the entry keyed by t is the
default for sources not listed.  A source name is BONE's source, the
`:name' of a `:sources' entry in config.edn.  Methods: `auto' uses
`gnaw-open-message-function' if set, else the web archive; `mua' forces
that function; `gnus', `notmuch' and `mu4e' open in that MUA by
message-id (Gnus also reads `gnaw-gnus-group'); `web' forces the web
archive."
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
Used by the `gnus' open method.  For a source not listed, the group is
asked for with completion, falling back to the Gnus registry."
  :type '(alist :key-type (string :tag "Source name")
                :value-type (string :tag "Gnus group"))
  :group 'gnaw)

(defvar gnaw-open-message-function nil
  "Function (MID INFO) a front-end sets to open a message in its MUA.")

(defun gnaw--method-for (info)
  "Return the open method for report INFO's source."
  (let* ((m gnaw-open-message-method)
         (name (plist-get info :source-name))
         (entry (or (and name (assoc name m)) (assq t m))))
    (if entry (cdr entry) 'auto)))

(defun gnaw--gnus-group-for (info)
  "Return the configured Gnus group for report INFO's source, or nil.
`gnaw-gnus-group' is an alist mapping a source name to its group."
  (let ((name (plist-get info :source-name)))
    (and name (cdr (assoc name gnaw-gnus-group)))))

(defvar gnaw--message-archive-url nil
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
    (let ((raw (gnaw--http-body (concat url "/raw") t)))
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
Enter the group reading a single article (avoiding a full fetch of a
large group), then select MID by its message-id; return to the calling
buffer on summary exit."
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

(defun gnaw--known-source-names ()
  "Return known source names from each source's meta.json."
  (delete-dups
   (delq nil (mapcar (lambda (s) (alist-get 'source (gnaw-source-meta s)))
                     (gnaw-sources)))))

;;;###autoload
(defun gnaw-set-source-open-method (source method &optional group)
  "Set the open METHOD for SOURCE (a source name, or t for the default).
For Gnus, also store GROUP.  Interactively, complete SOURCE, METHOD and
\(for Gnus) GROUP, then save with Customize."
  (interactive
   (let* ((s (completing-read "Source name (empty = default): "
                              (gnaw--known-source-names) nil nil))
          (src (if (string-empty-p s) t s))
          (m (intern (completing-read
                      "Open with: "
                      '("auto" "mua" "gnus" "notmuch" "mu4e" "web") nil t)))
          (g (when (eq m 'gnus)
               (gnaw--read-gnus-group "Gnus group (empty = ask each time): "))))
     (list src m g)))
  (customize-save-variable 'gnaw-open-message-method
                           (gnaw--alist-put gnaw-open-message-method source method))
  (when (and (eq method 'gnus) group (not (string-empty-p group)))
    (customize-save-variable 'gnaw-gnus-group
                             (gnaw--alist-put gnaw-gnus-group source group)))
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
A source's `:repo' in config.edn takes precedence; this is used only
when the source has none, before asking interactively."
  :type '(choice (const :tag "Ask each time" nil) directory)
  :group 'gnaw)

(defcustom gnaw-git-apply-options '("--3way")
  "Extra arguments passed to `git apply' by `gnaw-apply-patches'.
The default `--3way' falls back to a 3-way merge when a patch does not
apply cleanly (which may leave conflict markers to resolve) instead of
failing outright.  Another useful value is \"--whitespace=fix\"."
  :type '(repeat string)
  :group 'gnaw)

(defcustom gnaw-git-am-options '("--3way")
  "Extra arguments passed to `git am' by `gnaw-am-patches'.
The default `--3way' falls back to a 3-way merge when a patch does not
apply cleanly (leaving conflict markers to resolve) instead of failing
outright.  Other useful values include \"--signoff\" or \"--whitespace=fix\"."
  :type '(repeat string)
  :group 'gnaw)

(defcustom gnaw-checkout-base 'ask
  "Whether to check out a patch's recorded base commit before applying.
Patches made with `git format-patch --base' carry a `base-commit:'
trailer.  When that commit exists in the target repo, gnaw can check it
out first so the patch applies against its intended state.  Values: `ask'
prompts, nil never checks out, t checks out without prompting."
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

(defun gnaw--series-summary (members)
  "Summarize series MEMBERS (INFO plists) like \"2 acked, 1 open\".
Cover letters are excluded from the tally."
  (let ((acked 0) (closed 0) (open 0))
    (dolist (m members)
      (unless (gnaw--cover-p m)
        (let ((f (or (plist-get m :flags) "---")))
          (cond ((and (>= (length f) 3) (not (eq (aref f 2) ?-))) (cl-incf closed))
                ((and (>= (length f) 1) (not (eq (aref f 0) ?-))) (cl-incf acked))
                (t (cl-incf open))))))
    (string-join (delq nil (list (and (> acked 0) (format "%d acked" acked))
                                 (and (> closed 0) (format "%d closed" closed))
                                 (and (> open 0) (format "%d open" open))))
                 ", ")))

(defun gnaw-view-patches (info)
  "Show the patches of report INFO in a `diff-mode' buffer."
  (let ((patches (plist-get info :patches)))
    (unless patches (user-error "This report has no patches"))
    (with-current-buffer (get-buffer-create "*gnaw-patches*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (p patches)
          (let ((f (gnaw-patch-file info p)))
            (when f
              (insert-file-contents f)
              (goto-char (point-max)))))
        (goto-char (point-min)))
      (diff-mode)
      (pop-to-buffer (current-buffer)))))

(defun gnaw--patch-base-commit (files)
  "Return the `base-commit' trailer found in patch FILES, or nil.
Scans FILES in order and returns the first commit hash recorded by
`git format-patch --base'."
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
Runs in `default-directory' (the target repo).  Does nothing unless
`gnaw-checkout-base' is set and the base commit exists locally.  Signals
a `user-error' if the checkout fails.  Note that this leaves the repo on
a detached HEAD."
  (when gnaw-checkout-base
    (let ((base (gnaw--patch-base-commit files)))
      (when (and base
                 (zerop (call-process "git" nil nil nil
                                      "cat-file" "-e" (concat base "^{commit}")))
                 (or (eq gnaw-checkout-base t)
                     (y-or-n-p
                      (format "Check out base commit %s (detached HEAD) first? "
                              (substring base 0 (min 12 (length base)))))))
        (with-current-buffer (get-buffer-create "*gnaw-git*")
          (let ((inhibit-read-only t)) (erase-buffer))
          (unless (zerop (call-process "git" nil t nil "checkout" base))
            (display-buffer (current-buffer))
            (user-error "Git checkout %s failed" base)))))))

(defun gnaw--run-git-patches (info subcommand options hint)
  "Apply INFO's patches in its repo via git SUBCOMMAND (\"apply\" or \"am\").
OPTIONS is a list of extra arguments inserted before the patch files.
HINT is shown on failure."
  (let ((patches (plist-get info :patches)))
    (unless patches (user-error "This report has no patches"))
    (when (and (not (gnaw--series-complete-p info))
               (not (yes-or-no-p "Patch series looks incomplete; apply anyway? ")))
      (user-error "Aborted"))
    (let* ((repo (or (gnaw--source-repo info)
                     gnaw-apply-repo
                     (read-directory-name "Apply in git repo: ")))
           (files (mapcar (lambda (p) (gnaw-patch-file info p)) patches))
           (default-directory (file-name-as-directory repo)))
      (when (memq nil files)
        (user-error "Cannot fetch %d of %d patch file(s); not applying"
                    (seq-count #'null files) (length files)))
      (gnaw--maybe-checkout-base files)
      (let ((args (append (list subcommand) options (list "--") files)))
        (with-current-buffer (get-buffer-create "*gnaw-git*")
          (let ((inhibit-read-only t)) (erase-buffer))
          (let ((status (apply #'call-process "git" nil t nil args)))
            (if (zerop status)
                (message "gnaw: git %s applied %d patch(es) in %s"
                         subcommand (length files) repo)
              (display-buffer (current-buffer))
              (message "gnaw: git %s failed in %s (%s)" subcommand repo hint))))))))

(defun gnaw-apply-patches (info)
  "Apply INFO's patches to the working tree with `git apply'."
  (gnaw--run-git-patches info "apply" gnaw-git-apply-options
                         "rejects left as .rej"))

(defun gnaw-am-patches (info)
  "Apply INFO's patches as commits with `git am'."
  (gnaw--run-git-patches info "am" gnaw-git-am-options
                         "run `git am --abort' to undo"))

;;; Query filter (subset of the BONE web search syntax)

(defvar gnaw-list--query nil
  "Active `gnaw-list' filter query string, or nil.  Buffer-local in use.")

(defvar gnaw-list--expanded nil
  "Series ids unfolded in the current `gnaw-list' buffer.  Buffer-local.")

(defvar gnaw-list--show-skipped nil
  "When non-nil, show reports marked skipped.  Buffer-local in `gnaw-list'.")

(defvar gnaw-list--reports nil
  "Cached (MID . INFO) report pairs for the current `gnaw-list' buffer.
Set by `gnaw-list-reload'; re-rendering reuses it without re-reading
the cache.  Buffer-local in use.")

(defvar gnaw-list--mark-index 0
  "Index of the Mark column among the active columns.
Set by `gnaw--list-format'; used by `gnaw--mark-sort'.  Buffer-local in use.")

(defface gnaw-sticky '((t :weight bold))
  "Face for the sticky mark in the report list."
  :group 'gnaw)

(defun gnaw--query-text-match (needle hay)
  "Non-nil if HAY contains NEEDLE, case-insensitively.
NEEDLE `*' matches any non-empty HAY; empty NEEDLE matches anything."
  (let ((hay (or hay "")))
    (cond ((equal needle "*") (not (string-empty-p hay)))
          ((or (null needle) (string-empty-p needle)) t)
          (t (let ((case-fold-search t))
               (and (string-match-p (regexp-quote needle) hay) t))))))

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

(defun gnaw--query-date-match (spec field forward)
  "Non-nil if FIELD's date satisfies SPEC; FORWARD looks ahead from today.
SPEC is a duration (3d/2w/2m), a YYYY-MM-DD date, or an A..B range whose
ends are dates or durations and may be empty (the order is normalized)."
  (let ((fd (gnaw--query-ymd->days field))
        (today (time-to-days (current-time))))
    (when fd
      (if (string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" spec)
          ;; Read both ends before calling `gnaw--query-bound', which
          ;; clobbers the match data via its own `string-match'.
          (let* ((sa (match-string 1 spec))
                 (sb (match-string 2 spec))
                 (va (gnaw--query-bound sa forward today))
                 (vb (gnaw--query-bound sb forward today))
                 (lo (if (and va vb) (min va vb) va))
                 (hi (if (and va vb) (max va vb) vb)))
            (and (or (null lo) (>= fd lo)) (or (null hi) (<= fd hi))))
        (let ((d (gnaw--query-duration->days spec))
              (single (gnaw--query-ymd->days spec)))
          (cond (d (if forward (and (>= fd today) (<= fd (+ today d)))
                     (and (<= fd today) (>= fd (- today d)))))
                (single (= fd single))))))))

(defun gnaw--query-actor-match (needle &rest actors)
  "Non-nil if NEEDLE matches one of ACTORS (identity strings).
`*', `true' or empty NEEDLE matches when any actor is set; otherwise a
case-insensitive substring of an actor matches."
  (let ((set (seq-some (lambda (a) (and a (not (string-empty-p a)))) actors)))
    (if (member needle '("*" "true" nil ""))
        (and set t)
      (let ((case-fold-search t))
        (and (seq-some (lambda (a)
                         (and a (string-match-p (regexp-quote needle) a)))
                       actors)
             t)))))

(defun gnaw--query-val-any (val pred)
  "Non-nil if PRED holds for any comma-separated value in VAL.
Commas inside a field value are OR, as in the BONE web UI
\(e.g. `acked:alice,bob').  An empty VAL is passed through unsplit so it
keeps its \"match any\" meaning."
  (seq-some pred (if (or (null val) (string-empty-p val))
                     (list val)
                   (split-string val "," t))))

(defun gnaw--query-token-match (token mid info)
  "Non-nil if search TOKEN matches report MID with INFO."
  (let* ((i (string-search ":" token))
         (key (and i (substring token 0 i)))
         (val (and i (substring token (1+ i))))
         (prio (or (plist-get info :priority) 0)))
    (if (null key)
        (gnaw--query-text-match token (plist-get info :subject))
      (pcase key
        ((or "from" "f")
         (gnaw--query-val-any
          val (lambda (v)
                (or (gnaw--query-text-match v (plist-get info :from))
                    (gnaw--query-text-match v (plist-get info :from-name))))))
        ((or "subject" "s")
         (gnaw--query-val-any
          val (lambda (v) (gnaw--query-text-match v (plist-get info :subject)))))
        ((or "topic" "t")
         (gnaw--query-val-any
          val (lambda (v) (gnaw--query-text-match v (plist-get info :topic)))))
        ("type"
         (gnaw--query-val-any
          val (lambda (v)
                (and v (equal (downcase v)
                              (downcase (or (plist-get info :type) "")))))))
        ((or "priority" "p")
         (gnaw--query-val-any
          val (lambda (v) (equal v (number-to-string prio)))))
        ((or "mid" "m")
         (gnaw--query-val-any val (lambda (v) (gnaw--query-text-match v mid))))
        ((or "acked" "a")
         (gnaw--query-val-any
          val (lambda (v) (gnaw--query-actor-match v (plist-get info :acked)))))
        ((or "owned" "o")
         (gnaw--query-val-any
          val (lambda (v) (gnaw--query-actor-match
                           v (plist-get info :owned)
                           (plist-get info :owned-name)))))
        ((or "closed" "c")
         (gnaw--query-val-any
          val (lambda (v) (gnaw--query-actor-match v (plist-get info :closed)))))
        ((or "urgent" "u") (= (logand prio 2) 2))
        ((or "important" "i") (= (logand prio 1) 1))
        ((or "date" "d") (gnaw--query-date-match val (plist-get info :date) nil))
        ((or "deadline" "D") (gnaw--query-date-match val (plist-get info :deadline) t))
        ((or "expired" "e") (gnaw--query-date-match val (plist-get info :expiry) t))
        (_ (gnaw--query-text-match token (plist-get info :subject)))))))

(defun gnaw--query-parse (query)
  "Parse QUERY into OR-groups of AND-token lists (`|' = OR, space = AND)."
  (mapcar (lambda (grp) (split-string grp "[ \t]+" t))
          (split-string query "|" t)))

(defun gnaw--query-match-p (groups mid info)
  "Non-nil if report MID/INFO matches parsed GROUPS (from `gnaw--query-parse')."
  (seq-some (lambda (toks)
              (seq-every-p (lambda (tok) (gnaw--query-token-match tok mid info))
                           toks))
            groups))

(defconst gnaw--query-keys
  '("from:" "subject:" "topic:" "type:" "priority:" "mid:" "acked:"
    "owned:" "closed:" "urgent:" "important:" "date:" "deadline:" "expired:")
  "Long-form query keys completed in `gnaw-list-filter'.")

(defun gnaw--filter-completion (string pred action)
  "Completion table completing the query key of STRING's last token.
PRED and ACTION are the usual `completing-read' arguments; the rest of
the query (earlier tokens) is left untouched."
  (let ((beg (progn (string-match "[^ \t|]*\\'" string) (match-beginning 0))))
    (if (eq (car-safe action) 'boundaries)
        (let ((suffix (cdr action)))
          `(boundaries ,beg . ,(or (string-match "[ \t|]" suffix) (length suffix))))
      (let ((res (complete-with-action
                  action gnaw--query-keys (substring string beg) pred)))
        ;; For `try-completion' (ACTION nil) the table must return the
        ;; whole completed string, prefix included; `all-completions'
        ;; and `test-completion' operate on the last token alone.
        (if (and (null action) (stringp res))
            (concat (substring string 0 beg) res)
          res)))))

(defconst gnaw-report-types
  '("bug" "patch" "request" "announcement" "change" "release")
  "BONE report types, offered when filtering by type.")

(defun gnaw-list-filter-by (key)
  "Limit the list to reports whose KEY field matches a read value.
Read the value (completing types, `*' for flag fields), then set the
query to `KEY:value'; an empty value clears the filter."
  (let* ((flag (member key '("acked" "owned" "closed" "urgent" "important")))
         (val (cond (flag "*")
                    ((equal key "type") (completing-read "Type: " gnaw-report-types))
                    (t (read-string (format "%s: " key))))))
    (setq-local gnaw-list--query
                (and val (not (string-empty-p val)) (format "%s:%s" key val)))
    (gnaw-list-refresh)
    (if gnaw-list--query
        (message "gnaw: filter %s" gnaw-list--query)
      (message "gnaw: filter cleared"))))

(defmacro gnaw--define-filter-commands (&rest fields)
  "Define a `gnaw-list-filter-FIELD' command for each of FIELDS."
  `(progn
     ,@(mapcar (lambda (f)
                 `(defun ,(intern (concat "gnaw-list-filter-" f)) ()
                    ,(concat "Filter the report list by the " f " field.")
                    (interactive)
                    (gnaw-list-filter-by ,f)))
               fields)))

(gnaw--define-filter-commands
 "from" "subject" "topic" "priority" "mid"
 "date" "deadline" "expired"
 "acked" "owned" "closed" "urgent" "important")

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
    ("i" "Important"  gnaw-list-filter-important)]])

;;; Report browser (gnaw-list)

(defcustom gnaw-list-columns
  '(("Mark"      5 gnaw--mark-sort :mark)
    ("Type"      8 t :type)
    ("Flags"     5 t :flags)
    ("Pri"       4 gnaw--priority-sort :priority)
    ("Votes"     5 t :votes)
    ("From"     18 t :from-name)
    ("Att"       4 t :att)
    ("Subject"  50 t :subject)
    ("Created"  11 t :date)
    ("Activity" 11 t :last-activity)
    ("Topic"    16 t :topic))
  "Columns for `gnaw-list-mode' as (HEADER WIDTH SORT KEY) tuples.
SORT is t (sort on the printed string), nil, or a predicate function;
KEY is the INFO key (or :mark / :att) the cell displays.  The Subject
width is recomputed to fill the window by `gnaw--list-format'."
  :type '(repeat (list (string   :tag "Header")
                       (integer  :tag "Width")
                       (choice   :tag "Sort"
                                 (const :tag "By string" t)
                                 (const :tag "None" nil)
                                 (function :tag "Predicate"))
                       (symbol   :tag "Field key")))
  :group 'gnaw)

(defun gnaw--list-cell (key info mid state)
  "Return the display string for column KEY of report MID (INFO, STATE)."
  (pcase key
    ;; Local mark, one character like the gnaw CLI: * sticky, _ skipped.
    (:mark (cond ((gnaw-action-on-p state mid :sticky)
                  (propertize "*" 'face 'gnaw-sticky))
                 ((gnaw-action-on-p state mid :skip) "_")
                 (t " ")))
    (:att (if (plist-get info :patches) "+" ""))
    (:priority (gnaw-priority-letter (plist-get info :priority)))
    (:date (let ((d (plist-get info :date)))   ; keep the YYYY-MM-DD part only
             (if d (substring d 0 (min 10 (length d))) "")))
    (:subject (let ((s (or (plist-get info :subject) "")))
                (cond ((plist-get info :series-child) (concat "  " s))
                      ((plist-get info :series-summary)
                       (format "%s  (%s)" s (plist-get info :series-summary)))
                      (t s))))
    (_ (let ((v (plist-get info key))) (if v (format "%s" v) "")))))

(defun gnaw--mark-rank (cell)
  "Sort rank for a Mark-column CELL string, depending on the view.
Skipped hidden: skip < normal < sticky; skipped shown: sticky < normal
< skip."
  (let ((ch (and (> (length cell) 0) (aref cell 0))))
    (cond ((eq ch ?*) (if gnaw-list--show-skipped 0 2))
          ((eq ch ?_) (if gnaw-list--show-skipped 2 0))
          (t 1))))

(defun gnaw--mark-sort (a b)
  "Sort tabulated-list entries A and B by their Mark column."
  (< (gnaw--mark-rank (aref (cadr a) gnaw-list--mark-index))
     (gnaw--mark-rank (aref (cadr b) gnaw-list--mark-index))))

(defun gnaw--priority-sort (a b)
  "Sort tabulated-list entries A and B by numeric report priority."
  (< (or (plist-get (cdr (car a)) :priority) 0)
     (or (plist-get (cdr (car b)) :priority) 0)))

(defun gnaw--active-columns ()
  "Return `gnaw-list-columns' minus those named in config `:skip-columns'."
  (let ((skip (mapcar #'downcase (plist-get (gnaw-load-config) :skip-columns))))
    (cl-remove-if (lambda (c) (member (downcase (car c)) skip)) gnaw-list-columns)))

(defun gnaw--list-format ()
  "Return the `tabulated-list-format' vector for the active columns.
Grow the Subject column to fill the window; other columns keep their
width, so Topic stays at the right edge."
  (let* ((cols (gnaw--active-columns))
         (others (cl-remove-if (lambda (c) (eq (nth 3 c) :subject)) cols))
         (used (apply #'+ tabulated-list-padding 1
                      (mapcar (lambda (c) (1+ (nth 1 c))) others)))
         (subj (max 20 (- (window-body-width) used))))
    (setq-local gnaw-list--mark-index
                (or (cl-position :mark cols :key (lambda (c) (nth 3 c))) 0))
    (vconcat (mapcar (lambda (c)
                       (list (nth 0 c)
                             (if (eq (nth 3 c) :subject) subj (nth 1 c))
                             (nth 2 c)))
                     cols))))

(defun gnaw--list-entries ()
  "Return `tabulated-list-entries', folding patch series unless expanded.
A series is shown as one representative row (cover letter or first
patch) with a status summary; unfolded series (in `gnaw-list--expanded')
list each patch.  The query in `gnaw-list--query' filters the result."
  (let ((state (gnaw-read-state))
        (cols (gnaw--active-columns))
        (qgroups (and gnaw-list--query (gnaw--query-parse gnaw-list--query)))
        (pairs gnaw-list--reports)
        (groups (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (rows nil))
    (dolist (p pairs)
      (let ((sid (gnaw--series-id (cdr p))))
        (when sid (push p (gethash sid groups)))))
    (cl-flet ((row (pair &optional match-pairs)
                ;; Skip is judged on the displayed PAIR, but the query may
                ;; match any of MATCH-PAIRS (a folded series stays visible
                ;; when any of its members matches, not just the row shown).
                (when (and (or gnaw-list--show-skipped
                               (not (gnaw-action-on-p state (car pair) :skip)))
                           (or (null qgroups)
                               (seq-some
                                (lambda (mp)
                                  (gnaw--query-match-p qgroups (car mp) (cdr mp)))
                                (or match-pairs (list pair)))))
                  (push (list pair
                              (vconcat
                               (mapcar (lambda (c)
                                         (gnaw--list-cell (nth 3 c) (cdr pair) (car pair) state))
                                       cols)))
                        rows))))
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
                  (dolist (m members)
                    (row (cons (car m)
                               (plist-put (copy-sequence (cdr m)) :series-child t))))
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
    (define-key map (kbd "TAB") #'gnaw-list-toggle-fold)
    (define-key map "p" #'gnaw-list-view-patches)
    (define-key map "a" #'gnaw-list-apply-patches)
    (define-key map "A" #'gnaw-list-am-patches)
    (define-key map "g" #'gnaw-list-reload)
    (define-key map "G" #'gnaw-list-update)
    (define-key map "/" #'gnaw-list-filter)
    (define-key map "|" #'gnaw-list-filter-clear)
    (define-key map "=" #'gnaw-list-filter-transient)
    (define-key map "t" #'gnaw-list-limit-type)
    (define-key map "*" #'gnaw-list-toggle-sticky)
    (define-key map "_" #'gnaw-list-toggle-skip)
    (define-key map "\\" #'gnaw-list-toggle-skipped)
    (define-key map "?" #'describe-mode)
    map)
  "Keymap for `gnaw-list-mode'.")

(define-derived-mode gnaw-list-mode tabulated-list-mode "Gnaw"
  "Major mode listing open BONE reports.
\\<gnaw-list-mode-map>Press \\[describe-mode] for the full list of key bindings."
  (setq tabulated-list-format (gnaw--list-format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun gnaw-list-refresh ()
  "Re-render the list from the in-memory reports and current state.
Does not re-read the report cache; use `gnaw-list-reload' for that."
  (interactive)
  (setq tabulated-list-format (gnaw--list-format))
  (tabulated-list-init-header)
  (setq mode-line-process (and gnaw-list--query
                               (format " [%s]" gnaw-list--query)))
  (setq tabulated-list-entries (gnaw--list-entries))
  (tabulated-list-print t)
  (force-mode-line-update))

(defun gnaw-list-reload ()
  "Re-read reports from the local cache, then re-render."
  (interactive)
  (setq-local gnaw-list--reports (gnaw-reports))
  (gnaw-list-refresh))

(defun gnaw-list-filter-clear ()
  "Clear the active `gnaw-list' filter query."
  (interactive)
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

(defun gnaw-list-limit-type ()
  "Limit the list to a chosen report type."
  (interactive)
  (gnaw-list-filter-by "type"))

(defun gnaw-list--current ()
  "Return the (MID . INFO) pair at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No report at point")))

(defun gnaw-list-open ()
  "Open the email of the report at point."
  (interactive)
  (let ((p (gnaw-list--current)))
    (gnaw-read-message (car p) (cdr p))))

(defun gnaw-list-view-patches ()
  "View the patches of the report at point."
  (interactive)
  (gnaw-view-patches (cdr (gnaw-list--current))))

(defun gnaw-list-apply-patches ()
  "Apply the patches of the report at point with `git apply'."
  (interactive)
  (gnaw-apply-patches (cdr (gnaw-list--current))))

(defun gnaw-list-am-patches ()
  "Apply the patches of the report at point with `git am'."
  (interactive)
  (gnaw-am-patches (cdr (gnaw-list--current))))

(defun gnaw-list-toggle-fold ()
  "Fold or unfold the patch series of the report at point."
  (interactive)
  (let* ((pair (gnaw-list--current))
         (sid (gnaw--series-id (cdr pair)))
         (mid (car pair)))
    (unless sid (user-error "Not part of a patch series"))
    (setq-local gnaw-list--expanded
                (if (member sid gnaw-list--expanded)
                    (remove sid gnaw-list--expanded)
                  (cons sid gnaw-list--expanded)))
    (gnaw-list-refresh)
    ;; Return to the same report; if it is gone (folded from a sub-patch),
    ;; fall back to the series' representative row.
    (goto-char (point-min))
    (while (and (not (eobp))
                (let ((p (tabulated-list-get-id)))
                  (not (and p (equal (car p) mid)))))
      (forward-line 1))
    (when (eobp)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (let ((p (tabulated-list-get-id)))
                    (not (and p (equal (gnaw--series-id (cdr p)) sid)))))
        (forward-line 1)))))

(defun gnaw-list--toggle (action)
  "Toggle local mark ACTION on the report at point, then refresh."
  (let ((p (gnaw-list--current)))
    (gnaw-toggle-mark (car p) (cdr p) action)
    (gnaw-list-refresh)))

(defun gnaw-list-toggle-sticky ()
  "Toggle the sticky mark (keep visible) on the report at point."
  (interactive)
  (gnaw-list--toggle :sticky))

(defun gnaw-list-toggle-skip ()
  "Toggle the skip mark (hide) on the report at point."
  (interactive)
  (gnaw-list--toggle :skip))

(defun gnaw-list-toggle-skipped ()
  "Toggle whether skipped reports are shown."
  (interactive)
  (setq-local gnaw-list--show-skipped (not gnaw-list--show-skipped))
  (gnaw-list-refresh)
  (message "gnaw: skipped reports %s"
           (if gnaw-list--show-skipped "shown" "hidden")))

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
  "Read config.edn and return its raw alist, or nil."
  (let ((file (expand-file-name "config.edn" gnaw-config-dir)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (gnaw-edn-read-buffer))))))

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
meta.json or reports.json URL; the report files listed in meta.json's
`reports-files' are offered for selection (default all-open.json), the
source NAME (from meta.json) and its local git REPO are requested."
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
          (repo (when (y-or-n-p "Link a local git repo (for git am)? ")
                  (expand-file-name (read-directory-name "Git repo: ")))))
     (list (mapcar (lambda (f) (concat dir f)) chosen) name repo)))
  (let ((urls (if (listp urls) urls (list urls)))
        (name (and name (not (string-empty-p (string-trim name))) name)))
    (unless urls (user-error "No report file selected"))
    (gnaw--write-config (gnaw--config-add-source (gnaw--read-config-raw) urls name repo))
    (message "gnaw: added to config.edn: %s%s"
             (string-join urls ", ") (if name (format " (%s)" name) ""))
    urls))

(defun gnaw--setup-sources ()
  "Interactively add sources to config.edn until the user declines."
  (while (y-or-n-p (if (gnaw-sources) "Add another source? " "Add a source? "))
    (call-interactively #'gnaw-add-source)))

;;;###autoload
(defun gnaw ()
  "Browse open BONE reports in a tabulated list filling the frame.
Prompt to add a source when none is configured."
  (interactive)
  (unless (gnaw-sources)
    (gnaw--setup-sources))
  (let ((buf (get-buffer-create "*gnaw*")))
    (switch-to-buffer buf)
    (delete-other-windows)
    (gnaw-list-mode)
    (gnaw-list-reload)))

(provide 'gnaw)
;;; gnaw.el ends here
