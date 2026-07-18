;;; gnaw.el --- Browse and manage BONE reports in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail, news
;; URL: https://codeberg.org/bzg/gnaw.el
;; Version: 0.26.0
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
;;   `gnaw-update'       refresh the local cache; C-u forces a re-download
;;   `gnaw-toggle-mark'  toggle :sticky/:dismiss for a message-id
;;   `gnaw-read-state' / `gnaw-write-state'   state.edn I/O
;;   `gnaw-annotation'   fixed-width report annotation for MUA lines
;;
;;; Code:

(require 'url)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'transient)

;; `json-parse-buffer' and friends only exist when Emacs was built
;; with JSON support, which is optional before Emacs 30.
(unless (json-available-p)
  (error "gnaw.el needs an Emacs built with native JSON support"))

(defvar url-http-response-status)

(defgroup gnaw nil
  "Read and manage BONE reports shared with the gnaw CLI."
  :group 'mail)

(defconst gnaw-version (or (package-get-version) "0.26.0")
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
  "Functions run once `gnaw-update' finished refreshing the local cache.
The update downloads in the background, so the hook runs from its
callbacks, after `gnaw-update' itself returned.  Front-ends can use
this to re-apply their display."
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

(defun gnaw--file-mtime (file)
  "Return FILE's modification time, or nil when FILE is absent.
The mtime caches below compare it with `equal' to decide freshness."
  (file-attribute-modification-time (file-attributes file)))

(defvar gnaw--config-cache nil
  "Cons (MTIME . PLIST) caching the last `gnaw-load-config' result.")

(defun gnaw-load-config ()
  "Load `config.edn' and return a plist.
Keys: :addresses :skip-columns :source-configs (raw `:sources' maps)
and :sources (their URLs).  The result is cached until config.edn
changes."
  (let* ((file (expand-file-name "config.edn" gnaw-config-dir))
         (mtime (gnaw--file-mtime file)))
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

(defun gnaw--entry-repos (entry)
  "Return config.edn source ENTRY's `:repo' as a list of directories.
The entry holds a directory string or a vector of them."
  (mapcar #'expand-file-name (ensure-list (alist-get :repo entry))))

(defun gnaw--source-repos (info)
  "Return the local git repos for report INFO's source, as a list.
Reads `:repo' from the matching config.edn `:sources' entry, which
holds a directory string or a vector of them."
  (gnaw--entry-repos (gnaw--source-config-entry info)))

(defun gnaw--source-repo (info)
  "Return the first local git repo for report INFO's source, or nil.
The directory the save commands propose; `gnaw--target-repo' asks
which repo to apply in when the source lists several."
  (car (gnaw--source-repos info)))

(defun gnaw--multi-source-p ()
  "Return non-nil when config.edn defines more than one source."
  (> (length (plist-get (gnaw-load-config) :source-configs)) 1))

(defun gnaw--source-letter (info)
  "Return the letter identifying report INFO's source, or nil.
Reads `:letter' from the matching config.edn `:sources' entry."
  (alist-get :letter (gnaw--source-config-entry info)))

(defun gnaw--suggest-source-letter (name taken)
  "Suggest a letter identifying source NAME, avoiding the TAKEN ones.
Try NAME's letters and digits in order (so \"Org mode ML\" suggests
O), then A to Z.  TAKEN is a list of downcased single-character
strings; return an upcased one, or nil when everything is taken."
  (seq-some (lambda (ch)
              (let ((s (upcase (char-to-string ch))))
                (and (not (member (downcase s) taken)) s)))
            (append (seq-filter (lambda (ch)
                                  (string-match-p "[[:alnum:]]"
                                                  (char-to-string ch)))
                                (or name ""))
                    (number-sequence ?A ?Z))))

(defun gnaw--source-match-p (entry urls name)
  "Return non-nil when config source ENTRY shares one of URLS or is NAME.
This is the rule deciding which config.edn entry a
`gnaw--config-add-source' call replaces."
  (or (seq-intersection urls (alist-get :urls entry))
      (and name (equal name (alist-get :name entry)))))

(defun gnaw--taken-source-letters (entries)
  "Return the downcased letters the config source ENTRIES define."
  (mapcar #'downcase
          (delq nil (mapcar (lambda (s) (alist-get :letter s)) entries))))

(defun gnaw--read-source-letter (name taken &optional default)
  "Read the letter identifying source NAME in the browser's S column.
TAKEN is a list of downcased letters already identifying another
source; DEFAULT preempts the `gnaw--suggest-source-letter' suggestion.
Insist until the answer is a single letter or digit not in TAKEN --
the query syntax gives other characters a meaning (\"|\", quotes,
spaces), which would break the S: filters built from the letter."
  (let ((def (or default (gnaw--suggest-source-letter name taken)))
        letter)
    (while (not letter)
      (let ((s (string-trim
                (read-string (format "Letter for source %s%s: " name
                                     (if def (format " (default %s)" def) ""))
                             nil nil def))))
        (cond ((not (string-match-p "\\`[[:alnum:]]\\'" s))
               (message "gnaw: a single letter or digit, please")
               (sit-for 1))
              ((member (downcase s) taken)
               (message "gnaw: %s already identifies another source" s)
               (sit-for 1))
              (t (setq letter (upcase s))))))
    letter))

(defun gnaw--ensure-source-letters ()
  "Ask for the missing source letters and save them to config.edn.
With several sources, the browser's S column identifies each
report's source by the `:letter' of its config.edn entry: ask
interactively for the entries not defining one.  No-op with zero
or one source.  Quitting mid-prompts still saves the letters
answered so far, so they are not asked again."
  (let* ((raw (gnaw--read-config-raw))
         (cfgs (alist-get :sources raw)))
    (when (and (> (length cfgs) 1)
               (seq-some (lambda (s) (not (alist-get :letter s))) cfgs))
      (let ((taken (gnaw--taken-source-letters cfgs))
            (done nil)
            (changed nil))
        (unwind-protect
            (dolist (s cfgs)
              (push (if (alist-get :letter s)
                        s
                      (let ((letter (gnaw--read-source-letter
                                     (or (alist-get :name s)
                                         (car (alist-get :urls s)))
                                     taken)))
                        (push (downcase letter) taken)
                        (setq changed t)
                        (append s (list (cons :letter letter)))))
                    done))
          (when changed
            ;; A quit leaves the entries not reached in their old form.
            (let ((rest (nthcdr (length done) cfgs)))
              (gnaw--write-config
               (gnaw--alist-put raw :sources
                                (append (nreverse done) rest))))))))))

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

(defcustom gnaw-cache-attachments-max-age nil
  "Days before `gnaw-cache-cleanup' discards a cached attachment.
An attachment directory is discarded once every file in it is older.
Nil, the default, keeps attachments forever."
  :type '(choice (const :tag "Keep forever" nil) (integer :tag "Days"))
  :group 'gnaw)

(defun gnaw--http-parse-reply (url)
  "Parse the `url-retrieve' reply in the current buffer.
Return a plist (:status :body :etag); signal an error on HTTP
errors or a malformed reply, naming URL."
  (let ((status (bound-and-true-p url-http-response-status)))
    (when (and status (>= status 400))
      (error "HTTP error %d from %s" status url))
    (goto-char (point-min))
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (error "Malformed HTTP response from %s" url))
    (let ((header-end (point)))
      (list :status status
            :etag (save-excursion
                    (goto-char (point-min))
                    (let ((case-fold-search t))
                      (when (re-search-forward
                             "^ETag:[ \t]*\\([^\r\n]*\\)"
                             header-end t)
                        (match-string 1))))
            :body (buffer-substring-no-properties
                   header-end (point-max))))))

(defun gnaw--http-fetch (url &optional head)
  "Fetch URL and return a plist (:status :body :etag).
Non-nil HEAD sends a HEAD request instead of GET: the reply carries
the same headers but no body, which is enough to read :etag cheaply.
Signal an error on HTTP errors or after `gnaw-http-timeout' seconds
without a response."
  (let* ((coding-system-for-read 'binary)
         (url-request-method (if head "HEAD" url-request-method))
         (buf (url-retrieve-synchronously url t nil gnaw-http-timeout)))
    (unless buf (error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf (gnaw--http-parse-reply url))
      (kill-buffer buf))))

(defun gnaw--http-fetch-async (url head callback)
  "Fetch URL in the background, then pass CALLBACK one reply plist.
The plist carries :status :body :etag like `gnaw--http-fetch', or
just :error with a description when the fetch failed.  Non-nil HEAD
sends a HEAD request.  A watchdog enforces `gnaw-http-timeout':
`url-retrieve' alone would wait on a hung connection forever."
  (let* ((coding-system-for-read 'binary)
         (url-request-method (if head "HEAD" "GET"))
         (finished nil)
         timer buf)
    (cl-flet ((finish (reply)
                (unless finished
                  (setq finished t)
                  (when (timerp timer) (cancel-timer timer))
                  (funcall callback reply))))
      (setq buf (url-retrieve
                 url
                 (lambda (status)
                   (let ((reply (condition-case err
                                    (if-let* ((e (plist-get status :error)))
                                        (list :error (error-message-string e))
                                      (gnaw--http-parse-reply url))
                                  (error (list :error (error-message-string
                                                       err))))))
                     (kill-buffer (current-buffer))
                     (finish reply)))
                 nil t))
      ;; `run-at-time' fires a nil delay immediately: no timeout
      ;; configured means no watchdog at all.
      (setq timer (and gnaw-http-timeout
                       (run-at-time
                        gnaw-http-timeout nil
                        (lambda ()
                          (when (buffer-live-p buf)
                            (when-let* ((proc (get-buffer-process buf)))
                              (delete-process proc))
                            (kill-buffer buf))
                          (finish (list :error
                                        (format "no response after %ss"
                                                gnaw-http-timeout))))))))))

(defun gnaw--http-body (url)
  "Return the raw HTTP body bytes of URL.
Signal an error after `gnaw-http-timeout' seconds without a response."
  (plist-get (gnaw--http-fetch url) :body))

(defconst gnaw--json-parse-args
  '(:object-type alist :array-type list :false-object nil :null-object nil)
  "Keyword arguments shaping how gnaw parses JSON into Lisp data.
Objects become alists with symbol keys and arrays become lists.
Both `false' and `null' map to nil: downstream code tests report
fields with plain nil checks and cannot tell them apart.")

(defun gnaw--parse-json-string (string)
  "Parse STRING, JSON text, into Lisp data."
  (apply #'json-parse-string string gnaw--json-parse-args))

(defun gnaw--parse-json-file (file)
  "Parse FILE, UTF-8 encoded JSON, into Lisp data."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (apply #'json-parse-buffer gnaw--json-parse-args)))

(defun gnaw--validators-file (cache-file)
  "Return the file storing HTTP validators for CACHE-FILE."
  (concat cache-file ".headers"))

(defun gnaw--read-validators (cache-file)
  "Return the plist (:etag ...) saved for CACHE-FILE, or nil.
Validators are only meaningful while CACHE-FILE itself exists."
  (when (file-exists-p cache-file)
    (let ((file (gnaw--validators-file cache-file)))
      (when (file-exists-p file)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents file)
            (read (current-buffer))))))))

(defun gnaw--save-validators (cache-file etag)
  "Persist ETAG for CACHE-FILE, or drop the saved one when ETAG is nil."
  (let ((file (gnaw--validators-file cache-file)))
    (if etag
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 (list :etag etag) (current-buffer))))
      (when (file-exists-p file)
        (delete-file file)))))

(defun gnaw--etag-equal (a b)
  "Weakly compare ETags A and B, ignoring the W/ prefix.
The origin may weaken an ETag (gzip does) between two requests, so
only the opaque value decides whether the content changed."
  (and a b (equal (string-remove-prefix "W/" a)
                  (string-remove-prefix "W/" b))))

(defun gnaw--commit-json-reply (reply cache-file)
  "Write the JSON body of HTTP REPLY to CACHE-FILE, then its ETag.
Parse before writing -- a malformed download (truncated body, error
page) must not replace a valid cache -- and save the ETag only
after, so a failed write leaves the old one and the next refresh
still sees the stale cache.  Return `changed' when CACHE-FILE's
content changed, `refetched' when it was byte-identical."
  (let ((body (decode-coding-string (plist-get reply :body) 'utf-8)))
    (gnaw--parse-json-string body)
    (let ((wrote (gnaw--write-json-to-file body cache-file)))
      (gnaw--save-validators cache-file (plist-get reply :etag))
      (if wrote 'changed 'refetched))))

(defun gnaw--refresh-cache (url cache-file &optional force)
  "Refresh CACHE-FILE from URL, revalidating with a HEAD request.
When an ETag was saved by a previous refresh (and FORCE is nil),
first ask the server for the headers only and compare ETags locally:
an unchanged source costs a HEAD round-trip instead of a download.
Otherwise download and commit (see `gnaw--commit-json-reply').
Return `unchanged' when the saved ETag is still current, else the
commit's outcome."
  (let ((etag (unless force
                (plist-get (gnaw--read-validators cache-file) :etag))))
    (if (and etag
             ;; Revalidation is only an optimization: when the HEAD
             ;; fails (proxy rejecting the method, timeout), fall
             ;; through to the full GET instead of failing the
             ;; refresh.
             (gnaw--etag-equal etag
                               (plist-get (ignore-errors
                                            (gnaw--http-fetch url 'head))
                                          :etag)))
        'unchanged
      (gnaw--commit-json-reply (gnaw--http-fetch url) cache-file))))

(defun gnaw--update-source (source force callback)
  "Refresh SOURCE's cache in the background; CALLBACK gets the outcome.
The outcome is `changed', `unchanged', `refetched' as in
`gnaw--refresh-cache', or `failed' -- the failure is also messaged.
Follows the same sequence asynchronously: HEAD revalidation by saved
ETag unless FORCE, then a download parsed before it may overwrite
the cache."
  (let* ((cache-file (gnaw--source-to-cache-file source))
         (etag (unless force
                 (plist-get (gnaw--read-validators cache-file) :etag))))
    (cl-flet ((download ()
                (gnaw--http-fetch-async
                 source nil
                 (lambda (reply)
                   (funcall
                    callback
                    (condition-case err
                        (if-let* ((e (plist-get reply :error)))
                            (error "%s" e)
                          (gnaw--commit-json-reply reply cache-file))
                      (error
                       (message "gnaw: failed updating %s: %s"
                                source (error-message-string err))
                       'failed)))))))
      (if etag
          ;; Revalidation is only an optimization: when the HEAD fails
          ;; (proxy rejecting the method, timeout), fall through to the
          ;; full GET instead of failing the refresh.
          (gnaw--http-fetch-async
           source 'head
           (lambda (reply)
             (if (and (not (plist-get reply :error))
                      (gnaw--etag-equal etag (plist-get reply :etag)))
                 (funcall callback 'unchanged)
               ;; This runs from an async callback, outside the
               ;; dispatch guard of `gnaw-update': a synchronous error
               ;; here must still reach CALLBACK, or the pending
               ;; counter never comes down.
               (condition-case err
                   (download)
                 (error
                  (message "gnaw: failed updating %s: %s"
                           source (error-message-string err))
                  (funcall callback 'failed))))))
        (download)))))

(defun gnaw--write-json-to-file (json file)
  "Write JSON, a string, to FILE as UTF-8.
Leave an already-identical FILE untouched; return non-nil when its
content actually changed."
  (make-directory (file-name-directory file) t)
  (unless (and (file-exists-p file)
               (equal json
                      (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8))
                          (insert-file-contents file))
                        (buffer-string))))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert json)))
    t))

(defun gnaw--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (if (gnaw--http-url-p source)
      (let ((cache-file (gnaw--source-to-cache-file source)))
        ;; First read of this source: populate the cache (and its
        ;; ETag), then read it back like any other cached source.
        (unless (file-exists-p cache-file)
          (gnaw--refresh-cache source cache-file))
        (gnaw--parse-json-file cache-file))
    (gnaw--parse-json-file source)))

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

(defvar gnaw--reports-cache (make-hash-table :test 'equal)
  "Cache of `gnaw--extract-reports' results, keyed by source.
Each value is (FILE-MTIME CONFIG-MTIME PAIRS), the modification
times of the source's file and of config.edn (which carries the
source letters) when PAIRS was extracted.")

(defun gnaw--config-mtime ()
  "Return config.edn's modification time, or nil when absent."
  (gnaw--file-mtime (expand-file-name "config.edn" gnaw-config-dir)))

(defun gnaw--extract-reports (source)
  "Extract published reports from SOURCE as (MID . INFO) pairs.
Closed reports (status below 4, flags C, R, E or S) are kept when
SOURCE lists them, as an all.json does; the list hides them until
a query asks for them (see `gnaw-list--display-reports').
The result is cached until the source's file or config.edn changes
on disk, so a reload does not re-parse unchanged JSON.  Callers get
the cached list itself: copy before mutating (as `gnaw-reports'
does for `mapcan')."
  (let* ((file (if (gnaw--http-url-p source)
                   (gnaw--source-to-cache-file source)
                 source))
         (mtime (gnaw--file-mtime file))
         (cmtime (gnaw--config-mtime))
         (cached (gethash source gnaw--reports-cache)))
    (if (and mtime
             (equal (nth 0 cached) mtime)
             (equal (nth 1 cached) cmtime))
        (nth 2 cached)
      (let ((pairs (gnaw--extract-reports-1 source)))
        ;; Re-stat: `gnaw--read-json' populates a missing cache file.
        (puthash source
                 (list (gnaw--file-mtime file) cmtime pairs)
                 gnaw--reports-cache)
        pairs))))

(defun gnaw--extract-reports-1 (source)
  "Parse SOURCE and extract its report pairs (see `gnaw--extract-reports')."
  (let* ((data (gnaw--read-json source))
         (fv (alist-get 'bone-format data))
         (sname (alist-get 'source data))
         (letter (gnaw--source-letter (list :source source
                                            :source-name sname)))
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
            (acked-name   (alist-get 'acked-name r))
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
            (patch-seq    (alist-get 'patch-seq r))
            (trailers     (alist-get 'trailers r)))
        (when (and mid (numberp status))
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
                                       :source-letter letter
                                       :archived-at archived-at
                                       :patches patches
                                       :events events
                                       :texts texts
                                       :awaiting awaiting
                                       :related related
                                       :series series
                                       :patch-seq patch-seq
                                       :trailers trailers
                                       :acked acked
                                       :acked-name acked-name
                                       :owned owned
                                       :owned-name owned-name
                                       :closed closed))
                  result)))))
    (nreverse result)))

(defun gnaw-reports ()
  "Collect report pairs from all sources, tolerating failures.
A source that fails to load is skipped and reported through
`display-warning' -- a transient `message' would be clobbered by
the rendering that follows, leaving an empty listing with no
visible explanation."
  (mapcan
   (lambda (source)
     (condition-case err
         ;; Copy: `mapcan' splices destructively, and the extracted
         ;; list belongs to `gnaw--reports-cache' -- nconc'ing it in
         ;; place would chain the caches together, then loop them.
         (append (gnaw--extract-reports source) nil)
       (error
        (display-warning
         'gnaw
         (format "failed loading source %s: %s"
                 source (error-message-string err))
         :error)
        nil)))
   (gnaw-sources)))

(defvar gnaw--update-pending nil
  "Number of sources the running `gnaw-update' still waits on, or nil.")

(defun gnaw-update (&optional force callback)
  "Refresh the local cache from remote JSON sources, in the background.
The sources download in parallel while Emacs stays responsive; each
is first revalidated with a HEAD request comparing the server's ETag
to the saved one, so an unchanged source costs no download.  With a
prefix argument FORCE, skip the revalidation and re-download every
source.  Once every source finished, run `gnaw-after-update-hook',
then CALLBACK when non-nil."
  (interactive "P")
  (when gnaw--update-pending
    (user-error "gnaw: an update is already running"))
  (let* ((sources (cl-remove-if-not #'gnaw--http-url-p (gnaw-sources)))
         (changed 0)
         (failed 0)
         (done (lambda (msg)
                 (setq gnaw--update-pending nil)
                 (run-hooks 'gnaw-after-update-hook)
                 (message "%s" msg)
                 (when callback (funcall callback))))
         (finish (lambda (outcome)
                   (pcase outcome
                     ('changed (cl-incf changed))
                     ('failed (cl-incf failed)))
                   (when (zerop (cl-decf gnaw--update-pending))
                     (funcall done
                              (format "gnaw: cache refreshed%s%s."
                                      (if (zerop changed)
                                          ", no changes"
                                        (format " (%d source%s changed)"
                                                changed
                                                (if (= changed 1) "" "s")))
                                      (if (zerop failed)
                                          ""
                                        (format ", %d failed" failed))))))))
    (if (null sources)
        (funcall done "gnaw: no remote source to update")
      (setq gnaw--update-pending (length sources))
      (message "gnaw: updating %d source%s in the background..."
               (length sources) (if (cdr sources) "s" ""))
      (dolist (source sources)
        ;; A synchronous failure (malformed URL) must still count
        ;; down, or `gnaw--update-pending' would block updates forever.
        (condition-case err
            (gnaw--update-source source force finish)
          (error
           (message "gnaw: failed updating %s: %s"
                    source (error-message-string err))
           (funcall finish 'failed)))))))

(defun gnaw--tree-size (path)
  "Return the total byte size of PATH, a file or a directory."
  (if (file-directory-p path)
      (apply #'+ (mapcar (lambda (f)
                           (or (file-attribute-size (file-attributes f)) 0))
                         (directory-files-recursively path "")))
    (or (file-attribute-size (file-attributes path)) 0)))

(defun gnaw--cache-report-files (&optional all)
  "Return the files under cache/reports/ that no configured source owns.
Non-nil ALL returns every file there, the configured sources' own
caches and validators included."
  (let ((dir (expand-file-name "cache/reports" gnaw-config-dir))
        (keep (mapcan (lambda (s)
                        (when (gnaw--http-url-p s)
                          (let ((f (gnaw--source-to-cache-file s)))
                            (list f (gnaw--validators-file f)))))
                      (gnaw-sources))))
    (when (file-directory-p dir)
      (cl-remove-if (lambda (f) (and (not all) (member f keep)))
                    (directory-files dir t
                                     directory-files-no-dot-files-regexp)))))

(defun gnaw--cache-stale-attachment-dirs ()
  "Return the cached attachment directories the max age discards.
A directory under cache/patches/, cache/events/ or cache/text/ is
stale when every file in it is older than
`gnaw-cache-attachments-max-age' days (see `gnaw-cache-cleanup')."
  (when gnaw-cache-attachments-max-age
    (let ((cutoff (time-subtract (current-time)
                                 (days-to-time
                                  gnaw-cache-attachments-max-age)))
          stale)
      (dolist (subdir '("patches" "events" "text") (nreverse stale))
        (let ((base (expand-file-name (concat "cache/" subdir)
                                      gnaw-config-dir)))
          (when (file-directory-p base)
            (dolist (srcdir (directory-files
                             base t directory-files-no-dot-files-regexp))
              (when (file-directory-p srcdir)
                (dolist (d (directory-files
                            srcdir t directory-files-no-dot-files-regexp))
                  (when (and (file-directory-p d)
                             (cl-every (lambda (f)
                                         (time-less-p (gnaw--file-mtime f)
                                                      cutoff))
                                       (directory-files-recursively d "")))
                    (push d stale)))))))))))

(defun gnaw-cache-cleanup (&optional all)
  "Delete the cache files gnaw no longer needs, after confirming.
Candidates are the report caches of sources config.edn no longer
lists -- their file names hash the source URL, so a changed or
removed source strands its old cache forever -- and the attachments
untouched for `gnaw-cache-attachments-max-age' days.  With a prefix
argument ALL, also delete the configured sources' report caches;
the next update re-downloads them.  State.edn is never touched: it
is user data shared with the gnaw CLI, not a cache."
  (interactive "P")
  (let* ((files (gnaw--cache-report-files all))
         (dirs (gnaw--cache-stale-attachment-dirs))
         (bytes (apply #'+ (mapcar #'gnaw--tree-size (append files dirs)))))
    (if (not (or files dirs))
        (message "gnaw: cache clean, nothing to delete")
      (if (not (y-or-n-p
                (format "gnaw: delete %d source cache file(s) and %d attachment dir(s), freeing %s? "
                        (length files) (length dirs)
                        (file-size-human-readable bytes))))
          (message "gnaw: cache kept")
        (dolist (f files) (delete-file f))
        (dolist (d dirs)
          (delete-directory d t)
          ;; Drop the per-source parent when it just emptied.
          (let ((parent (file-name-directory (directory-file-name d))))
            (when (directory-empty-p parent)
              (delete-directory parent))))
        ;; Drop the in-memory extractions the deletions orphaned.
        (let ((sources (gnaw-sources)))
          (maphash (lambda (k _)
                     (unless (and (not all) (member k sources))
                       (remhash k gnaw--reports-cache)))
                   gnaw--reports-cache))
        (message "gnaw: freed %s" (file-size-human-readable bytes))))))

;;; State file (state.edn)

(defvar gnaw--state-cache nil
  "List (MTIME ALIST TABLE) caching the last `gnaw-read-state' read.
MTIME is state.edn's modification time when ALIST was parsed and
TABLE indexes ALIST's entries by message-id.  The gnaw CLI shares
the file, so the mtime decides freshness.")

(defun gnaw--state-index (state)
  "Return STATE's entries in a hash table keyed by message-id."
  (let ((table (make-hash-table :test 'equal :size (length state))))
    (dolist (kv state table)
      (puthash (car kv) (cdr kv) table))))

(defun gnaw-read-state ()
  "Read and return the gnaw state alist, or nil.
The result is cached until state.edn changes on disk (the gnaw CLI
shares the file); `gnaw--state-table' serves the indexed view.  A
rewrite landing within the filesystem's mtime granularity would go
unnoticed until the next bump; sub-second timestamps on modern
filesystems make that window negligible."
  (let* ((file (expand-file-name "state.edn" gnaw-config-dir))
         (mtime (gnaw--file-mtime file)))
    (if (and gnaw--state-cache (equal (car gnaw--state-cache) mtime))
        (nth 1 gnaw--state-cache)
      (let ((state (gnaw--read-edn-file file)))
        (setq gnaw--state-cache (list mtime state (gnaw--state-index state)))
        state))))

(defun gnaw--state-table ()
  "Return the current state indexed by message-id in a hash table."
  (gnaw-read-state)
  (nth 2 gnaw--state-cache))

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
      (rename-file tmp file t))
    ;; Refresh the cache from the state just written, saving the next
    ;; read a re-parse.
    (setq gnaw--state-cache
          (list (gnaw--file-mtime file) state (gnaw--state-index state)))))

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
    (gnaw--undo-push (format "%s toggle" (substring (symbol-name action) 1))
                     (list (cons mid (cdr (assoc mid state)))))
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
      (gnaw--undo-push "mark removal" (list (cons mid entry)))
      (let ((new (gnaw--alist-dissoc (gnaw--alist-dissoc entry :sticky)
                                     :dismiss)))
        (gnaw-write-state
         (if (gnaw--entry-removable-p new)
             (gnaw--state-delete state mid)
           (gnaw--state-put state mid new))))
      t)))

;;; Undo of mark changes

(defvar gnaw--undo-log nil
  "Session log of mark changes, most recent first.
Each element is (LABEL . PAIRS) where LABEL describes the change and
PAIRS lists (MID . ENTRY) with each entry as it was in state.edn
before the change, nil when the report had none.")

(defun gnaw--undo-push (label pairs)
  "Record LABEL and PAIRS, a list of (MID . ENTRY-BEFORE), for `gnaw-undo'."
  (push (cons label pairs) gnaw--undo-log))

(defun gnaw--undo-restore (state mid before)
  "Return STATE with MID's entry restored to BEFORE.
Only the keys owned by gnaw.el are taken from BEFORE; keys another
program wrote to the current entry since (such as the CLI's
`:skip-since') survive the restoration."
  (let* ((current (cdr (assoc mid state)))
         (entry (append
                 (cl-remove-if-not (lambda (kv)
                                     (memq (car kv) gnaw--own-entry-keys))
                                   before)
                 (cl-remove-if (lambda (kv)
                                 (memq (car kv) gnaw--own-entry-keys))
                               current))))
    (if (or (null entry) (gnaw--entry-removable-p entry))
        (gnaw--state-delete state mid)
      (gnaw--state-put state mid entry))))

(defun gnaw-undo ()
  "Undo the last mark change made from this Emacs session.
Restore the affected state.edn entries as they were before the
change, original timestamps included -- something re-toggling a
mark cannot do.  Repeated calls walk further back.  Marks written
by another program since the change are left untouched."
  (interactive)
  (unless gnaw--undo-log
    (user-error "gnaw: no mark change to undo in this session"))
  (let ((op (pop gnaw--undo-log))
        (state (gnaw--read-state-for-update)))
    (dolist (pair (cdr op))
      (setq state (gnaw--undo-restore state (car pair) (cdr pair))))
    (gnaw-write-state state)
    (message "gnaw: undid %s" (car op))))

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
  "Open the current message's web archive page in a browser.
Only the `web' open method records that URL in its message buffer;
called anywhere else -- the report list included -- fall back on
the report at point, like \\<gnaw-list-mode-map>\\[gnaw-list-browse]."
  (interactive)
  (cond (gnaw--message-archive-url (browse-url gnaw--message-archive-url))
        ((derived-mode-p 'gnaw-list-mode) (gnaw-list-browse))
        (t (user-error "No archive URL for this message"))))

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

(defun gnaw--read-gnus-group (prompt &optional default)
  "Read a Gnus group with PROMPT, completing over all known groups.
Start Gnus first when needed so the group list is populated.
Empty input returns DEFAULT; completion appends the default and a
colon to PROMPT, so PROMPT must end with neither."
  (require 'gnus)
  (unless (gnus-alive-p) (gnus))
  (gnus-group-completing-read prompt nil nil nil nil default))

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

(defun gnaw--read-open-message-method (&optional default)
  "Read an open-message method interactively, DEFAULT preselected."
  (let ((def (symbol-name (or default 'auto))))
    (intern (completing-read (format-prompt "Open messages with" def)
                             gnaw--open-message-method-choices
                             nil t nil nil def))))

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

(defun gnaw--configured-value (alist source)
  "Return the ALIST value configured for SOURCE, a name or t.
A source without an entry of its own falls back to the default
entry, the one keyed by t."
  (cdr (or (and (stringp source) (assoc source alist))
           (assq t alist))))

(defun gnaw--method-string (method group)
  "Return METHOD as a string, GROUP appended when METHOD is `gnus'."
  (format "%s%s" method
          (if (and (eq method 'gnus) group (not (string-empty-p group)))
              (format " in %s" group)
            "")))

(defun gnaw--read-source-gnus-group (source)
  "Read the Gnus group for SOURCE, defaulting to the configured one.
SOURCE is a source name string, or t for the default entry."
  (let ((def (gnaw--configured-value gnaw-gnus-group source)))
    (gnaw--read-gnus-group
     (if def "Gnus group" "Gnus group (empty = ask each time)") def)))

;;;###autoload
(defun gnaw-configure-email-client (source method &optional group)
  "Configure how SOURCE messages are opened.
SOURCE is a configured source name, or t for the global default.  METHOD is
one of `auto', `mua', `gnus', `notmuch', `mu4e' or `web'.  When METHOD is
`gnus', GROUP stores the Gnus group to search first."
  (interactive
   (let* ((source (gnaw--read-email-client-source))
          (method (gnaw--read-open-message-method
                   (gnaw--configured-value gnaw-open-message-method source)))
          (group (when (eq method 'gnus)
                   (gnaw--read-source-gnus-group source))))
     (list source method group)))
  (gnaw--save-source-open-method source method group)
  (message "gnaw: %s messages open with %s"
           (if (eq source t) "all sources" source)
           (gnaw--method-string method group))
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

(defcustom gnaw-am-branch-function #'gnaw-branch-who-what-v
  "Function proposing a branch name to `gnaw-am-patches'.
Called with the report INFO plist; the returned string (or nil)
pre-fills the new-branch prompt."
  :type 'function
  :group 'gnaw)

(defcustom gnaw-am-create-worktree nil
  "When non-nil, `gnaw-am-patches' applies patches in a new worktree.
The worktree shares the repo's git store but leaves its current
checkout untouched; its directory is read by
`gnaw-am-read-worktree-function'.  A prefix argument on the am
commands inverts this setting for one call."
  :type 'boolean
  :group 'gnaw)

(defcustom gnaw-am-read-worktree-function
  #'gnaw-am-read-worktree-sibling-named-by-branch
  "Function reading the to-be-created worktree of `gnaw-am-patches'.
Called with two arguments: the code repository, and the name of the
branch to create there -- nil for a detached HEAD."
  :type 'function
  :group 'gnaw)

(defcustom gnaw-am-show-repo t
  "Whether to show the repository after a successful `git am'.
Non-nil pops a `magit-status' buffer on the directory the patches
were applied in -- the worktree, when one was created -- falling
back on `dired' when magit is not loaded."
  :type 'boolean
  :group 'gnaw)

(defcustom gnaw-am-fold-trailers t
  "Whether `gnaw-am-patches' folds the collected trailers into commits.
BONE collects the review trailers (Acked-by:, Reviewed-by:, ...)
posted in reply to a patch and publishes them in the report's
`trailers' field.  When non-nil, `git am' receives a temporary copy
of each patch file with the missing trailers appended to the commit
message, the way b4 does; the cached patch files are never modified.
This is also the master switch for `gnaw-am-synthetic-trailers'."
  :type 'boolean
  :group 'gnaw)

(defcustom gnaw-am-synthetic-trailers '(acked link)
  "Trailers derived from the report state by `gnaw-am-patches'.
On mailing lists where review acts are BONE commands (\"Acked.\",
\"Confirmed.\", \"Reviewed.\") rather than kernel-style reply
trailers, the report state itself carries the review information.
Each element synthesizes one trailer, folded like the collected
ones (and only when `gnaw-am-fold-trailers' is non-nil):
 - `acked': \"Reviewed-by: Name <address>\" from the acked state --
   BONE's acked is the strong review approval, so Reviewed-by is
   its kernel-language translation;
 - `owned': \"Reviewed-by: Name <address>\" from the owned state;
 - `link':  \"Link: <url>\" to the report's archived web page.
`owned' is not in the default: an owner has not necessarily
reviewed -- enable it only if that shortcut holds on your source.
A synthetic trailer is dropped when BONE already collected one
with the same address and an equivalent key -- Acked-by and
Reviewed-by count as one approval family, so a literal
\"Acked-by:\" posted in the thread is never upgraded."
  :type '(set (const :tag "Acked. as Reviewed-by:" acked)
              (const :tag "Owned. as Reviewed-by:" owned)
              (const :tag "Archived page as Link:" link))
  :group 'gnaw)

(defcustom gnaw-checkout-base 'ask
  "Whether `gnaw-apply-patches' checks out the base commit first.
When a patch carries a `base-commit:' trailer (`git format-patch
--base') that exists in the target repo, gnaw can check it out first.
Values: `ask' prompts, nil never, t always.  (`gnaw-am-patches'
instead proposes the base commit as the start of the branch it
creates.)"
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Never" nil)
                 (const :tag "Always" t))
  :group 'gnaw)

(defcustom gnaw-save-no-confirm nil
  "When non-nil, the save commands write their files without asking.
They save into the source's first configured `:repo' (or
`gnaw-apply-repo') without prompting for a directory, and
overwrite existing files silently.  A prefix argument on a save
command inverts this setting for that call: it silences the prompts
when this is nil, and restores them when it is non-nil."
  :type 'boolean
  :group 'gnaw)

(defun gnaw--save-no-confirm-p (arg)
  "Return the effective no-confirm flag for a save command.
ARG, the command's prefix argument, inverts `gnaw-save-no-confirm'."
  (xor gnaw-save-no-confirm arg))

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

(defun gnaw--person-trailer (key addr name)
  "Return \"KEY: NAME <ADDR>\", without NAME when nil or empty.
Return nil without ADDR."
  (and (stringp addr) (not (string-empty-p addr))
       (format "%s: %s" key
               (if (and name (not (string-empty-p name)))
                   (format "%s <%s>" name addr)
                 addr))))

(defun gnaw--synthetic-trailers (info)
  "Return the trailers derived from report INFO's state.
Per `gnaw-am-synthetic-trailers': the addresses posted by the
Acked. and Owned. commands become Reviewed-by: lines, and the
report's archived web page a Link: line."
  (let ((wanted gnaw-am-synthetic-trailers))
    (delq nil
          (list (and (memq 'acked wanted)
                     (gnaw--person-trailer "Reviewed-by"
                                           (plist-get info :acked)
                                           (plist-get info :acked-name)))
                (and (memq 'owned wanted)
                     (gnaw--person-trailer "Reviewed-by"
                                           (plist-get info :owned)
                                           (plist-get info :owned-name)))
                (and (memq 'link wanted)
                     (let ((url (plist-get info :archived-at)))
                       (and (stringp url) (not (string-empty-p url))
                            (concat "Link: " url))))))))

(defun gnaw--trailer-key-family (key)
  "Return trailer KEY's comparison family, downcased.
Acked-by and Reviewed-by are one approval family: a collected ack
covers a synthetic review of the same address, and vice versa."
  (let ((k (downcase key)))
    (if (member k '("acked-by" "reviewed-by")) "approval" k)))

(defun gnaw--trailer-addr (trailer)
  "Return TRAILER's address -- its <...> part, else its whole value --
downcased, or nil when TRAILER has no colon."
  (when-let* ((colon (string-search ":" trailer)))
    (let ((val (string-trim (substring trailer (1+ colon)))))
      (downcase (if (string-match "<\\([^>]+\\)>" val)
                    (match-string 1 val)
                  val)))))

(defun gnaw--merge-trailers (collected synthetic)
  "Append the SYNTHETIC trailers not already covered by COLLECTED.
A synthetic trailer is covered when a collected one carries the
same address or URL (compared whole, case-insensitively) under a
key of the same family (see `gnaw--trailer-key-family'), so an
\"Acked-by: Admin <a@x>\" collected from a reply suppresses the
state-derived \"Reviewed-by: a@x\" instead of being upgraded by
it."
  (append collected
          (seq-remove
           (lambda (tr)
             (let ((fam (gnaw--trailer-key-family
                         (substring tr 0 (string-search ":" tr))))
                   (addr (gnaw--trailer-addr tr)))
               (seq-some
                (lambda (c)
                  (when-let* ((ccolon (string-search ":" c)))
                    (and (equal fam (gnaw--trailer-key-family
                                     (substring c 0 ccolon)))
                         (equal addr (gnaw--trailer-addr c)))))
                collected)))
           synthetic)))

(defun gnaw--fold-trailers-file (file trailers)
  "Return a temp copy of patch FILE with TRAILERS folded in.
TRAILERS are \"Key: Name <addr>\" strings; those already present in
the commit message (case-insensitively) are skipped.  The missing
ones are inserted just before the \"---\" separator that ends the
message.  Return FILE itself when nothing is missing or the file has
no separator (then there is no commit message to extend safely)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (not (re-search-forward "^---$" nil t))
        file
      (let* ((sep (match-beginning 0))
             (msg (buffer-substring (point-min) sep))
             (case-fold-search t)
             ;; Whitespace-insensitive dedup: "Reviewed-by:  Bob <b@x>"
             ;; and "Reviewed-by: Bob <b@x>" are the same trailer.
             (missing (seq-remove
                       (lambda (tr)
                         (string-match-p
                          (concat "^"
                                  (mapconcat #'regexp-quote (split-string tr)
                                             "[ \t]+")
                                  "[ \t]*$")
                          msg))
                       trailers)))
        (if (not missing)
            file
          (goto-char sep)
          (dolist (tr missing) (insert tr "\n"))
          (let ((tmp (make-temp-file "gnaw-am-" nil ".patch")))
            (write-region nil nil tmp nil 'silent)
            tmp))))))

(defun gnaw--shorten-subject (subject)
  "Shorten SUBJECT into a branch-name fragment like \"fix-tangle-noweb\".
Drops any quote characters, then the leading \"Re:\" and
\"[PATCH ...]\"-style tags, then keeps words (except a, an and the)
until exceeding five words or twenty characters, joined with dashes."
  (let ((words (split-string
                (downcase (replace-regexp-in-string
                           "\\`\\(?:[Rr][Ee]:\\|\\[[^]\n]*\\]\\|[ \t]\\)*"
                           ""
                           (replace-regexp-in-string "['\"]" "" subject)))
                "\\W+" t))
        (num-words 0) (num-chars 0) kept)
    (catch 'stop
      (dolist (word words)
        (unless (member word '("a" "an" "the"))
          (cl-incf num-words)
          (cl-incf num-chars (length word))
          (push word kept)
          (when (or (> num-words 5) (> num-chars 20))
            (throw 'stop nil)))))
    (string-join (nreverse kept) "-")))

(defun gnaw-branch-who-what-v (info)
  "Return a branch name like \"ec/fix-tangle-noweb__v2\" for report INFO.
\"e\" and \"c\" are the initials of the sender's first two names,
\"fix-tangle-noweb\" a shortened subject and \"v2\" the series
version announced in the subject, when present.  Return nil when
INFO's subject yields no words to name the branch after."
  (when-let* ((subject (plist-get info :subject))
              (what (gnaw--shorten-subject subject))
              ((not (string-empty-p what))))
    (let* ((sender (replace-regexp-in-string
                    "['\"]" ""
                    (let ((n (plist-get info :from-name)))
                      (if (and n (not (string-empty-p n))) n
                        (car (split-string (or (plist-get info :from) "") "@"))))))
           (initials (mapconcat (lambda (w) (downcase (substring w 0 1)))
                                (seq-take (split-string sender) 2)
                                ""))
           (version (and (string-match "\\[[^]\n]*PATCH \\(v[0-9]+\\)" subject)
                         (match-string 1 subject))))
      (concat (and (not (string-empty-p initials)) (concat initials "/"))
              what
              (and version (concat "__" version))))))

(defun gnaw-am-read-worktree-sibling-named-by-branch (repo branch)
  "Read a to-be-created worktree directory, proposed as a sibling of REPO.
The proposed name is REPO's name plus BRANCH, when one is given."
  (let ((fname (directory-file-name repo)))
    (read-directory-name
     "Create worktree: " (file-name-directory fname) nil nil
     (and branch (concat (file-name-nondirectory fname) "-"
                         (string-replace "/" "-" branch))))))

(defun gnaw--git (dir &rest args)
  "Run git ARGS in DIR and return the exit status.
The output replaces the contents of the *gnaw-git* buffer, which
callers display on failure."
  (with-current-buffer (get-buffer-create "*gnaw-git*")
    ;; An existing buffer keeps the default-directory of its first
    ;; use; re-point it at the current repo.
    (setq default-directory (file-name-as-directory dir))
    (let ((inhibit-read-only t)) (erase-buffer))
    (apply #'call-process "git" nil t nil args)))

(defun gnaw--commit-in-repo-p (commit)
  "Non-nil when COMMIT exists in the git repo of `default-directory'."
  (zerop (call-process "git" nil nil nil
                       "cat-file" "-e" (concat commit "^{commit}"))))

(defun gnaw--maybe-checkout-base (files)
  "Offer to check out the base commit recorded in patch FILES.
Runs in `default-directory' (the target repo) per `gnaw-checkout-base',
when the commit exists locally.  Leaves the repo on a detached HEAD;
signals a `user-error' if the checkout fails."
  (when gnaw-checkout-base
    (let ((base (gnaw--patch-base-commit files)))
      (when (and base
                 (gnaw--commit-in-repo-p base)
                 (or (eq gnaw-checkout-base t)
                     (y-or-n-p
                      (format "Check out base commit %s (detached HEAD) first? "
                              (substring base 0 (min 12 (length base)))))))
        (unless (zerop (gnaw--git default-directory "checkout" base))
          (display-buffer "*gnaw-git*")
          (user-error "Git checkout %s failed" base))))))

(defun gnaw--target-repo (info)
  "Return the git repo directory in which to apply INFO's patches.
The source's configured `:repo' -- asking which one when it lists
several -- else `gnaw-apply-repo', else a prompt."
  (file-name-as-directory
   (let ((repos (gnaw--source-repos info)))
     (cond ((cdr repos) (completing-read "Apply in git repo: " repos nil t
                                         nil nil (car repos)))
           (repos (car repos))
           (gnaw-apply-repo gnaw-apply-repo)
           (t (read-directory-name "Apply in git repo: "))))))

(defun gnaw--confirm-incomplete-series (info patches)
  "Ask before applying when INFO's series is explicitly incomplete.
A non-nil PATCHES is a deliberate subset: no confirmation then."
  (when (and (not patches)
             (not (gnaw--series-complete-p info))
             (not (yes-or-no-p "Patch series looks incomplete; apply anyway? ")))
    (user-error "Aborted")))

(defun gnaw-apply-patches (info &optional patches)
  "Apply INFO's patches to the working tree with `git apply'.
PATCHES restricts the operation to these `:patches' entries."
  (let ((files (gnaw--patch-files info "applying" patches)))
    (gnaw--confirm-incomplete-series info patches)
    (let ((default-directory (gnaw--target-repo info)))
      (gnaw--maybe-checkout-base files)
      (if (zerop (apply #'gnaw--git default-directory
                        (append (list "apply") gnaw-git-apply-options
                                (list "--") files)))
          (message "gnaw: git apply applied %d patch(es) in %s"
                   (length files) default-directory)
        (display-buffer "*gnaw-git*")
        (message "gnaw: git apply failed in %s (rejects left as .rej)"
                 default-directory)))))

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

(defun gnaw-am-patches (info &optional patches toggle-worktree)
  "Apply INFO's patches as commits with `git am'.
First prompt for a branch to create (empty input keeps a detached
HEAD) and for its start point (empty input starts from the current
HEAD; the patches' recorded `base-commit:' is proposed when the
repo has it).  When `gnaw-am-create-worktree', inverted by
TOGGLE-WORKTREE, is non-nil, branch and apply in a new worktree
read by `gnaw-am-read-worktree-function', leaving the repo's
checkout untouched.  The review trailers collected by BONE, plus
those synthesized from the report state per
`gnaw-am-synthetic-trailers', are folded into the commits per
`gnaw-am-fold-trailers'.  A successful run shows the directory
applied in, per `gnaw-am-show-repo'.  PATCHES restricts the
operation to these `:patches' entries."
  (let ((files (gnaw--patch-files info "applying" patches)))
    (gnaw--confirm-incomplete-series info patches)
    (let* ((repo (gnaw--target-repo info))
           (default-directory repo)
           (use-worktree (xor gnaw-am-create-worktree toggle-worktree))
           (branch (let ((b (read-string "New branch (empty for detached): "
                                         (funcall gnaw-am-branch-function info))))
                     (and (not (string-blank-p b)) b)))
           (base (completing-read
                  "Base commit (empty for HEAD): "
                  (let ((b (gnaw--patch-base-commit files)))
                    (and b (gnaw--commit-in-repo-p b) (list b)))))
           (am-dir (if use-worktree
                       (file-name-as-directory
                        (expand-file-name
                         (funcall gnaw-am-read-worktree-function repo branch)))
                     repo)))
      (when (and use-worktree (file-exists-p am-dir))
        (user-error "Worktree directory %s already exists" am-dir))
      (unless (zerop (apply #'gnaw--git repo
                            (append (if use-worktree
                                        (list "worktree" "add")
                                      (list "checkout"))
                                    (if branch (list "-b" branch) (list "--detach"))
                                    (and use-worktree (list am-dir))
                                    (and (not (string-blank-p base))
                                         (list base)))))
        (display-buffer "*gnaw-git*")
        (user-error "Git %s failed"
                    (if use-worktree "worktree add" "checkout")))
      (let ((trailers (and gnaw-am-fold-trailers
                           (delete-dups
                            (gnaw--merge-trailers
                             (plist-get info :trailers)
                             (gnaw--synthetic-trailers info)))))
            am-files)
        (unwind-protect
            (progn
              ;; Fold file by file, extending am-files as we go: should
              ;; a copy fail midway, the cleanup below still sees the
              ;; copies made before the error.
              (if (null trailers)
                  (setq am-files files)
                (dolist (f files)
                  (setq am-files
                        (cons (gnaw--fold-trailers-file f trailers) am-files)))
                (setq am-files (nreverse am-files)))
              (if (zerop (apply #'gnaw--git am-dir
                                (append (list "am") gnaw-git-am-options
                                        (list "--") am-files)))
                  (progn
                    (message "gnaw: git am applied %d patch(es) in %s%s%s"
                             (length files) am-dir
                             (if branch (format " on branch %s" branch)
                               " (detached HEAD)")
                             (if (equal am-files files) "" ", trailers folded"))
                    (when gnaw-am-show-repo
                      (if (fboundp 'magit-status-setup-buffer)
                          (magit-status-setup-buffer am-dir)
                        (dired am-dir))))
                (display-buffer "*gnaw-git*")
                (message "gnaw: git am failed in %s (run `git am --abort' to undo)"
                         am-dir)))
          ;; The folded files are temp copies; the cached originals stay.
          (dolist (f am-files)
            (unless (member f files) (delete-file f))))))))

(defun gnaw-save-patches (info &optional no-confirm patches)
  "Save INFO's patch files to a directory.
Prompt for the target directory, proposing the source's first
`:repo' (or `gnaw-apply-repo') when one is configured, and ask
before overwriting.  When NO-CONFIRM is non-nil, save to that
repo without prompting and overwrite silently.  PATCHES restricts
the operation to these `:patches' entries."
  (let* ((files (gnaw--patch-files info "saving" patches))
         (repo (or (gnaw--source-repo info) gnaw-apply-repo))
         (dir (if (and repo no-confirm)
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
Set by `gnaw-list-reload'.  Buffer-local in use.  Closed reports
are included when a source lists them, but stay hidden until a
query asks for them (see `gnaw-list--display-reports').")

(defvar gnaw-list--mark-index nil
  "Index of the Mark column among the active columns, nil when hidden.
Set by `gnaw--list-format'.  Buffer-local in use.")

(defface gnaw-sticky '((t :weight bold))
  "Face for sticky reports in the report list."
  :group 'gnaw)

(defface gnaw-dismissed '((t :inherit shadow))
  "Face for dismissed reports when they are shown in the report list."
  :group 'gnaw)

(defface gnaw-closed '((t :slant italic))
  "Face for closed reports, in the report list and the related view."
  :group 'gnaw)

(defface gnaw-missing '((t :inherit shadow :slant italic))
  "Face for related reports absent from the loaded sources.
Their placeholder rows are built from the relation metadata only."
  :group 'gnaw)

;; The cell faces below inherit standard faces instead of setting
;; colors, so the list follows the user's theme.  Sticky, dismissed and
;; closed rows override them with the whole-row faces computed by
;; `gnaw--row-faces'.

(defface gnaw-type-bug '((t :inherit font-lock-warning-face))
  "Face for the Type cell of bug reports."
  :group 'gnaw)

(defface gnaw-type-patch '((t :inherit font-lock-function-name-face))
  "Face for the Type cell of patch reports."
  :group 'gnaw)

(defface gnaw-type-other '((t :inherit font-lock-doc-face))
  "Face for the Type cell of reports of other types."
  :group 'gnaw)

(defface gnaw-date '((t :inherit shadow))
  "Face for the Created and Activity cells."
  :group 'gnaw)

(defface gnaw-acked '((t :inherit success))
  "Face for the A letter in the Flags cell."
  :group 'gnaw)

(defface gnaw-owned '((t :inherit warning))
  "Face for the O letter in the Flags cell."
  :group 'gnaw)

(defface gnaw-votes '((t :inherit bold))
  "Face for Votes cells with a non-zero score."
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
are resolved once, here; an unparseable SPEC or range bound matches
nothing -- an empty bound is open, an invalid one is an error, like
on the BONE web page."
  (let ((today (time-to-days (current-time)))
        (lo nil) (hi nil) (none nil))
    (if (string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" spec)
        ;; Read both ends before calling `gnaw--query-bound', which
        ;; clobbers the match data via its own `string-match'.
        (let* ((sa (match-string 1 spec))
               (sb (match-string 2 spec))
               (va (gnaw--query-bound sa forward today))
               (vb (gnaw--query-bound sb forward today)))
          (if (or (and (not (string-empty-p sa)) (null va))
                  (and (not (string-empty-p sb)) (null vb)))
              (setq none t)
            (setq lo (if (and va vb) (min va vb) va)
                  hi (if (and va vb) (max va vb) vb))))
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

(defun gnaw--query-number-bound (s)
  "Parse range bound S into an integer, or nil when S is not one."
  (and (string-match-p "\\`-?[0-9]+\\'" s) (string-to-number s)))

(defun gnaw--query-number-matcher (spec getter)
  "Compile numeric SPEC into a predicate on the number read by GETTER.
Each comma (OR) alternative of SPEC is an integer N or an A..B
range whose ends may be empty (the order is normalized), like the
date: ranges; an unparseable alternative or bound matches nothing.
GETTER maps an INFO plist to the number, nil when the report has
none -- which no alternative matches."
  (let ((ranges
         (delq nil
               (mapcar
                (lambda (alt)
                  (if (string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" alt)
                      (let* ((sa (match-string 1 alt))
                             (sb (match-string 2 alt))
                             (va (gnaw--query-number-bound sa))
                             (vb (gnaw--query-number-bound sb)))
                        (unless (or (and (not (string-empty-p sa)) (null va))
                                    (and (not (string-empty-p sb)) (null vb)))
                          (cons (if (and va vb) (min va vb) va)
                                (if (and va vb) (max va vb) vb))))
                    (when-let* ((n (gnaw--query-number-bound alt)))
                      (cons n n))))
                (gnaw--query-vals spec)))))
    (lambda (_mid info)
      (when-let* ((n (funcall getter info)))
        (seq-some (lambda (r)
                    (and (or (null (car r)) (>= n (car r)))
                         (or (null (cdr r)) (<= n (cdr r)))))
                  ranges)))))

(defun gnaw--query-actor-matcher (needle)
  "Compile query NEEDLE into a predicate on an actor (identity) string.
`*' or `true' (any case) matches any set actor; otherwise NEEDLE
matches as `gnaw--query-text-matcher' does, so /regexp/ and \"quoted\"
values work here too -- but never against an unset actor."
  (if (member (downcase needle) '("*" "true"))
      (lambda (a) (and a (not (string-empty-p a))))
    (let ((m (gnaw--query-text-matcher needle)))
      (lambda (a)
        (and a (not (string-empty-p a)) (funcall m a))))))

(defun gnaw--query-flag-matcher (val bit)
  "Compile VAL into a predicate testing priority BIT on INFO.
`*' and `true' require the flag, `false' its absence (any case);
any other VAL matches nothing."
  (let* ((val (downcase val))
         (want (and (member val '("*" "true")) t)))
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
      ((or "topic" "T")
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
      ((or "type" "t")
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
      ((or "votes" "v")
       (gnaw--query-number-matcher
        val (lambda (info)
              (when-let* ((v (plist-get info :votes)))
                (gnaw--votes-number v)))))
      ((or "msgs" "M")
       (gnaw--query-number-matcher
        val (lambda (info)   ; thread size, initial mail included
              (when-let* ((n (plist-get info :replies))) (1+ n)))))
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
      ;; source:O (a single character) matches the source letter of
      ;; the S column, or a one-character source name, exactly; a
      ;; longer value matches the source name as topic: does, and
      ;; source:* keeps its any-source meaning.
      ((or "source" "S")
       (gnaw--query-field-matcher
        val
        (lambda (v)
          (if (or (/= (length v) 1) (equal v "*"))
              (gnaw--query-text-matcher v)
            (let ((d (downcase v)))
              (lambda (s) (and s (equal d (downcase s)))))))
        (gnaw--query-getter :source-letter)
        (gnaw--query-getter :source-name)))
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

(defun gnaw--query-reveals-closed-p (query)
  "Non-nil when QUERY explicitly asks for closed reports.
That is, when an unnegated token filters on a closing flag (a
flags: value naming C, R, E or S) or on the closed: key -- the
tokens `gnaw-list-limit-closed' and the Flags cell filter produce.
`gnaw-list--display-reports' hides closed reports otherwise."
  (seq-some
   (lambda (tokens)
     (seq-some
      (lambda (token)
        (and (not (string-prefix-p "-" token))
             (let* ((i (string-search ":" token))
                    (key (and i (substring token 0 i)))
                    (val (and i (substring token (1+ i)))))
               (and val (not (string-empty-p val))
                    (or (member key '("closed" "c"))
                        (and (member key '("flags" "F"))
                             (string-match-p "[CRES]" (upcase val))))))))
      tokens))
   (gnaw--query-parse query)))

(defconst gnaw--query-keys
  '("from:" "subject:" "similar:" "topic:" "source:" "type:" "priority:"
    "votes:" "msgs:" "mid:" "acked:" "owned:" "closed:" "urgent:"
    "important:" "flags:" "att:" "date:" "deadline:" "expired:")
  "Long-form query keys completed in `gnaw-list-filter'.")

(defconst gnaw--query-text-keys
  '("from" "f" "subject" "s" "similar" "topic" "T" "mid" "m"
    "acked" "a" "owned" "o" "closed" "c")
  "Query keys taking free text, aliases included.
Keep in sync with `gnaw--query-compile-key'.  The live preview
holds their value back until it reaches
`gnaw-list-filter-live-min-chars'.")

(defconst gnaw--query-closed-keys
  '("source" "S" "type" "t" "priority" "p" "votes" "v" "msgs" "M"
    "urgent" "u" "important" "i" "flags" "F" "att" "attributes" "A"
    "date" "d" "deadline" "D" "expired" "e")
  "Query keys taking closed-set or short values, aliases included.
Keep in sync with `gnaw--query-compile-key'.  The live preview only
waits for their value to be non-empty.")

(defconst gnaw-report-types
  '("bug" "patch" "request" "announcement" "change" "release")
  "BONE report types, offered when filtering by type.")

(defun gnaw--filter-value-candidates (key)
  "Return completion candidates for the value of filter KEY, or nil.
Topics come from the reports of the list buffer the minibuffer was
entered from.  Candidates the tokenizer would split apart (a source
name with spaces) are offered quoted, so completing one yields a
working query."
  (mapcar
   #'gnaw--query-quote-val
   (pcase key
     ((or "type" "t") gnaw-report-types)
     ((or "source" "S")
      (let (cands)
        (dolist (p (buffer-local-value
                    'gnaw-list--reports
                    (window-buffer (minibuffer-selected-window))))
          (dolist (k '(:source-letter :source-name))
            (when-let* ((v (plist-get (cdr p) k)))
              (cl-pushnew v cands :test #'equal))))
        (nreverse cands)))
     ((or "topic" "T")
      (gnaw-topics (buffer-local-value
                    'gnaw-list--reports
                    (window-buffer (minibuffer-selected-window))))))))

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

(defvar-local gnaw-list--toggle-point nil
  "Point saved by `gnaw-list-filter-toggle' when it set its filter.
A (MID . LINE) cons restored when the same toggle clears its query.
Replacing one toggle's filter by another's keeps the saved point,
whose LINE was measured in the view the chain of toggles started
from -- the view a clear returns to.  `gnaw-list-filter' resets it
otherwise, so a non-nil value always dates from the active chain.")

(defvar-local gnaw-list--cell-filter nil
  "State of the `gnaw-list-filter-cell' toggle, or nil.
A list (QUERY PREV-QUERY PREV-RELATED-MIDS PREV-RELATED-ENTRIES MID
LINE): the query the command set, then the view it replaced and the
point position in it, restored when the command is called again
while QUERY is still active.  `gnaw-list-filter' resets it.")

(defun gnaw-list-filter-toggle (query &optional arg)
  "Set the list filter to QUERY, clearing it when QUERY is already active.
Clearing restores point to the report (or, failing that, the line)
it was on when the toggle -- or the chain of toggles it replaced --
set its filter.  In the related-reports view, whose entry query is
not the toggle's doing, always set the filter.  ARG is the raw
prefix argument: with one \\[universal-argument], add QUERY to the
active filter (AND) instead of replacing it, without toggling; with
two, toggle the negation -QUERY, excluding the matching reports
instead of keeping them; with three, add -QUERY to the active
filter (AND)."
  (when (and (consp arg) (>= (car arg) 16))
    (setq query (concat "-" query)
          arg (>= (car arg) 64)))
  (if (and (not arg) (not gnaw-list--related-mids)
           (equal gnaw-list--query query))
      (let ((saved gnaw-list--toggle-point)
            (col (current-column)))
        (gnaw-list-filter "")
        (when saved
          ;; `gnaw-list--goto-mid-or-line' lands on column 0: put the
          ;; cursor back on the column it was on, as a plain refresh does.
          (gnaw-list--goto-mid-or-line (car saved) (cdr saved))
          (move-to-column col)))
    (let ((saved (or gnaw-list--toggle-point
                     (cons (car (tabulated-list-get-id))
                           (line-number-at-pos)))))
      (gnaw-list-filter (gnaw-list--query-add query arg))
      (unless arg
        (setq gnaw-list--toggle-point saved)))))

(defun gnaw-list-filter-by (key &optional arg)
  "Limit the list to reports whose KEY field matches a read value.
Read the value (completing types and topics), then set the query to
`KEY:value'; an empty value clears the filter.  The flag fields
\(acked, owned, closed, urgent, important) read no value and toggle:
calling the command again while its filter is active clears it.
ARG is the raw prefix argument: non-nil adds the condition to the
active filter (AND) instead of replacing it, and on the flag fields
two \\[universal-argument] toggle the negation (-KEY:*), excluding
the matching reports instead of keeping them; three add that
negation to the active filter (AND)."
  (if (member key '("acked" "owned" "closed" "urgent" "important"))
      (gnaw-list-filter-toggle (concat key ":*") arg)
    (let ((val (cond ((equal key "type")
                      (completing-read "Type: " gnaw-report-types))
                     ((equal key "topic")
                      (completing-read "Topic: " (gnaw-topics gnaw-list--reports)))
                     ((equal key "flags")
                      (read-string "Flags letters, all required (A O C R E S): "))
                     ((equal key "att")
                      (read-string "Att glyphs, all required (. ~ + x @ #): "))
                     (t (read-string (format "%s: " key))))))
      (gnaw-list-filter
       (if (string-empty-p val) ""
         (gnaw-list--query-add
          (format "%s:%s" key (gnaw--query-quote-val val)) arg))))))

(eval-and-compile
  (defun gnaw--filter-prefix-doc (excluded)
    "Return the docstring paragraph on the filter prefix arguments.
EXCLUDED names the reports two prefix arguments exclude."
    (concat "With a prefix argument ARG, add the condition to the\n"
            "active filter (AND) instead of replacing it; with two\n"
            "prefix arguments, exclude " excluded "\n"
            "instead of keeping them; with three, add that exclusion\n"
            "to the active filter (AND).")))

(defmacro gnaw--define-filter-commands (&rest fields)
  "Define a `gnaw-list-filter-FIELD' command for each of FIELDS."
  `(progn
     ,@(mapcar (lambda (f)
                 `(defun ,(intern (concat "gnaw-list-filter-" f)) (&optional arg)
                    ,(concat "Filter the report list by the " f " field.\n"
                             (if (member f '("acked" "owned" "closed"
                                             "urgent" "important"))
                                 (concat
                                  "Calling it again while its filter is active clears it.\n"
                                  (gnaw--filter-prefix-doc
                                   (concat "the " f " reports (-" f ":*)")))
                               (concat
                                "With a prefix argument ARG, add the condition to\n"
                                "the active filter (AND) instead of replacing it.")))
                    (interactive "P")
                    (gnaw-list-filter-by ,f arg)))
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
    ("Votes"     5 gnaw--votes-sort :votes)
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
the high-signal short codes (votes, flags, type), then identity, then
the flexible subject, then the creation date as the rightmost time
anchor.  The Pri (priority), Activity (last activity) and Topic columns
are left out by default to reduce noise. If you want to re-add them:
  (\"Pri\"       4 gnaw--priority-sort :priority)
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
The Flags and Att cells carry a help echo spelling them out, and the
S cell the full source name, which `tabulated-list-print-col'
preserves.  Every cell carries a
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
    (:type (let ((s (or (plist-get info :type) "")))
             (propertize s 'face (pcase s
                                   ("bug" 'gnaw-type-bug)
                                   ("patch" 'gnaw-type-patch)
                                   (_ 'gnaw-type-other)))))
    (:votes (let* ((v (plist-get info :votes))
                   (s (if v (format "%s" v) "")))
              (if (> (gnaw--votes-number v) 0)
                  (propertize s 'face 'gnaw-votes)
                s)))
    (:flags (let ((f (copy-sequence (or (plist-get info :flags) ""))))
              (dotimes (i (length f))
                (pcase (aref f i)
                  (?A (put-text-property i (1+ i) 'face 'gnaw-acked f))
                  (?O (put-text-property i (1+ i) 'face 'gnaw-owned f))))
              (if-let* ((help (gnaw--flags-help info)))
                  (propertize f 'help-echo (concat "Flags: " help))
                f)))
    (:source-letter
     (let ((l (or (plist-get info :source-letter) "")))
       (if-let* ((name (plist-get info :source-name)))
           (propertize l 'help-echo (concat "Source: " name))
         l)))
    (:msgs (let ((n (plist-get info :replies)))   ; thread size, initial mail included
             (if n (number-to-string (1+ n)) "")))
    (:priority (gnaw-priority-letter (plist-get info :priority)))
    (:date (let ((d (plist-get info :date)))   ; keep the YYYY-MM-DD part only
             (if d (propertize (substring d 0 (min 10 (length d)))
                               'face 'gnaw-date)
               "")))
    (:last-activity (let ((d (plist-get info :last-activity)))
                      (if d (propertize (substring d 0 (min 10 (length d)))
                                        'face 'gnaw-date)
                        "")))
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
  "Return `gnaw-list-columns' minus those named in config `:skip-columns'.
With several configured sources, prepend the S column, which shows
the letter identifying each report's source (the `:letter' of its
config.edn entry, see `gnaw-add-source')."
  (let ((skip (mapcar #'downcase (plist-get (gnaw-load-config) :skip-columns)))
        (cols (if (gnaw--multi-source-p)
                  (cons '("S" 2 t :source-letter) gnaw-list-columns)
                gnaw-list-columns)))
    (cl-remove-if (lambda (c) (member (downcase (car c)) skip)) cols)))

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

(defun gnaw--report-cells (mid info entry cols)
  "Return the row cell strings for a report.
MID, INFO and ENTRY are the report's message-id, plist and state
entry; COLS the active columns."
  (mapcar (lambda (c) (gnaw--list-cell (nth 3 c) info entry mid)) cols))

(defun gnaw--row-faces (entry info)
  "Return the whole-row face list for a report, nil when plain.
ENTRY is the report's state.edn entry, INFO its report plist.  The
sticky or dismissed mark face combines with `gnaw-closed' when the
report is closed, so a marked closed row keeps both cues."
  (delq nil
        (list (cond ((assq :sticky entry) 'gnaw-sticky)
                    ((assq :dismiss entry) 'gnaw-dismissed))
              (and (gnaw--closed-p info) 'gnaw-closed))))

(defun gnaw--list-entries-related ()
  "Return `tabulated-list-entries' for the related-reports view.
Show the reports of `gnaw-list--related-mids' only, closed ones in
italic, ignoring the filter query and the dismissed filter.  Related
reports absent from the loaded sources (e.g. closed ones when the
source is an open-reports JSON) get a placeholder row built from the
relation metadata, shown in `gnaw-missing' face."
  (let ((state (gnaw--state-table))
        (cols (gnaw--active-columns))
        found rows)
    (dolist (p gnaw-list--reports)
      (when (member (car p) gnaw-list--related-mids)
        (push (car p) found)
        (let* ((entry (gethash (car p) state))
               (faces (gnaw--row-faces entry (cdr p)))
               (cells (gnaw--report-cells (car p) (cdr p) entry cols)))
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
                             :source-letter (plist-get origin :source-letter)
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

(defun gnaw-list--display-reports ()
  "Return the loaded pairs the list may display under the current query.
Open reports always; closed ones only when the query asks for
them (see `gnaw--query-reveals-closed-p'), as `gnaw-list-limit-closed'
or a flags: filter naming a close flag does."
  (if (and gnaw-list--query
           (gnaw--query-reveals-closed-p gnaw-list--query))
      gnaw-list--reports
    (cl-remove-if (lambda (p) (gnaw--closed-p (cdr p)))
                  gnaw-list--reports)))

(defun gnaw--list-entries-full ()
  "Return `tabulated-list-entries' for the full report list.
Closed reports enter the list only through a query asking for
them (see `gnaw-list--display-reports')."
  (let ((state (gnaw--state-table))
        (cols (gnaw--active-columns))
        (qgroups (and gnaw-list--query (gnaw--query-compile gnaw-list--query)))
        (pairs (gnaw-list--display-reports))
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
                (let* ((entry   (gethash (car pair) state))
                       (dismiss (and (assq :dismiss entry) t)))
                  (when (and (or gnaw-list--show-dismissed (not dismiss))
                             (or (null qgroups)
                                 (seq-some
                                  (lambda (mp)
                                    (gnaw--query-match-p qgroups (car mp) (cdr mp)))
                                  (or match-pairs (list pair)))))
                    (let ((cells (gnaw--report-cells (car pair) (cdr pair)
                                                     entry cols))
                          (faces (gnaw--row-faces entry (cdr pair))))
                      (when faces
                        (setq cells (mapcar (lambda (s) (propertize s 'face faces))
                                            cells)))
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
    (define-key map "w" #'gnaw-list-copy-archive-url)
    (define-key map "L" #'gnaw-list-store-link)
    (define-key map [mouse-2] #'gnaw-list-mouse-open)
    ;; Let mouse-1 follow the rows, which carry mouse-face (the
    ;; package-menu convention).
    (define-key map [follow-link] 'mouse-face)
    (define-key map "f" #'gnaw-list-follow-mode)
    (define-key map (kbd "TAB") #'gnaw-list-tab)
    (define-key map "v" #'gnaw-list-attachment-view)
    (define-key map "V" #'gnaw-list-attachment-save)
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
    (define-key map (kbd "C-/") #'gnaw-list-undo)
    (define-key map (kbd "C-_") #'gnaw-list-undo)
    (define-key map "h" #'gnaw-show-help)
    (define-key map "s" #'gnaw-list-sort)
    (define-key map "S" #'gnaw-list-sort)
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
  ;; The native refresh entry points: C-x x g, auto-revert...  Pass
  ;; NO-ASK: an auto-revert timer must not prompt for source letters.
  (setq-local revert-buffer-function
              (lambda (&rest _) (gnaw-list-reload t)))
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

(defun gnaw--anchor-cells (entries)
  "Truncate ENTRIES' cells that would fill or overflow their column.
`tabulated-list-print-col' re-anchors the display on the column
grid (a `:align-to' display property) in a cell's trailing padding;
a cell filling its column gets no padding, so when the buffer text
is scaled away from the frame's canonical character width (e.g. by
`text-scale-adjust'), everything after that cell drifts off the
grid.  Truncating such cells one column short keeps an anchored
padding space on every cell.  The last column never pads: leave it
whole."
  (let ((fmt tabulated-list-format))
    (dolist (e entries entries)
      (let ((cells (cadr e)))
        (dotimes (i (1- (length fmt)))
          (let ((w (nth 1 (aref fmt i)))
                (s (aref cells i)))
            (when (>= (string-width s) w)
              (aset cells i
                    (truncate-string-to-width s (1- w) nil nil t)))))))))

(defun gnaw-list-refresh (&optional update)
  "Re-render the list from the in-memory reports and current state.
Does not re-read the report cache; use `gnaw-list-reload' for that.
Non-nil UPDATE only prints the rows that appeared or disappeared,
which is much faster when few did.  A row whose report survived is
never reprinted, even when its content or faces changed -- that is
`tabulated-list-print's update contract -- so only the live filter
preview may pass it, where a row only ever appears or disappears
whole; every command changing rows in place needs the full render."
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
  (let* ((win (get-buffer-window nil t))
         (start (and win (window-start win)))
         ;; From bol to bol: `count-lines' counts a partial line as one.
         (wline (and win (count-lines start (line-beginning-position))))
         ;; `tabulated-list-print' only restores the column when the
         ;; current row survives the reprint; when a filter removes it,
         ;; point lands on the first row at column 0.  Landing on the
         ;; first row is expected -- losing the column is not.
         (col (current-column)))
    (setq tabulated-list-format (gnaw--list-format))
    ;; The S column comes and goes with the number of sources: a sort
    ;; key naming a gone column would abort `tabulated-list-print',
    ;; so fall back on the default sort, as `gnaw-list-mode' does.
    (when (and tabulated-list-sort-key
               (not (assoc (car tabulated-list-sort-key)
                           (gnaw--active-columns))))
      (setq tabulated-list-sort-key
            (and gnaw-list-sort-key
                 (assoc (car gnaw-list-sort-key) (gnaw--active-columns))
                 gnaw-list-sort-key)))
    (tabulated-list-init-header)
    (gnaw-list--update-mode-line)
    (setq tabulated-list-entries (gnaw--anchor-cells (gnaw--list-entries)))
    (tabulated-list-print t update)
    (move-to-column col)
    (force-mode-line-update)
    (when win
      ;; Reprinting moved the window's own point; put it back on the
      ;; report `tabulated-list-print' remembered, selected or not.
      (set-window-point win (point))
      ;; A filter can shrink the list into fewer rows than the window
      ;; holds; the saved start, taken in the taller view, would then
      ;; anchor the window on the last rows with the other matches
      ;; hidden above it (or on blank space past the end).  When the
      ;; whole list fits, show it from the top instead.
      (when (<= (count-lines (point-min) (point-max))
                (window-body-height win))
        (setq start (point-min)))
      (set-window-start win start t)
      (unless (pos-visible-in-window-p (point) win)
        (with-selected-window win (recenter wline))))))

(defun gnaw-list-reload (&optional no-ask)
  "Re-read reports from the local cache, then re-render.
With several sources, first ask for the letters identifying them in
the S column when config.edn does not define some (see
`gnaw--ensure-source-letters') -- unless NO-ASK is non-nil, as on
the `revert-buffer' path, where auto-revert timers must not block
on a minibuffer prompt.  Reset the subject-words cache, then
re-warm it once Emacs has been idle for a second, so the first
similar: filter finds it filled."
  (interactive)
  (unless no-ask (gnaw--ensure-source-letters))
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
  (gnaw-list-filter ""))

(defcustom gnaw-list-filter-live t
  "Whether `gnaw-list-filter' previews its results while typing.
When nil, the list only refreshes once the query is confirmed."
  :type 'boolean
  :group 'gnaw)

(defcustom gnaw-list-filter-live-delay 0.2
  "Idle seconds before `gnaw-list-filter' previews the query being typed.
Each pause this long refreshes the list with what the minibuffer
contents match, a half-typed trailing token left out (see
`gnaw--filter-preview-query')."
  :type 'number
  :group 'gnaw)

(defcustom gnaw-list-filter-live-min-chars 3
  "Minimum needle length before it takes part in the live preview.
A shorter bare word or free-text value matches too many subjects to
make a useful preview; the list stays put until this many
characters are typed.  Closed-set values (type:, priority:...),
`*', `true', \"quoted\" and complete /regexp/ needles are not held
back by this limit."
  :type 'integer
  :group 'gnaw)

(defun gnaw--filter-preview-token (tok)
  "Return the previewable part of trailing query token TOK, or nil.
TOK is still being typed, and previewing it too early misleads: a
known key awaiting its value, an open \"quote and an open /regexp/
match nothing by construction, and a free-text needle shorter than
`gnaw-list-filter-live-min-chars' matches too much.  An incomplete
last comma (OR) alternative drops off alone when complete ones
precede it; nil means TOK contributes nothing yet."
  (let* ((case-fold-search nil)
         (bare (if (string-prefix-p "-" tok) (substring tok 1) tok))
         (colon (string-search ":" bare))
         (key (and colon (substring bare 0 colon)))
         (text (and key (member key gnaw--query-text-keys) t))
         (closed (and key (member key gnaw--query-closed-keys) t))
         ;; The needle being typed: a known key's value, or the whole
         ;; token (a bare word keeps its commas, see
         ;; `gnaw--query-subject-matcher').
         (needle (if (or text closed) (substring bare (1+ colon)) bare)))
    (cond
     ;; A known key with an empty value matches nothing.
     ((and (or text closed) (string-empty-p needle)) nil)
     ;; An open quote swallows the rest of the query.
     ((cl-oddp (cl-count ?\" bare)) nil)
     ((or closed
          (gnaw--query-regexp needle)
          (gnaw--query-quoted needle))
      tok)
     (t
      ;; Free text: judge the alternative being typed, the last one.
      (let ((alt (if text (car (last (split-string needle ","))) needle)))
        (cond
         ((or (string-empty-p alt)      ; a trailing comma changes nothing
              (member alt '("*" "true"))
              (gnaw--query-quoted alt)
              (gnaw--query-regexp alt)
              (and (not (string-prefix-p "/" alt)) ; no open regexp
                   (>= (length alt) gnaw-list-filter-live-min-chars)))
          tok)
         ;; Complete alternatives before the open one still preview;
         ;; recheck them, the comma split is blind to an open regexp.
         ((and text (> (- (length needle) (length alt) 1) 0))
          (gnaw--filter-preview-token
           (substring tok 0 (- (length tok) (length alt) 1))))
         (t nil)))))))

(defun gnaw--filter-preview-query (query)
  "Return QUERY as the live preview should run it, or nil for none.
The trailing token, still being typed, only joins the preview once
it can match something sensible (`gnaw--filter-preview-token');
until then the preview runs the rest of QUERY.  Return nil when
nothing previewable remains."
  (let* ((beg (gnaw--filter-token-start query))
         (tok (substring query beg))
         (q (concat (substring query 0 beg)
                    (if (string-empty-p tok) ""
                      (or (gnaw--filter-preview-token tok) "")))))
    (and (not (string-empty-p (string-trim q))) q)))

(defun gnaw--filter-read ()
  "Read a filter query for the current report list, previewing it live.
Pauses of `gnaw-list-filter-live-delay' seconds while typing show
in the list what the query matches (see `gnaw--filter-preview-query';
tokens the preview leaves out fall back to the active filter).
Aborting the minibuffer restores the view the preview replaced."
  (let* ((buf (current-buffer))
         (orig-query gnaw-list--query)
         (orig-related gnaw-list--related-mids)
         (shown orig-query)
         timer dirty committed full)
    (cl-labels
        ((preview ()
           (setq timer nil)
           (if (input-pending-p)        ; still typing: do not even start
               (schedule)
             (when-let* ((mini (active-minibuffer-window))
                         (win (get-buffer-window buf)))
               (let ((query (or (gnaw--filter-preview-query
                                 (with-current-buffer (window-buffer mini)
                                   (minibuffer-contents-no-properties)))
                                orig-query)))
                 (unless (equal query shown)
                   ;; Typing stays responsive: `while-no-input' aborts
                   ;; the render as soon as a key arrives, leaving it
                   ;; partial, and the retry finishes it at the next
                   ;; pause.
                   (setq dirty t)
                   ;; Leaving the related view changes the face of rows
                   ;; whose text stays equal, which the row-diffing
                   ;; update render would skip: reprint everything until
                   ;; one full render completes.  The timer runs in the
                   ;; minibuffer, so read the flag in the list buffer.
                   (when (buffer-local-value 'gnaw-list--related-mids buf)
                     (setq full t))
                   (if (eq t (while-no-input
                               (with-selected-window win
                                 (setq-local gnaw-list--related-mids nil)
                                 (setq-local gnaw-list--query query)
                                 (gnaw-list-refresh (not full))
                                 nil)))
                       (schedule)
                     (setq shown query
                           full nil)))))))
         (schedule (&rest _)
           (when (timerp timer) (cancel-timer timer))
           (setq timer (run-with-idle-timer
                        gnaw-list-filter-live-delay nil #'preview))))
      (unwind-protect
          (prog1
              (minibuffer-with-setup-hook
                  (lambda ()
                    (when gnaw-list-filter-live
                      (add-hook 'after-change-functions #'schedule nil t)))
                (let ((minibuffer-local-completion-map
                       (let ((m (copy-keymap minibuffer-local-completion-map)))
                         (define-key m " " nil) ; let SPACE separate tokens
                         (define-key m "?" nil) ; and `?' self-insert
                         m)))
                  (completing-read "Filter: " #'gnaw--filter-completion
                                   nil nil orig-query)))
            (setq committed t))
        (when (timerp timer) (cancel-timer timer))
        (when (and dirty (not committed) (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local gnaw-list--query orig-query)
            (setq-local gnaw-list--related-mids orig-related)
            ;; Re-entering the related view changes the face of
            ;; text-equal rows, which the update render would keep
            ;; stale: reprint everything in that case.
            (let ((update (null orig-related)))
              (if-let* ((win (get-buffer-window buf)))
                  (with-selected-window win (gnaw-list-refresh update))
                (gnaw-list-refresh update)))))))))

(defun gnaw-list-filter (query)
  "Filter the report list by QUERY; an empty QUERY clears the filter.
QUERY combines `key:value' tokens with spaces (AND) and `|' (OR).
While the query is typed, the list previews its matches (see
`gnaw-list-filter-live' and `gnaw-list-filter-live-delay').  With a
prefix argument, clear the active filter without prompting."
  (interactive
   (list (if current-prefix-arg "" (gnaw--filter-read))))
  (setq-local gnaw-list--related-mids nil)  ; filtering leaves the related view
  ;; Any query change ends the filter toggles; they re-save afterwards.
  (setq gnaw-list--toggle-point nil)
  (setq gnaw-list--cell-filter nil)
  (setq-local gnaw-list--query
              (and (not (string-empty-p (string-trim query))) query))
  (gnaw-list-refresh)
  (if gnaw-list--query
      (message "gnaw: filter %s" gnaw-list--query)
    (message "gnaw: filter cleared")))

(defun gnaw-list-update (&optional force)
  "Refresh the remote cache in the background, then reload the list.
With a prefix argument FORCE, re-download sources unconditionally.
Emacs stays responsive while the sources download; the list reloads
itself once the update finished.  Any missing source letters are
asked for now, while the user is at the keyboard: the reload runs
in a callback, outside any interaction, so it must not prompt."
  (interactive "P")
  (gnaw--ensure-source-letters)
  (let ((buf (current-buffer)))
    (gnaw-update force
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (gnaw-list-reload t)))))))

(defun gnaw-list-limit-type (&optional add)
  "Limit the list to a chosen report type.
With a prefix argument ADD, add the condition to the active
filter (AND) instead of replacing it."
  (interactive "P")
  (gnaw-list-filter-by "type" add))

(defmacro gnaw--define-limit-commands (&rest specs)
  "Define a `gnaw-list-limit-NAME' toggle command for each of SPECS.
Each spec is (NAME QUERY SUMMARY EXCLUDED): the command toggles a
filter on QUERY, SUMMARY is its docstring's first line, and EXCLUDED
names the reports two prefix arguments exclude."
  `(progn
     ,@(mapcar (pcase-lambda (`(,name ,query ,summary ,excluded))
                 `(defun ,(intern (concat "gnaw-list-limit-" name)) (&optional arg)
                    ,(concat summary "\n"
                             "Calling it again while its filter is active clears it.\n"
                             (gnaw--filter-prefix-doc excluded))
                    (interactive "P")
                    (gnaw-list-filter-toggle ,query arg)))
               specs)))

(gnaw--define-limit-commands
 ("closed" "flags:C,R,E,S"
  "Limit the list to closed reports, whatever the close reason."
  "the closed reports")
 ("awaiting" "att:."
  "Limit the list to reports awaiting a reply."
  "the awaiting reports")
 ("related" "att:~"
  "Limit the list to reports with related reports."
  "the reports with related reports")
 ("attachments" "att:+,x,@,#"
  "Limit the list to reports carrying at least one attachment."
  "the reports with attachments"))

(defun gnaw-list-filter-cell (&optional arg)
  "Toggle a filter built from the value of the cell at point.
On the S column, keep the reports of that source; on From, the
author's reports; on Type, the reports of that type; on Votes, the
reports with at least that vote score; on Flags, the reports
carrying all the cell's flags letters; on Att, the reports carrying
all the cell's glyphs; on Msgs, the threads with at least as many
messages; on Created, the reports created on or after that date;
on Subject, the reports with a similar subject (at least three
significant words in common, see `gnaw--query-similar-matcher') --
signaling a `user-error' instead of filtering when no other report
has a similar subject.
Outside any cell (the leading padding or past the last column, where
point commonly rests), fall back on the Subject column.  While the
filter set by this command is active, calling it again without a
prefix argument restores the view the filter replaced (a query, a
related-reports narrowing, or the full list) and puts point back on
the report it was on.  ARG is the raw prefix argument: with
one \\[universal-argument], add the condition to the active
filter (AND) instead of replacing it; with two, filter on the
negated condition, excluding the matching reports instead of keeping
them; with three, add that exclusion to the active filter (AND)."
  (interactive "P")
  (if (and (not arg) gnaw-list--cell-filter
           (equal gnaw-list--query (car gnaw-list--cell-filter)))
      (pcase-let ((`(,_ ,query ,mids ,entries ,mid ,line)
                   gnaw-list--cell-filter)
                  (col (current-column)))
        (setq gnaw-list--cell-filter nil)
        (setq-local gnaw-list--query query)
        (setq-local gnaw-list--related-mids mids)
        (setq-local gnaw-list--related-entries entries)
        (gnaw-list-refresh)
        ;; `gnaw-list--goto-mid-or-line' lands on column 0: put the
        ;; cursor back on the column it was on, as a plain refresh does.
        (gnaw-list--goto-mid-or-line mid line)
        (move-to-column col)
        (message "gnaw: %s" (cond (mids "back to the related view")
                                  (query (format "filter %s" query))
                                  (t "filter cleared"))))
    (let ((negate (and (consp arg) (>= (car arg) 16)))
          (add (if (and (consp arg) (>= (car arg) 16))
                   (>= (car arg) 64)
                 arg))
          (info (cdr (gnaw-list--current)))
          (col (or (get-text-property (point) 'tabulated-list-column-name)
                   ;; End of line: the cell just before point.
                   (and (> (point) (line-beginning-position))
                        (get-text-property (1- (point))
                                           'tabulated-list-column-name))
                   "Subject"))
          (prev (list gnaw-list--query gnaw-list--related-mids
                      gnaw-list--related-entries
                      (car (tabulated-list-get-id))
                      (line-number-at-pos))))
      (gnaw-list-filter
       (gnaw-list--query-add
        (concat
         (and negate "-")
         (pcase col
           ("S"
            (let ((l (plist-get info :source-letter)))
              (when (member l '(nil ""))
                (user-error "No source letter on this row"))
              (format "S:%s" l)))
           ("From"
            (let ((from (or (plist-get info :from)
                            (plist-get info :from-name))))
              (when (member from '(nil ""))
                (user-error "No author on this row"))
              (format "from:%s" (gnaw--query-quote-val from))))
           ("Type" (format "type:%s" (or (plist-get info :type) "bug")))
           ("Votes"
            (let ((v (plist-get info :votes)))
              (unless v (user-error "No votes on this row"))
              (format "votes:%d.." (gnaw--votes-number v))))
           ("Flags"
            (let ((f (replace-regexp-in-string
                      "-" "" (or (plist-get info :flags) ""))))
              (when (string-empty-p f)
                (user-error "No flags on this row"))
              (format "flags:%s" f)))
           ("Att"
            (let ((glyphs (string-replace " " "" (gnaw--att-string info))))
              (when (string-empty-p glyphs)
                (user-error "No attributes on this row"))
              (format "att:%s" glyphs)))
           ("Msgs"
            (let ((n (plist-get info :replies)))
              (unless n (user-error "No messages on this row"))
              (format "msgs:%d.." (1+ n))))
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
                          (gnaw-list--display-reports))
                         2)
                  (user-error "No other report with a similar subject"))
                (format "similar:%s" val))))
           (_ (user-error "No cell filter for the %s column" col))))
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

(defun gnaw-list--archive-url ()
  "Return the archive URL of the report at point, or signal a `user-error'."
  (pcase-let ((`(,mid . ,info) (gnaw-list--current)))
    (or (gnaw-message-archive-url mid info)
        (user-error "No archive URL for this report"))))

(defun gnaw-list-browse ()
  "Browse the archived web page of the report at point."
  (interactive)
  (browse-url (gnaw-list--archive-url)))

(defun gnaw-list-copy-archive-url ()
  "Copy the archived web page URL of the report at point to the kill ring."
  (interactive)
  (let ((url (gnaw-list--archive-url)))
    (kill-new url)
    (message "gnaw: copied %s" url)))

;; Org links: gnaw:MID opens the report's email with the method
;; configured in `gnaw-open-message-method', wherever the link lives.

(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function org-link-store-props "ol" (&rest plist))
(declare-function org-store-link "ol" (arg &optional interactive?))

(defun gnaw-org-follow-link (mid _prefix)
  "Open the email of the report with message-id MID, from a gnaw: link.
Look MID up in the reports a live list buffer already holds in
memory; without one, re-read the cached sources."
  (let* ((mid (gnaw-normalize-mid mid))
         (pair (or (seq-some
                    (lambda (buf)
                      (with-current-buffer buf
                        (and (derived-mode-p 'gnaw-list-mode)
                             (assoc mid gnaw-list--reports))))
                    (buffer-list))
                   (assoc mid (gnaw-reports)))))
    (unless pair
      (user-error "No report %s in the gnaw cache" mid))
    (gnaw-read-message (car pair) (cdr pair))))

(defun gnaw-org-store-link (&optional _interactive?)
  "Store an Org link to the report at point in the gnaw list."
  (when-let* (((derived-mode-p 'gnaw-list-mode))
              (pair (tabulated-list-get-id)))
    (org-link-store-props
     :type "gnaw"
     :link (concat "gnaw:" (gnaw--strip-mid (car pair)))
     :description (plist-get (cdr pair) :subject))))

(with-eval-after-load 'ol
  (org-link-set-parameters "gnaw"
                           :follow #'gnaw-org-follow-link
                           :store #'gnaw-org-store-link))

(defun gnaw-list-store-link ()
  "Store an Org link to the report at point.
Insert it in an Org buffer with `org-insert-link' (C-c C-l)."
  (interactive)
  (require 'ol)
  (call-interactively #'org-store-link))

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

(defun gnaw-list-patch-am (&optional toggle-worktree)
  "Apply the target patches with `git am', on a branch it offers to create.
A prefix argument TOGGLE-WORKTREE inverts `gnaw-am-create-worktree'
for this call."
  (interactive "P")
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (gnaw-am-patches info patches toggle-worktree)))

(defun gnaw-list-patch-save (&optional arg)
  "Save the target patch files to a directory.
The directory prompt proposes the source's configured `:repo' (or
`gnaw-apply-repo'), and existing files ask before being overwritten,
unless `gnaw-save-no-confirm' says otherwise; a prefix argument ARG
inverts that setting for this call."
  (interactive "P")
  (pcase-let ((`(,info . ,patches) (gnaw--patch-target)))
    (gnaw-save-patches info (gnaw--save-no-confirm-p arg) patches)))

(transient-define-prefix gnaw-list-patch-transient (info &optional patches)
  "Act on PATCHES of report INFO -- all of its patches when nil."
  [["Patches"
    ("v" "View in diff-mode" gnaw-list-patch-view)
    ("a" "Apply (git apply)" gnaw-list-patch-apply)
    ("m" "Apply as commits (git am; C-u: worktree)" gnaw-list-patch-am)
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

(defun gnaw-list-attachment-save (&optional arg)
  "Save an attachment of the report at point, asking which when several.
The target directory prompt proposes the source's first configured
`:repo' (or `gnaw-apply-repo'), and existing files ask before being
overwritten, unless `gnaw-save-no-confirm' says otherwise; a prefix
argument ARG inverts that setting for this call.  Patches go through
`gnaw-save-patches'."
  (interactive "P")
  (let* ((info (cdr (gnaw-list--current)))
         (att (gnaw--choose-attachment info "Save attachment: "))
         (no-confirm (gnaw--save-no-confirm-p arg)))
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
    (when (< (cl-count sid (gnaw-list--display-reports)
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
            ((and sid (>= (cl-count sid (gnaw-list--display-reports)
                                    :key (lambda (p) (gnaw--series-id (cdr p)))
                                    :test #'equal)
                          2))
             (gnaw-list-patch-series-fold))
            ((plist-get info :related) (gnaw-list-related-narrow))
            (t (user-error
                "This report has no related reports or no series to unfold"))))))

(defun gnaw-list--goto (pred)
  "Move point to the first row whose (MID . INFO) pair satisfies PRED.
Return non-nil if found, else return nil with point stepped back
onto the last row, so a failed search never strands point past it."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((p (tabulated-list-get-id)))
        (if (and p (funcall pred p))
            (setq found t)
          (forward-line 1))))
    (when (and (not found) (not (bobp)))
      (forward-line -1))
    found))

(defun gnaw-list--goto-mid (mid)
  "Move point to the row whose report id is MID.
Return non-nil if found, else return nil with point on the last row."
  (gnaw-list--goto (lambda (p) (equal (car p) mid))))

(defun gnaw-list--goto-mid-or-line (mid line)
  "Move point to MID's row, or to LINE when MID has no row anymore.
The converse preference of `gnaw-list--restore-point': after a view
change, following the report matters more than the screen position."
  (unless (and mid (gnaw-list--goto-mid mid))
    (gnaw-list--goto-line line)))

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

(defun gnaw-list--forward-row (col)
  "Move to the next row and to column COL, staying put on the last row.
Mark commands move down so a run of reports can be marked in a row."
  (forward-line 1)
  (unless (tabulated-list-get-id) (forward-line -1))
  (move-to-column col))

(defun gnaw-list--refresh-keeping-point ()
  "Re-render the list, keeping the cursor in place.
When the refresh kept the row count, a report whose line changed
was re-sorted away: stay at the same line rather than chase it (see
`gnaw-list--restore-point').  When rows appeared or vanished, every
line below them shifted: follow the report instead.  Point keeps
its column either way."
  (let ((mid (car (tabulated-list-get-id)))
        (line (line-number-at-pos))
        (col (current-column))
        (rows (count-lines (point-min) (point-max))))
    (gnaw-list-refresh)
    (if (= rows (count-lines (point-min) (point-max)))
        (gnaw-list--restore-point mid line)
      (gnaw-list--goto-mid-or-line mid line))
    (move-to-column col)))

(defun gnaw-list--toggle (action)
  "Toggle local mark ACTION on the report at point, then refresh.
Keep the cursor in place (see `gnaw-list--restore-point')."
  (let ((p (gnaw-list--current)))
    (gnaw-toggle-mark (car p) (cdr p) action)
    (gnaw-list--refresh-keeping-point)))

(defun gnaw-list-toggle-sticky ()
  "Toggle the sticky mark on the report at point, then move down.
Sticky reports are shown in bold and exported to todo.org by the gnaw
CLI.  Moving down keeps point on its column."
  (interactive)
  (let ((col (current-column)))
    (gnaw-list--toggle :sticky)
    (gnaw-list--forward-row col)))

(defun gnaw-list-toggle-dismiss ()
  "Toggle the dismiss mark (hide) on the report at point, immediately.
Turn off `gnaw-list-follow-mode' first.  Then move to the following
report, so a run of reports can be dismissed without chasing point;
point keeps its column.  See `gnaw-list-flag-dismiss' for deferred
dismissal."
  (interactive)
  (gnaw-list--follow-off)
  (let* ((p (gnaw-list--current))
         (line (line-number-at-pos))
         (col (current-column))
         (next (save-excursion
                 (forward-line 1)
                 (car (tabulated-list-get-id))))
         (on (gnaw-toggle-mark (car p) (cdr p) :dismiss)))
    (gnaw-list-refresh)
    ;; Land on the report that followed; without one (last row), stay in
    ;; place like the other mark commands.
    (unless (and next (gnaw-list--goto-mid next))
      (gnaw-list--restore-point (car p) line))
    (move-to-column col)
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
       (gnaw-mark-prefix (gethash mid (gnaw--state-table))))
     t)))

(defun gnaw-list-flag-dismiss ()
  "Flag the report at point for dismissal (or unflag it), then move down.
Flagged reports show D in the Mark column and are dismissed all at
once by \\<gnaw-list-mode-map>\\[gnaw-list-execute-flags].  Flags live
in memory only; nothing is written until then.  Moving down keeps
point on its column."
  (interactive)
  (let ((mid (car (gnaw-list--current)))
        (col (current-column)))
    (setq-local gnaw-list--flagged
                (if (member mid gnaw-list--flagged)
                    (delete mid gnaw-list--flagged)
                  (cons mid gnaw-list--flagged)))
    (gnaw-list--set-mark-cell mid)
    (gnaw-list--update-mode-line)
    (gnaw-list--forward-row col)))

(defun gnaw-list-execute-flags ()
  "Dismiss the reports flagged by \\<gnaw-list-mode-map>\\[gnaw-list-flag-dismiss].
Turn off `gnaw-list-follow-mode' first; write state.edn only once."
  (interactive)
  (unless gnaw-list--flagged
    (user-error "No report flagged for dismissal"))
  (gnaw-list--follow-off)
  (let ((state (gnaw--read-state-for-update))
        (count 0)
        befores)
    (dolist (fmid gnaw-list--flagged)
      (let ((pair (assoc fmid gnaw-list--reports)))
        (when (and pair (not (gnaw-action-on-p state fmid :dismiss)))
          (push (cons fmid (cdr (assoc fmid state))) befores)
          (setq state (gnaw--apply-transition state :dismiss fmid (cdr pair)))
          (setq count (1+ count)))))
    (when befores
      (gnaw--undo-push (format "dismissal of %d report(s)" count) befores))
    (gnaw-write-state state)
    (setq-local gnaw-list--flagged nil)
    (gnaw-list--refresh-keeping-point)
    (message "gnaw: dismissed %d report(s) (type _ to include dismissed reports to the view, C-/ to undo)"
             count)))

(defun gnaw-list-remove-marks ()
  "Remove the mark or dismissal flag from the report at point, then move down.
Moving down keeps point on its column."
  (interactive)
  (let ((p (gnaw-list--current))
        (col (current-column)))
    (cond
     ((member (car p) gnaw-list--flagged)
      (setq-local gnaw-list--flagged (delete (car p) gnaw-list--flagged))
      (gnaw-list--set-mark-cell (car p))
      (gnaw-list--update-mode-line)
      (gnaw-list--forward-row col))
     ((gnaw-remove-marks (car p))
      (gnaw-list--refresh-keeping-point)
      (gnaw-list--forward-row col))
     (t (message "gnaw: no mark on this report")))))

(defun gnaw-list-undo ()
  "Undo the last mark change of this session, then refresh the list.
See `gnaw-undo' for what is restored."
  (interactive)
  (gnaw-undo)
  (gnaw-list--refresh-keeping-point))

(defun gnaw-list-toggle-dismissed ()
  "Toggle whether dismissed reports are shown."
  (interactive)
  (setq-local gnaw-list--related-mids nil) ; the toggle leaves the related view
  (setq-local gnaw-list--show-dismissed (not gnaw-list--show-dismissed))
  (gnaw-list-refresh)
  (message "gnaw: dismissed reports %s"
           (if gnaw-list--show-dismissed "shown" "hidden")))

(defun gnaw-list-sort (&optional n)
  "Sort the report list by the column at point.
With a numeric prefix argument N, sort by the Nth column instead.
Like `tabulated-list-sort', but keep the cursor on its report and
in its column across the re-ordering, and refuse with a clearer
message when point sits in the margin, outside any column."
  (interactive "P")
  (unless (or n (get-text-property (point) 'tabulated-list-column-name))
    (user-error "Point is not in a sortable column"))
  (let ((mid (car (tabulated-list-get-id)))
        (line (line-number-at-pos))
        (col (current-column)))
    (tabulated-list-sort n)
    (gnaw-list--goto-mid-or-line mid line)
    (move-to-column col)))

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
     (gnaw-list-copy-archive-url "copy the archived web page URL")
     (gnaw-list-store-link "store an Org link to the report (C-c C-l inserts it)")
     (gnaw-list-follow-mode "toggle follow mode (auto-show the mail at point)"))
    ("Marks\n—————"
     (gnaw-list-toggle-sticky "toggle the sticky mark (bold, exported to todo.org)")
     (gnaw-list-flag-dismiss "flag for dismissal (D in the Mark column)")
     (gnaw-list-execute-flags "dismiss the flagged reports")
     (gnaw-list-toggle-dismiss "dismiss immediately (toggle)")
     (gnaw-list-remove-marks "remove the mark or flag at point")
     (gnaw-list-undo "undo the last mark change (timestamps restored)")
     (gnaw-list-toggle-dismissed "show / hide dismissed reports"))
    ("Filter and sort\n———————————————"
     (gnaw-list-filter "filter with key:value tokens (C-u: clear the filter)")
     (gnaw-list-filter-transient "filter by one field (menu)")
     (gnaw-select-preset-filter "apply a preset filter")
     (gnaw-list-limit-type "limit to a report type")
     (gnaw-list-filter-topic "filter by a topic, with completion")
     (gnaw-list-filter-acked "only the acked reports (toggle)")
     (gnaw-list-filter-owned "only the owned reports (toggle)")
     (gnaw-list-limit-closed "only the closed reports (canceled, resolved...) (toggle)")
     (gnaw-list-limit-awaiting "only the reports awaiting a reply (toggle)")
     (gnaw-list-limit-related "only the reports with related reports (toggle)")
     (gnaw-list-limit-attachments "only the reports with attachments (toggle)")
     (gnaw-list-filter-cell "toggle a filter on the cell at point (source, author, type, votes, flags, att, msgs, date, subject)")
     "C-u on the keys above (also in the = menu) adds the condition"
     "to the active filter (AND).  On the fixed filter keys -- a, o,"
     "c, ., ~, + and the = menu flags -- C-u C-u excludes the matching"
     "reports instead (-owned:*...) and C-u C-u C-u adds the exclusion"
     "to the active filter (AND)"
     (gnaw-list-sort "sort by the column at point")
     (gnaw-sort "sort by a criterion"))
    ("Patches and attachments\n———————————————————————"
     (gnaw-list-tab "fold / unfold the series, or narrow to related reports")
     (gnaw-list-quit "leave the related view, clear the filter, or quit the window")
     (gnaw-list-attachment-view "view an attachment (patches in diff-mode)")
     (gnaw-list-attachment-save "save an attachment (proposes the configured repo)")
     (gnaw-list-patch-apply "apply the patches with git apply (C-u: git am)")
     (gnaw-list-attachments "menu acting on patches and attachments")
     "git am asks for a branch to create and its base commit; C-u on"
     "the menu's m key inverts gnaw-am-create-worktree (apply in a"
     "new worktree, leaving the repo's checkout untouched)")
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

(defun gnaw--config-add-source (config urls name repo &optional letter)
  "Return CONFIG alist with a source (URLS, NAME, REPO, LETTER) added.
URLS is a list of reports.json URLs; REPO a git directory or a list
of them (a single one is stored as a plain string); LETTER
identifies the source in the browser's S column, and defaults to
the letter of the entry being replaced, so a letterless update
keeps it.  An existing source sharing any URL or the NAME is
replaced; a LETTER already identifying a kept entry signals a
`user-error'."
  (let* ((repo (if (and (consp repo) (null (cdr repo))) (car repo) repo))
         (sources (alist-get :sources config))
         (others (cl-remove-if (lambda (s) (gnaw--source-match-p s urls name))
                               sources))
         (letter (or letter
                     (alist-get :letter
                                (seq-find (lambda (s)
                                            (gnaw--source-match-p s urls name))
                                          sources))))
         (src (append (list (cons :urls urls))
                      (and name (list (cons :name name)))
                      (and letter (list (cons :letter letter)))
                      (and repo (list (cons :repo repo))))))
    (when (and letter (member (downcase letter)
                              (gnaw--taken-source-letters others)))
      (user-error "gnaw: letter %s already identifies another source" letter))
    (gnaw--alist-put config :sources (append others (list src)))))

(defun gnaw--write-config (config)
  "Write CONFIG alist to config.edn as UTF-8."
  (let ((file (expand-file-name "config.edn" gnaw-config-dir)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert (gnaw--edn-write config) "\n")))
    (setq gnaw--config-cache nil)))

(declare-function completing-read-multiple "crm")

(defun gnaw--read-repos (repos)
  "Read git repo directories for a source, extending the kept REPOS.
With no REPOS, first ask whether to link one at all.  Return the
list, the save-target default first."
  (when (or repos (y-or-n-p "Link a local git repo (for patches)? "))
    (unless repos
      (setq repos (list (expand-file-name (read-directory-name "Git repo: ")))))
    (while (y-or-n-p (format "Linked: %s.  Link another git repo? "
                             (mapconcat #'abbreviate-file-name repos ", ")))
      (setq repos (append repos
                          (list (expand-file-name
                                 (read-directory-name "Git repo: ")))))))
  (delete-dups repos))

;;;###autoload
(defun gnaw-add-source (urls &optional name repo letter)
  "Add a source (URLS, NAME, REPO, LETTER) to config.edn.
URLS is a list of reports.json URLs; REPO a git directory or a list
of them; LETTER identifies the source in the browser's S column,
shown when several sources are configured.  Interactively, give
a base, meta.json or reports.json URL, then pick the report files
listed in meta.json, the source NAME, its LETTER \(suggesting the
first free one of NAME) and its local git repo(s) for patches; an
updated source hands down its repos and offers to link more."
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
          (urls (mapcar (lambda (f) (concat dir f)) chosen))
          (cfgs (alist-get :sources (gnaw--read-config-raw)))
          (old (seq-find (lambda (s) (gnaw--source-match-p s urls name)) cfgs))
          (letter
           ;; Letters taken by the sources this add does not replace;
           ;; a replaced entry hands down its letter as the default.
           (let ((taken (gnaw--taken-source-letters
                         (cl-remove-if (lambda (s)
                                         (gnaw--source-match-p s urls name))
                                       cfgs))))
             (gnaw--read-source-letter (or name (car urls)) taken
                                       (alist-get :letter old))))
          ;; A replaced entry hands down its repos; more can be linked.
          (repo (gnaw--read-repos (gnaw--entry-repos old))))
     (list urls name repo letter)))
  (let ((urls (if (listp urls) urls (list urls)))
        (name (and name (not (string-empty-p (string-trim name))) name)))
    (unless urls (user-error "No report file selected"))
    (gnaw--write-config
     (gnaw--config-add-source (gnaw--read-config-raw) urls name repo letter))
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
  (let* ((method (gnaw--read-open-message-method
                  (gnaw--configured-value gnaw-open-message-method source)))
         (group (when (eq method 'gnus)
                  (gnaw--read-source-gnus-group source))))
    (gnaw-configure-email-client source method group)))

(defun gnaw--configure-one-source ()
  "Add one source, then configure how its messages are opened."
  (let* ((urls (call-interactively #'gnaw-add-source))
         (source (or (gnaw--source-name-for-urls urls) t)))
    (gnaw--configure-email-client-for-source source)
    urls))

(defun gnaw--source-summary (entry)
  "Return a description of config.edn source ENTRY, one item per line."
  (let* ((name (alist-get :name entry))
         (urls (append (alist-get :urls entry) nil))
         (letter (alist-get :letter entry))
         (repos (gnaw--entry-repos entry))
         (method (and name (cdr (assoc name gnaw-open-message-method))))
         (group (and name (gnaw--configured-value gnaw-gnus-group name))))
    (concat
     (format "  %s%s\n" (if letter (format "[%s] " letter) "")
             (or name "(unnamed source)"))
     (mapconcat (lambda (u) (format "    %s\n" u)) urls "")
     (and repos
          (format "    repo: %s\n"
                  (mapconcat #'abbreviate-file-name repos ", ")))
     (and method
          (format "    messages open with: %s\n"
                  (gnaw--method-string method group))))))

(defun gnaw--config-summary ()
  "Return a description of the configured sources and open methods."
  (let ((sources (plist-get (gnaw-load-config) :source-configs))
        (method (or (alist-get t gnaw-open-message-method) 'auto))
        (group (alist-get t gnaw-gnus-group)))
    (concat
     (format "Current configuration (%s)\n\n"
             (abbreviate-file-name
              (expand-file-name "config.edn" gnaw-config-dir)))
     (if sources
         (mapconcat #'gnaw--source-summary sources "\n")
       "No source configured.\n")
     (format "\nBy default, messages open with: %s\n"
             (gnaw--method-string method group)))))

(defun gnaw--show-config-summary ()
  "Display the current configuration in a window; return its buffer.
`gnaw-configure' keeps the window around so each prompt is answered
with the existing configuration in sight."
  (let ((buf (get-buffer-create "*gnaw configuration*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (gnaw--config-summary))
        (goto-char (point-min))))
    (display-buffer buf)
    buf))

;;;###autoload
(defun gnaw-configure ()
  "Run interactive gnaw setup.
Configure a report source, its local patch repository and the mail
client used to open report messages; with existing sources, optionally
only the mail client.  The existing configuration stays displayed in
a window while the prompts run, refreshed after each change."
  (interactive)
  (let ((added-source nil)
        (summary nil))
    (unwind-protect
        (progn
          (if (gnaw-sources)
              (progn
                (setq summary (gnaw--show-config-summary))
                (if (y-or-n-p "Add or update Gnaw sources? ")
                    (progn
                      (gnaw--configure-one-source)
                      (setq added-source t))
                  (call-interactively #'gnaw-configure-email-client)))
            (gnaw--configure-one-source)
            (setq added-source t))
          (when added-source
            (setq summary (gnaw--show-config-summary))
            (while (y-or-n-p "Add another source? ")
              (gnaw--configure-one-source)
              (setq summary (gnaw--show-config-summary)))))
      (when (buffer-live-p summary)
        (if-let* ((win (get-buffer-window summary t)))
            (quit-window t win)
          (kill-buffer summary))))))

(defcustom gnaw-inhibit-startup-tip nil
  "When non-nil, `gnaw' does not display its startup tip.
The tip presents a randomly chosen report list command."
  :type 'boolean
  :group 'gnaw)

(defun gnaw--startup-tip ()
  "Return a startup message presenting a random report list command.
The command is drawn from `gnaw-list--help-sections', skipping the
Help section, whose \"h\" key the message always mentions."
  (let* ((cmds (cl-loop for (title . entries) in gnaw-list--help-sections
                        unless (string-prefix-p "Help" title)
                        append (cl-remove-if-not #'consp entries)))
         (cmd (nth (random (length cmds)) cmds))
         (key (where-is-internal (car cmd) gnaw-list-mode-map t)))
    (format "gnaw some bone! Tip: \"%s\" — %s (\"h\" for the full help)"
            (if key (key-description key) (format "M-x %s" (car cmd)))
            (cadr cmd))))

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
    (unless gnaw-inhibit-startup-tip
      (message "%s" (gnaw--startup-tip)))))

(provide 'gnaw)
;;; gnaw.el ends here
