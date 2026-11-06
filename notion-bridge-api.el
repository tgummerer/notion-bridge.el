;;; notion-bridge-api.el --- Notion API HTTP layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas Gummerer

;; Author: Thomas Gummerer
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; HTTP client layer for the Notion API.
;;
;; Every request goes through `notion-bridge-api--curl'.  The token is
;; handed to curl in a private config file and request bodies on stdin,
;; so neither appears on a command line.
;;
;; Writes are refused unless they happen inside
;; `notion-bridge-api-with-write-scope', and then only against the IDs
;; that scope allows: the open page and the blocks fetched from it.
;; Pages are never deleted or archived, and a block that carries a page
;; (a subpage, an inline database) or shared content (a synced block)
;; can never be deleted.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'auth-source)

(defgroup notion-bridge nil
  "Bidirectional Notion integration for Emacs."
  :group 'tools
  :prefix "notion-bridge-")

(defcustom notion-bridge-api-token nil
  "Notion API integration token.
Can be a string token, a function that returns a token, or nil.
When nil, the token is looked up via auth-source for host
\"api.notion.com\"."
  :type '(choice (string :tag "Token string")
                 (function :tag "Function returning token")
                 (const :tag "Use auth-source" nil))
  :group 'notion-bridge)

(defcustom notion-bridge-api-timeout 60
  "Seconds before a single Notion request is abandoned."
  :type 'natnum
  :group 'notion-bridge)

(defconst notion-bridge-api--base-url "https://api.notion.com/v1"
  "Base URL for the Notion API.")

(defconst notion-bridge-api--version "2022-06-28"
  "Notion API version.")

(defconst notion-bridge-api--max-retries 4
  "Retries for rate-limited, unavailable, or unreachable requests.")

(defconst notion-bridge-api--protected-types
  '("child_page" "child_database" "synced_block")
  "Block types that carry a page or shared content.
Deleting one of these deletes more than this page's content, so they
are never deleted, edited, or descended into for writes.")

;;; Token Resolution

(defun notion-bridge-api--get-token ()
  "Resolve the Notion API token.
Check `notion-bridge-api-token' first (string or function), then
fall back to auth-source lookup for host \"api.notion.com\"."
  (cond
   ((stringp notion-bridge-api-token)
    notion-bridge-api-token)
   ((functionp notion-bridge-api-token)
    (funcall notion-bridge-api-token))
   (t
    (let ((found (car (auth-source-search :host "api.notion.com"
                                          :max 1))))
      (when found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret)
              (funcall secret)
            secret)))))))

;;; Write scope

(defvar notion-bridge-api--write-scope nil
  "Plist describing what writes are allowed right now, or nil for none.
:page is the open page's ID; :edit, :append, :delete and
:discussions are hash tables of normalized IDs.")

(defun notion-bridge-api--norm (id)
  "Normalize a Notion ID: lowercase, without hyphens."
  (and id (downcase (replace-regexp-in-string "-" "" id))))

(defun notion-bridge-api--id-set (ids)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (id ids) (when id (puthash (notion-bridge-api--norm id) t h)))
    h))

(defun notion-bridge-api-make-scope (page-id blocks &optional discussions)
  "Build a write scope for PAGE-ID from its fetched BLOCKS tree.
Blocks inside protected blocks are excluded, protected blocks may not
be edited or deleted, and a block whose subtree contains a protected
block may not be deleted.  DISCUSSIONS are comment threads on the page."
  (let ((edit nil) (append (list page-id)) (delete nil))
    (cl-labels
        ((walk (bs)
           (let ((any-protected nil))
             (dolist (b bs)
               (let* ((type (plist-get b :type))
                      (id (plist-get b :id))
                      (protected (member type notion-bridge-api--protected-types))
                      (sub-protected (and (not protected)
                                          (walk (plist-get b :children)))))
                 (if protected
                     (setq any-protected t)
                   (push id edit)
                   (push id append)
                   (if (or sub-protected (notion-bridge-api--unfetched-p b))
                       (setq any-protected t)
                     (push id delete))
                   (when sub-protected (setq any-protected t)))))
             any-protected)))
      (walk blocks))
    (list :page (notion-bridge-api--norm page-id)
          :edit (notion-bridge-api--id-set edit)
          :append (notion-bridge-api--id-set append)
          :delete (notion-bridge-api--id-set delete)
          :discussions (notion-bridge-api--id-set discussions))))

(defun notion-bridge-api--unfetched-p (block)
  "Non-nil if BLOCK has children that were not fetched."
  (and (eq (plist-get block :has_children) t)
       (not (plist-get block :children))))

(defmacro notion-bridge-api-with-write-scope (scope &rest body)
  "Run BODY with writes allowed only as described by SCOPE."
  (declare (indent 1))
  `(let ((notion-bridge-api--write-scope ,scope))
     ,@body))

(defun notion-bridge-api--scope-add (ids)
  "Allow edits, appends and deletes on freshly created IDS."
  (when notion-bridge-api--write-scope
    (dolist (id ids)
      (dolist (k '(:edit :append :delete))
        (puthash (notion-bridge-api--norm id) t
                 (plist-get notion-bridge-api--write-scope k))))))

(defun notion-bridge-api--refuse (fmt &rest args)
  (signal 'error (list (concat "notion-bridge refused a write: "
                               (apply #'format fmt args)))))

(defun notion-bridge-api--allowed-p (key id)
  (gethash (notion-bridge-api--norm id)
           (plist-get notion-bridge-api--write-scope key)))

(defun notion-bridge-api--check-write (method endpoint body)
  "Signal an error unless METHOD ENDPOINT with BODY is within scope."
  (let ((scope notion-bridge-api--write-scope)
        (path (car (split-string endpoint "?"))))
    (cond
     ((and (equal method "POST") (equal path "/search")) t)
     ((null scope)
      (notion-bridge-api--refuse "%s %s outside of a push to the open page"
                                 method path))
     ((and (equal method "DELETE")
           (string-match "\\`/blocks/\\([^/]+\\)\\'" path))
      (unless (notion-bridge-api--allowed-p :delete (match-string 1 path))
        (notion-bridge-api--refuse
         "deleting %s, which is not deletable content of the open page"
         (match-string 1 path))))
     ((equal method "DELETE")
      (notion-bridge-api--refuse "DELETE %s (pages are never deleted)" path))
     ((and (equal method "PATCH")
           (string-match "\\`/blocks/\\([^/]+\\)/children\\'" path))
      (unless (notion-bridge-api--allowed-p :append (match-string 1 path))
        (notion-bridge-api--refuse "appending to %s outside the open page"
                                   (match-string 1 path))))
     ((and (equal method "PATCH")
           (string-match "\\`/blocks/\\([^/]+\\)\\'" path))
      (when (or (plist-member body :archived) (plist-member body :in_trash))
        (notion-bridge-api--refuse "archiving a block via PATCH"))
      (unless (notion-bridge-api--allowed-p :edit (match-string 1 path))
        (notion-bridge-api--refuse "editing %s outside the open page"
                                   (match-string 1 path))))
     ((and (equal method "PATCH")
           (string-match "\\`/pages/\\([^/]+\\)\\'" path))
      (unless (equal (notion-bridge-api--norm (match-string 1 path))
                     (plist-get scope :page))
        (notion-bridge-api--refuse "editing page %s, which is not the open page"
                                   (match-string 1 path)))
      (unless (equal (cl-loop for (k _) on body by #'cddr collect k)
                     '(:properties))
        (notion-bridge-api--refuse "changing anything but page properties")))
     ((and (equal method "POST") (equal path "/comments"))
      (let ((parent-page (plist-get (plist-get body :parent) :page_id))
            (discussion (plist-get body :discussion_id)))
        (unless (or (and parent-page
                         (equal (notion-bridge-api--norm parent-page)
                                (plist-get scope :page)))
                    (and discussion
                         (notion-bridge-api--allowed-p :discussions discussion)))
          (notion-bridge-api--refuse
           "commenting outside the open page's discussions"))))
     (t
      (notion-bridge-api--refuse "%s %s is not a write notion-bridge makes"
                                 method path)))))

;;; Transport

(defun notion-bridge-api--config-file (token)
  "Write a private curl config with TOKEN's headers; return its name."
  (let ((file (make-temp-file "notion-bridge-curl")))
    (set-file-modes file #o600)
    (with-temp-file file
      (insert (format "header = \"Authorization: Bearer %s\"\n" token)
              (format "header = \"Notion-Version: %s\"\n"
                      notion-bridge-api--version)
              "header = \"Content-Type: application/json\"\n"))
    file))

(defun notion-bridge-api--curl-args (config method url has-body)
  (append (list "-s" "-K" config "-X" method
                "--max-time" (number-to-string notion-bridge-api-timeout)
                "--connect-timeout" "15"
                "-w" "\n%header{retry-after}\n%{http_code}")
          (when has-body (list "--data-binary" "@-"))
          (list url)))

(defun notion-bridge-api--split-output (output)
  "Split curl OUTPUT into (STATUS RETRY-AFTER BODY)."
  (let* ((lines (split-string output "\n"))
         (n (length lines)))
    (if (< n 3)
        (list 0 nil "")
      (list (string-to-number (nth (- n 1) lines))
            (let ((ra (nth (- n 2) lines)))
              (and (string-match-p "\\`[0-9]+\\'" ra) (string-to-number ra)))
            (string-join (butlast lines 2) "\n")))))

(defun notion-bridge-api--curl (method url &optional body-string)
  "Run one curl request; return (STATUS RETRY-AFTER BODY-STRING).
STATUS is 0 when the request could not complete."
  (let* ((token (or (notion-bridge-api--get-token)
                    (error "Notion API token not configured.  Set `notion-bridge-api-token' or configure auth-source for host \"api.notion.com\"")))
         (config (notion-bridge-api--config-file token)))
    (unwind-protect
        (with-temp-buffer
          (let ((args (notion-bridge-api--curl-args config method url body-string)))
            (if body-string
                (apply #'call-process-region
                       (encode-coding-string body-string 'utf-8) nil
                       "curl" nil t nil args)
              (apply #'call-process "curl" nil t nil args)))
          (notion-bridge-api--split-output
           (decode-coding-string (buffer-string) 'utf-8)))
      (delete-file config))))

(defun notion-bridge-api--parse (body-str)
  (condition-case nil
      (json-parse-string body-str
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false)
    (error nil)))

(defun notion-bridge-api--retryable-p (status)
  (memq status '(0 409 429 500 502 503 504)))

(defun notion-bridge-api--request-sync (method endpoint &optional body)
  "Make a synchronous request to the Notion API.
METHOD is the HTTP method, ENDPOINT the API path, BODY an optional
plist sent as JSON.  Writes are checked against the write scope.
Returns the parsed JSON response; signals an error on failure."
  (unless (equal method "GET")
    (notion-bridge-api--check-write method endpoint body))
  (let ((url (concat notion-bridge-api--base-url endpoint))
        (body-str (and body (json-serialize body :false-object :json-false)))
        (attempt 0)
        result)
    (while (not result)
      (pcase-let ((`(,status ,retry-after ,resp) (notion-bridge-api--curl
                                                  method url body-str)))
        (let ((json (notion-bridge-api--parse resp)))
          (cond
           ((and (>= status 200) (< status 300))
            (setq result (or json (list :ok t))))
           ((and (notion-bridge-api--retryable-p status)
                 (< attempt notion-bridge-api--max-retries))
            (cl-incf attempt)
            (sleep-for (or retry-after (expt 2 (1- attempt)))))
           ((eql status 0)
            (error "Notion API unreachable after %d attempts: %s %s"
                   (1+ attempt) method endpoint))
           ((eql status 401)
            (error "Notion API authentication failed (401).  Check your API token"))
           ((eql status 404)
            (error "Notion API resource not found (404): %s" endpoint))
           (t
            (error "Notion API error (%s): %s [%s %s]"
                   status (or (plist-get json :message) "Unknown error")
                   method endpoint))))))
    result))

;;; Parallel reads

(defun notion-bridge-api--request-parallel (requests)
  "Run read-only REQUESTS concurrently, a few at a time.
REQUESTS is a list of (\"GET\" ENDPOINT).  Returns results in order.
Requests that fail concurrently are retried one by one, so every
element is a parsed response; a request that still fails signals."
  (dolist (r requests)
    (unless (equal (car r) "GET")
      (notion-bridge-api--refuse "parallel %s requests" (car r))))
  (let* ((token (or (notion-bridge-api--get-token)
                    (error "Notion API token not configured")))
         (results (make-vector (length requests) nil))
         (config (notion-bridge-api--config-file token)))
    (unwind-protect
        (cl-loop
         for start from 0 below (length requests) by 3
         do (let ((procs nil))
              (cl-loop
               for req in (cl-subseq requests start
                                     (min (+ start 3) (length requests)))
               for idx from start
               do (let* ((buf (generate-new-buffer " *notion-async*"))
                         (proc (apply #'start-process
                                      (format "notion-%d" idx) buf "curl"
                                      (notion-bridge-api--curl-args
                                       config "GET"
                                       (concat notion-bridge-api--base-url
                                               (nth 1 req))
                                       nil))))
                    (set-process-query-on-exit-flag proc nil)
                    (push (list idx proc buf) procs)))
              (dolist (p procs)
                (while (process-live-p (nth 1 p))
                  (accept-process-output (nth 1 p) 0.1)))
              (dolist (p procs)
                (pcase-let ((`(,status ,_ ,body)
                             (with-current-buffer (nth 2 p)
                               (notion-bridge-api--split-output
                                (decode-coding-string (buffer-string) 'utf-8)))))
                  (when (and (>= status 200) (< status 300))
                    (aset results (nth 0 p) (notion-bridge-api--parse body))))
                (kill-buffer (nth 2 p)))
              (sleep-for 1.0)))
      (delete-file config))
    (cl-loop for r in requests
             for i from 0
             unless (aref results i)
             do (aset results i (notion-bridge-api--request-sync
                                 "GET" (nth 1 r))))
    (append results nil)))

;;; Public API Functions

(defun notion-bridge-api-get-page (page-id)
  "Retrieve a Notion page by PAGE-ID."
  (notion-bridge-api--request-sync "GET" (format "/pages/%s" page-id)))

(defun notion-bridge-api-get-blocks (block-id &optional start-cursor)
  "Retrieve one page of child blocks of BLOCK-ID."
  (let ((endpoint (format "/blocks/%s/children?page_size=100" block-id)))
    (when start-cursor
      (setq endpoint (concat endpoint "&start_cursor="
                             (url-hexify-string start-cursor))))
    (notion-bridge-api--request-sync "GET" endpoint)))

(defun notion-bridge-api-get-blocks-all (block-id)
  "Retrieve all child blocks of BLOCK-ID, handling pagination."
  (let ((all-blocks nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let ((resp (notion-bridge-api-get-blocks block-id cursor)))
        (setq all-blocks (nconc all-blocks (plist-get resp :results)))
        (setq has-more (eq (plist-get resp :has_more) t))
        (setq cursor (plist-get resp :next_cursor))))
    all-blocks))

(defun notion-bridge-api-get-blocks-recursive (block-id)
  "Retrieve the content tree under BLOCK-ID.
Blocks with children get a :children key.  Subpages, databases and
synced blocks are not descended into: their content is not this page's."
  (mapcar
   (lambda (block)
     (when (and (eq (plist-get block :has_children) t)
                (not (member (plist-get block :type)
                             notion-bridge-api--protected-types)))
       (setq block (plist-put block :children
                              (notion-bridge-api-get-blocks-recursive
                               (plist-get block :id)))))
     block)
   (notion-bridge-api-get-blocks-all block-id)))

(defun notion-bridge-api-update-block (block-id properties)
  "Update the block BLOCK-ID with PROPERTIES."
  (notion-bridge-api--request-sync "PATCH" (format "/blocks/%s" block-id)
                                   properties))

(defun notion-bridge-api-append-blocks (block-id children &optional after)
  "Append CHILDREN under BLOCK-ID, after the sibling AFTER if given.
Returns the created blocks, in order, and adds them to the write scope."
  (let* ((body (list :children (vconcat children)))
         (_ (when after (setq body (plist-put body :after after))))
         (resp (notion-bridge-api--request-sync
                "PATCH" (format "/blocks/%s/children" block-id) body))
         (created (plist-get resp :results)))
    (when after
      (let ((pos (cl-position (notion-bridge-api--norm after) created
                              :key (lambda (b) (notion-bridge-api--norm
                                                (plist-get b :id)))
                              :test #'equal)))
        (when pos (setq created (nthcdr (1+ pos) created)))
        (setq created (cl-subseq created 0 (min (length children)
                                                (length created))))))
    (unless after
      (setq created (last created (length children))))
    (notion-bridge-api--scope-add (mapcar (lambda (b) (plist-get b :id)) created))
    created))

(defun notion-bridge-api-delete-block (block-id)
  "Delete the block BLOCK-ID, if the write scope allows it."
  (notion-bridge-api--request-sync "DELETE" (format "/blocks/%s" block-id)))

(defun notion-bridge-api-update-page-title (page-id property title)
  "Set PAGE-ID's title PROPERTY to TITLE."
  (notion-bridge-api--request-sync
   "PATCH" (format "/pages/%s" page-id)
   (list :properties
         (list (intern (concat ":" property))
               (list :title (vector (list :type "text"
                                          :text (list :content title))))))))

(defun notion-bridge-api-search (query &optional filter start-cursor)
  "Search Notion with QUERY string.
Optional FILTER is a plist (e.g., \\='(:value \"page\" :property \"object\")).
Optional START-CURSOR for pagination."
  (let ((body (list :query query)))
    (when filter
      (setq body (plist-put body :filter filter)))
    (when start-cursor
      (setq body (plist-put body :start_cursor start-cursor)))
    (notion-bridge-api--request-sync "POST" "/search" body)))

(defun notion-bridge-api-get-comments (block-id)
  "Retrieve all comments for BLOCK-ID (can be a page ID)."
  (let ((all-comments nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let ((resp (notion-bridge-api--request-sync
                   "GET" (concat (format "/comments?block_id=%s" block-id)
                                 (if cursor
                                     (concat "&start_cursor="
                                             (url-hexify-string cursor))
                                   "")))))
        (setq all-comments (nconc all-comments (plist-get resp :results)))
        (setq has-more (eq (plist-get resp :has_more) t))
        (setq cursor (plist-get resp :next_cursor))))
    all-comments))

(defun notion-bridge-api-post-comment (page-id text &optional discussion-id)
  "Post a comment on PAGE-ID with TEXT.
If DISCUSSION-ID is provided, reply to that discussion thread."
  (let ((body (list :rich_text (vector (list :type "text"
                                             :text (list :content text))))))
    (if discussion-id
        (setq body (plist-put body :discussion_id discussion-id))
      (setq body (plist-put body :parent (list :page_id page-id))))
    (notion-bridge-api--request-sync "POST" "/comments" body)))

(defun notion-bridge-api-get-comments-parallel (block-ids)
  "Fetch comments for all BLOCK-IDS concurrently; return them flattened."
  (let ((results (notion-bridge-api--request-parallel
                  (mapcar (lambda (id)
                            (list "GET" (format "/comments?block_id=%s" id)))
                          block-ids))))
    (apply #'append (mapcar (lambda (r) (plist-get r :results)) results))))

(defun notion-bridge-api-search-all (&optional filter)
  "Retrieve all pages accessible to the integration.
Optional FILTER is a plist (e.g., \\='(:value \"page\" :property \"object\")).
Handles pagination automatically.  Returns a list of all result plists."
  (let ((all-results nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let ((body (list :query "" :page_size 100)))
        (when filter (setq body (plist-put body :filter filter)))
        (when cursor (setq body (plist-put body :start_cursor cursor)))
        (let ((resp (notion-bridge-api--request-sync "POST" "/search" body)))
          (setq all-results (nconc all-results (plist-get resp :results)))
          (setq has-more (eq (plist-get resp :has_more) t))
          (setq cursor (plist-get resp :next_cursor)))))
    all-results))

(provide 'notion-bridge-api)
;;; notion-bridge-api.el ends here
