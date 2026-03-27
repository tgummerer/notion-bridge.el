;;; notion-bridge-api.el --- Notion API HTTP layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas Gummerer

;; Author: Thomas Gummerer
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; HTTP client layer for the Notion API.  Provides synchronous request
;; functions for pages, blocks, and search.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
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

(defconst notion-bridge-api--base-url "https://api.notion.com/v1"
  "Base URL for the Notion API.")

(defconst notion-bridge-api--version "2022-06-28"
  "Notion API version.")

(defconst notion-bridge-api--rate-limit-delay 1.0
  "Seconds to wait before retrying after a 429 rate limit response.")

(defconst notion-bridge-api--max-retries 3
  "Maximum number of retries for rate-limited requests.")

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

;;; HTTP Layer

(defun notion-bridge-api--parse-response ()
  "Parse the HTTP response in the current buffer.
Returns a cons cell (STATUS-CODE . PARSED-JSON)."
  (goto-char (point-min))
  (let ((status-code nil))
    ;; Extract HTTP status code
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (setq status-code (string-to-number (match-string 1))))
    ;; Find the body (after blank line)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (let* ((body (buffer-substring-no-properties (point) (point-max)))
           (parsed (condition-case nil
                       (json-parse-string body :object-type 'plist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object :json-false)
                     (error nil))))
      (cons status-code parsed))))

(defun notion-bridge-api--request-sync (method endpoint &optional body)
  "Make a synchronous HTTP request to the Notion API using curl.
METHOD is the HTTP method (\"GET\", \"POST\", \"PATCH\", \"DELETE\").
ENDPOINT is the API path (e.g., \"/pages/PAGE-ID\").
BODY is an optional plist to send as JSON.

Returns the parsed JSON response as a plist.
Signals an error on HTTP errors."
  (let* ((token (notion-bridge-api--get-token))
         (url (concat notion-bridge-api--base-url endpoint))
         (retries 0)
         result)
    (unless token
      (error "Notion API token not configured.  Set `notion-bridge-api-token' or configure auth-source for host \"api.notion.com\""))
    (cl-block nil
      (while (<= retries notion-bridge-api--max-retries)
        (let* ((json-body-str (when body
                                (json-serialize body :false-object :json-false)))
               (args (append
                      (list "-s" "-X" method)
                      (list "-H" (concat "Authorization: Bearer " token))
                      (list "-H" (concat "Notion-Version: " notion-bridge-api--version))
                      (list "-H" "Content-Type: application/json")
                      (when json-body-str
                        (list "-d" json-body-str))
                      (list "-w" "\n%{http_code}")
                      (list url)))
               (output (with-temp-buffer
                         (apply #'call-process "curl" nil t nil args)
                         (buffer-string)))
               ;; Last line is the HTTP status code
               (lines (split-string output "\n"))
               (status-str (car (last lines)))
               (status (string-to-number status-str))
               (body-str (string-join (butlast lines) "\n"))
               (json-body (condition-case nil
                              (json-parse-string body-str
                                                 :object-type 'plist
                                                 :array-type 'list
                                                 :null-object nil
                                                 :false-object :json-false)
                            (error nil))))
          (cond
           ;; Success
           ((and (> status 0) (>= status 200) (< status 300))
            (setq result json-body)
            (cl-return nil))
           ;; Rate limited
           ((eql status 429)
            (cl-incf retries)
            (when (> retries notion-bridge-api--max-retries)
              (error "Notion API rate limited after %d retries: %s %s"
                     notion-bridge-api--max-retries method endpoint))
            (sleep-for notion-bridge-api--rate-limit-delay))
           ;; Auth error
           ((eql status 401)
            (error "Notion API authentication failed (401).  Check your API token"))
           ;; Not found
           ((eql status 404)
            (error "Notion API resource not found (404): %s" endpoint))
           ;; Other error
           (t
            (let ((msg (or (and json-body (plist-get json-body :message)) "Unknown error")))
              (error "Notion API error (%s): %s [%s %s]"
                     status msg method endpoint)))))))
    result))

;;; Parallel Requests

(defun notion-bridge-api--request-parallel (requests)
  "Execute multiple API REQUESTS in parallel using async curl processes.
REQUESTS is a list of (METHOD ENDPOINT &optional BODY) lists.
Returns a list of results in the same order, each being a plist or nil on error.
Respects Notion's rate limit by batching in groups of 3."
  (let* ((token (notion-bridge-api--get-token))
         (results (make-vector (length requests) nil))
         (batch-size 3))
    (unless token
      (error "Notion API token not configured"))
    (cl-loop for batch-start from 0 below (length requests) by batch-size
             do (let* ((batch-end (min (+ batch-start batch-size) (length requests)))
                       (batch (cl-subseq requests batch-start batch-end))
                       (procs nil)
                       (bufs nil))
                  ;; Launch batch of async curl processes
                  (cl-loop for req in batch
                           for idx from batch-start
                           do (let* ((method (nth 0 req))
                                     (endpoint (nth 1 req))
                                     (body (nth 2 req))
                                     (url (concat notion-bridge-api--base-url endpoint))
                                     (json-body-str (when body
                                                      (json-serialize body :false-object :json-false)))
                                     (args (append
                                            (list "-s" "-X" method)
                                            (list "-H" (concat "Authorization: Bearer " token))
                                            (list "-H" (concat "Notion-Version: " notion-bridge-api--version))
                                            (list "-H" "Content-Type: application/json")
                                            (when json-body-str (list "-d" json-body-str))
                                            (list "-w" "\n%{http_code}")
                                            (list url)))
                                     (buf (generate-new-buffer (format " *notion-async-%d*" idx)))
                                     (proc (apply #'start-process
                                                  (format "notion-%d" idx)
                                                  buf "curl" args)))
                                (push (cons idx (cons proc buf)) procs)
                                (push buf bufs)))
                  ;; Wait for all processes in batch to finish
                  (dolist (entry procs)
                    (let ((proc (cadr entry)))
                      (while (process-live-p proc)
                        (accept-process-output proc 0.1))))
                  ;; Collect results
                  (dolist (entry procs)
                    (let* ((idx (car entry))
                           (buf (cddr entry)))
                      (with-current-buffer buf
                        (let* ((output (buffer-string))
                               (lines (split-string output "\n"))
                               (status-str (car (last lines)))
                               (status (string-to-number status-str))
                               (body-str (string-join (butlast lines) "\n"))
                               (parsed (condition-case nil
                                           (json-parse-string body-str
                                                              :object-type 'plist
                                                              :array-type 'list
                                                              :null-object nil
                                                              :false-object :json-false)
                                         (error nil))))
                          (when (and (>= status 200) (< status 300))
                            (aset results idx parsed))))))
                  ;; Clean up buffers
                  (dolist (buf bufs)
                    (kill-buffer buf))
                  ;; Small delay between batches for rate limiting
                  (when (< batch-end (length requests))
                    (sleep-for 0.3))))
    (append results nil)))

;;; Public API Functions

(defun notion-bridge-api-get-page (page-id)
  "Retrieve a Notion page by PAGE-ID."
  (notion-bridge-api--request-sync "GET" (format "/pages/%s" page-id)))

(defun notion-bridge-api-get-blocks (block-id &optional start-cursor)
  "Retrieve child blocks of BLOCK-ID.
Returns a plist with :results, :has_more, and :next_cursor.
If START-CURSOR is non-nil, fetch the next page of results."
  (let ((endpoint (format "/blocks/%s/children" block-id)))
    (when start-cursor
      (setq endpoint (concat endpoint "?start_cursor=" start-cursor)))
    (notion-bridge-api--request-sync "GET" endpoint)))

(defun notion-bridge-api-get-blocks-all (block-id)
  "Retrieve all child blocks of BLOCK-ID, handling pagination.
Returns a list of all block plists."
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
  "Retrieve all child blocks of BLOCK-ID recursively.
Each block that has children will have a :children key added
containing its nested child blocks."
  (let ((blocks (notion-bridge-api-get-blocks-all block-id)))
    (mapcar
     (lambda (block)
       (when (plist-get block :has_children)
         (let ((children (notion-bridge-api-get-blocks-recursive
                          (plist-get block :id))))
           (setq block (plist-put block :children children))))
       block)
     blocks)))

(defun notion-bridge-api-update-block (block-id properties)
  "Update a block identified by BLOCK-ID.
PROPERTIES is a plist of block properties to update."
  (notion-bridge-api--request-sync "PATCH"
                                   (format "/blocks/%s" block-id)
                                   properties))

(defun notion-bridge-api-append-blocks (block-id children)
  "Append CHILDREN blocks to the block identified by BLOCK-ID.
CHILDREN is a list or vector of block plists conforming to the Notion API."
  (notion-bridge-api--request-sync "PATCH"
                                   (format "/blocks/%s/children" block-id)
                                   (list :children (if (vectorp children)
                                                       children
                                                     (vconcat children)))))

(defun notion-bridge-api-delete-block (block-id)
  "Delete a block identified by BLOCK-ID."
  (notion-bridge-api--request-sync "DELETE"
                                   (format "/blocks/%s" block-id)))

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
  "Retrieve all comments for BLOCK-ID (can be a page ID).
Returns a list of comment plists."
  (let ((all-comments nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let* ((endpoint (format "/comments?block_id=%s" block-id))
             (resp (progn
                     (when cursor
                       (setq endpoint (concat endpoint "&start_cursor=" cursor)))
                     (notion-bridge-api--request-sync "GET" endpoint))))
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
        (plist-put body :discussion_id discussion-id)
      (plist-put body :parent (list :page_id page-id)))
    (notion-bridge-api--request-sync "POST" "/comments" body)))

(defun notion-bridge-api-get-comments-parallel (block-ids)
  "Fetch comments for all BLOCK-IDS in parallel.
Returns a flat list of all comment plists."
  (let* ((requests (mapcar (lambda (id)
                             (list "GET" (format "/comments?block_id=%s" id)))
                           block-ids))
         (results (notion-bridge-api--request-parallel requests))
         (all-comments nil))
    (dolist (resp results)
      (when resp
        (let ((comments (plist-get resp :results)))
          (setq all-comments (append all-comments comments)))))
    all-comments))

(defun notion-bridge-api-delete-blocks-parallel (block-ids)
  "Delete all blocks in BLOCK-IDS in parallel."
  (let ((requests (mapcar (lambda (id)
                            (list "DELETE" (format "/blocks/%s" id)))
                          block-ids)))
    (notion-bridge-api--request-parallel requests)))

(defun notion-bridge-api-search-all (&optional filter)
  "Retrieve all pages accessible to the integration.
Optional FILTER is a plist (e.g., \\='(:value \"page\" :property \"object\")).
Handles pagination automatically.  Returns a list of all result plists."
  (let ((all-results nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let* ((body (list :query ""))
             (resp (progn
                     (when filter
                       (setq body (plist-put body :filter filter)))
                     (when cursor
                       (setq body (plist-put body :start_cursor cursor)))
                     (plist-put body :page_size 100)
                     (notion-bridge-api--request-sync "POST" "/search" body))))
        (setq all-results (nconc all-results (plist-get resp :results)))
        (setq has-more (eq (plist-get resp :has_more) t))
        (setq cursor (plist-get resp :next_cursor))))
    all-results))

(provide 'notion-bridge-api)
;;; notion-bridge-api.el ends here
