;;; notion-bridge-sync.el --- Sync orchestration for notion-bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: notion-bridge
;; Keywords: tools

;;; Commentary:

;; Pull and push orchestration between Notion pages and org-mode buffers.
;; Handles conflict detection, diffing, and incremental updates.

;;; Code:

(require 'cl-lib)
(require 'notion-bridge-api)
(require 'notion-bridge-org)
(require 'notion-bridge-cache)

;;; Buffer-local variables

(defvar-local notion-bridge--page-id nil
  "Notion page ID for the current buffer.")

(defvar-local notion-bridge--last-edited nil
  "Last edited time from Notion for conflict detection.")

;;; ============================================================
;;; Pull: Notion -> Org buffer
;;; ============================================================

(defun notion-bridge-sync--find-buffer-for-page (page-id)
  "Find an existing buffer that is linked to PAGE-ID, or nil."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf
                  (equal notion-bridge--page-id page-id))
           return buf))

(defun notion-bridge-sync-pull (page-id)
  "Pull a Notion page by PAGE-ID and display as an org buffer.
Reuses an existing buffer for this page if one exists."
  (interactive "sNotion page ID: ")
  (let* ((page (notion-bridge-api-get-page page-id))
         (last-edited (plist-get page :last_edited_time))
         (title (notion-bridge-sync--extract-title page))
         (blocks (notion-bridge-api-get-blocks-recursive page-id))
         ;; Load page map if available for inter-page link resolution
         (notion-bridge-org--page-map
          (or notion-bridge-org--page-map
              (and (boundp 'notion-bridge-org-directory)
                   (notion-bridge-sync-load-page-index
                    (symbol-value 'notion-bridge-org-directory)))))
         (org-content (notion-bridge-org--blocks-to-org blocks))
         ;; Reuse existing buffer if one is already open for this page
         (existing-buf (notion-bridge-sync--find-buffer-for-page page-id))
         (buf-name (format "*notion: %s*" (or title page-id)))
         (buf (or existing-buf (get-buffer-create buf-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert file-level keywords
        (insert (format "#+TITLE: %s\n" (or title "")))
        (insert (format "#+NOTION_PAGE_ID: %s\n" page-id))
        (insert (format "#+NOTION_LAST_SYNCED: %s\n"
                        (format-time-string "%FT%T%z")))
        (insert "\n")
        ;; Insert blocks one by one, tracking block-id to line positions
        (let ((block-line-map (make-hash-table :test 'equal))
              (all-comments (notion-bridge-sync--fetch-all-comments page-id blocks)))
          (notion-bridge-sync--insert-blocks-tracked blocks block-line-map)
          (when all-comments
            (insert "\n")
            (notion-bridge-sync--insert-comments all-comments))
          (goto-char (point-min))
          (org-mode)
          (when all-comments
            (notion-bridge-sync--add-comment-overlays
             all-comments blocks block-line-map))
          ;; Set buffer-local vars
          (setq notion-bridge--page-id page-id)
          (setq notion-bridge--last-edited last-edited)
          ;; Build and save cache (including comments)
          (let ((cache (notion-bridge-sync--build-cache-from-blocks
                        page-id last-edited blocks))
                (cache-key (or buffer-file-name (buffer-name))))
            (when all-comments
              (setq cache (notion-bridge-cache-set-comments
                           cache (notion-bridge-sync--serialize-comments all-comments))))
            (notion-bridge-cache-save cache-key cache)))
        ;; Enable notion-bridge-mode if available
        (when (fboundp 'notion-bridge-mode)
          (notion-bridge-mode 1))
        (set-buffer-modified-p nil)
        ;; Save to disk if file-backed, and re-save cache under file path
        (when buffer-file-name
          (save-buffer)
          ;; Re-save cache under the file path key
          (let ((old-cache (notion-bridge-cache-load (buffer-name))))
            (when old-cache
              (notion-bridge-cache-save buffer-file-name old-cache))))))
    (pop-to-buffer-same-window buf)
    (message "Pulled Notion page: %s" (or title page-id))
    buf))

(defun notion-bridge-sync-refresh (page-id)
  "Refresh the current buffer with fresh content from Notion page PAGE-ID.
Unlike `notion-bridge-sync-pull', this updates the current buffer in-place."
  (let* ((page (notion-bridge-api-get-page page-id))
         (last-edited (plist-get page :last_edited_time))
         (title (notion-bridge-sync--extract-title page))
         (blocks (notion-bridge-api-get-blocks-recursive page-id))
         (notion-bridge-org--page-map
          (or notion-bridge-org--page-map
              (and (boundp 'notion-bridge-org-directory)
                   (notion-bridge-sync-load-page-index
                    (symbol-value 'notion-bridge-org-directory)))))
         (org-content (notion-bridge-org--blocks-to-org blocks))
         (pos (point)))
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'notion-bridge-comment t)
      (erase-buffer)
      (insert (format "#+TITLE: %s\n" (or title "")))
      (insert (format "#+NOTION_PAGE_ID: %s\n" page-id))
      (insert (format "#+NOTION_LAST_SYNCED: %s\n"
                      (format-time-string "%FT%T%z")))
      (insert "\n")
      (let ((block-line-map (make-hash-table :test 'equal))
            (all-comments (notion-bridge-sync--fetch-all-comments page-id blocks)))
        (notion-bridge-sync--insert-blocks-tracked blocks block-line-map)
        (when all-comments
          (insert "\n")
          (notion-bridge-sync--insert-comments all-comments)
          (notion-bridge-sync--add-comment-overlays
           all-comments blocks block-line-map))
        (goto-char (min pos (point-max)))
        (org-mode)
        (setq notion-bridge--page-id page-id)
        (setq notion-bridge--last-edited last-edited)
        (let ((cache (notion-bridge-sync--build-cache-from-blocks
                      page-id last-edited blocks))
              (cache-key (or buffer-file-name (buffer-name))))
          (when all-comments
            (setq cache (notion-bridge-cache-set-comments
                         cache (notion-bridge-sync--serialize-comments all-comments))))
          (notion-bridge-cache-save cache-key cache)))
      (when (fboundp 'notion-bridge-mode)
        (notion-bridge-mode 1))
      (set-buffer-modified-p nil))
    (message "Refreshed: %s" (or title page-id))))

(defun notion-bridge-sync--extract-title (page)
  "Extract title string from a Notion PAGE plist.
Searches all properties for the one with type \"title\"."
  (let* ((properties (plist-get page :properties))
         (title-prop nil))
    ;; Find the property with type "title" (could be named anything)
    (cl-loop for (key val) on properties by #'cddr
             when (and (plistp val) (equal (plist-get val :type) "title"))
             do (setq title-prop val)
             and return nil)
    (when title-prop
      (let ((title-arr (plist-get title-prop :title)))
        (when title-arr
          (let ((result (mapconcat
                         (lambda (rt)
                           (or (plist-get rt :plain_text)
                               (plist-get (plist-get rt :text) :content)
                               ""))
                         title-arr
                         "")))
            (if (string-empty-p result) nil result)))))))

(defun notion-bridge-sync--build-cache-from-blocks (page-id last-edited blocks)
  "Build a cache plist from PAGE-ID, LAST-EDITED, and BLOCKS."
  (let ((cache (list :blocks nil)))
    (setq cache (notion-bridge-cache-set-page-info cache page-id last-edited))
    (cl-loop for block in blocks
             for idx from 0
             do (let* ((block-id (plist-get block :id))
                       (type (plist-get block :type))
                       (org-text (notion-bridge-org--block-to-org block))
                       (hash (notion-bridge-cache-compute-hash org-text)))
                  (setq cache (notion-bridge-cache-update-block
                               cache idx block-id hash type))))
    cache))

;;; ============================================================
;;; Push: Org buffer -> Notion
;;; ============================================================

(defun notion-bridge-sync-push ()
  "Push the current org buffer to Notion.
Replaces all blocks on the page with the current buffer contents."
  (interactive)
  (unless notion-bridge--page-id
    (error "No Notion page associated with this buffer.  Pull first"))
  (let* ((page-id notion-bridge--page-id)
         (cache-key (or buffer-file-name (buffer-name))))
    ;; Conflict detection: check if remote changed since last pull
    (notion-bridge-sync--check-conflicts page-id)
    ;; Parse current buffer to Notion blocks (strip comments section)
    (let* ((org-string (notion-bridge-sync--strip-comments-section
                        (buffer-substring-no-properties (point-min) (point-max))))
           (new-blocks (notion-bridge-org--org-to-blocks org-string))
           (new-count (length new-blocks)))
      ;; Delete all existing blocks on the page (in parallel)
      (message "Clearing existing blocks...")
      (let* ((existing (notion-bridge-api-get-blocks-all page-id))
             (ids (cl-remove nil (mapcar (lambda (b) (plist-get b :id))
                                        (reverse existing)))))
        (when ids
          (notion-bridge-api-delete-blocks-parallel ids)))
      ;; Append all new blocks (two-pass for nested children)
      (message "Pushing %d blocks..." new-count)
      (notion-bridge-sync--append-blocks-recursive page-id new-blocks)
      ;; Update the page title if it changed
      (notion-bridge-sync--update-title page-id)
      ;; Refresh cache from remote
      (let ((page (notion-bridge-api-get-page page-id)))
        (setq notion-bridge--last-edited (plist-get page :last_edited_time))
        (let* ((remote-blocks (notion-bridge-api-get-blocks-recursive page-id))
               (new-cache (notion-bridge-sync--build-cache-from-blocks
                           page-id notion-bridge--last-edited remote-blocks)))
          (notion-bridge-cache-save cache-key new-cache)))
      ;; Update keyword in buffer
      (notion-bridge-sync--update-keyword "NOTION_LAST_SYNCED"
                                          (format-time-string "%FT%T%z"))
      (set-buffer-modified-p nil)
      (message "Push complete: %d blocks synced" new-count))))

(defun notion-bridge-sync--update-title (page-id)
  "Update the Notion page title from the buffer's #+TITLE keyword."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
      (let ((title (match-string-no-properties 1)))
        (condition-case nil
            (notion-bridge-api--request-sync
             "PATCH" (format "/pages/%s" page-id)
             (list :properties
                   (list :title
                         (list :title
                               (vector (list :type "text"
                                             :text (list :content title)))))))
          (error nil))))))

;;; ============================================================
;;; Block-tracked insertion (for comment placement)
;;; ============================================================

(defun notion-bridge-sync--insert-blocks-tracked (blocks block-line-map &optional depth)
  "Insert BLOCKS as org text, recording block ID to line number in BLOCK-LINE-MAP.
DEPTH controls nesting level."
  (let ((depth (or depth 0))
        (prev-type nil))
    (dolist (block blocks)
      (let* ((type (plist-get block :type))
             (block-id (plist-get block :id))
             (text (notion-bridge-org--block-to-org block depth))
             (list-types '("bulleted_list_item" "numbered_list_item" "to_do"))
             (prev-is-list (member prev-type list-types))
             (curr-is-list (member type list-types))
             (sep (cond
                   ((null prev-type) "")
                   ((and prev-is-list curr-is-list) "\n")
                   (t "\n\n"))))
        (insert sep)
        ;; Record the line number for this block
        (when block-id
          (puthash block-id (line-number-at-pos) block-line-map))
        (insert text)
        (setq prev-type type)))))

;;; ============================================================
;;; Comment fetching
;;; ============================================================

(defun notion-bridge-sync--fetch-all-comments (page-id blocks)
  "Fetch all comments for PAGE-ID and its BLOCKS.
Uses parallel requests for block-level comments.
Returns a combined list of comment plists."
  (let ((all-comments nil))
    ;; Page-level comments
    (condition-case nil
        (setq all-comments (notion-bridge-api-get-comments page-id))
      (error nil))
    ;; Block-level comments in parallel
    (let ((block-ids (cl-remove nil (mapcar (lambda (b) (plist-get b :id)) blocks))))
      (when block-ids
        (condition-case nil
            (let ((block-cmts (notion-bridge-api-get-comments-parallel block-ids)))
              (setq all-comments (append all-comments block-cmts)))
          (error nil))))
    all-comments))

;;; ============================================================
;;; Comment serialization (for cache persistence)
;;; ============================================================

(defun notion-bridge-sync--serialize-comments (comments)
  "Convert raw API COMMENTS into a cache-friendly list of plists."
  (mapcar
   (lambda (c)
     (let* ((parent (plist-get c :parent))
            (parent-type (plist-get parent :type))
            (parent-id (or (plist-get parent :block_id)
                           (plist-get parent :page_id)))
            (author (or (plist-get (plist-get c :created_by) :name) "Unknown"))
            (date (let ((ct (plist-get c :created_time)))
                    (if ct (substring ct 0 10) "")))
            (rt (plist-get c :rich_text))
            (text (mapconcat (lambda (r) (or (plist-get r :plain_text) "")) rt ""))
            (discussion-id (plist-get c :discussion_id)))
       (list :parent-type parent-type
             :parent-id parent-id
             :discussion-id discussion-id
             :author author
             :date date
             :text text)))
   comments))

(defun notion-bridge-sync-restore-comments ()
  "Restore comment overlays from cache for the current buffer.
Called when `notion-bridge-mode' activates on a file-backed buffer."
  (let* ((cache-key (or buffer-file-name (buffer-name)))
         (cache (notion-bridge-cache-load cache-key))
         (comments (notion-bridge-cache-get-comments cache))
         (blocks (plist-get cache :blocks)))
    (when (and comments blocks)
      ;; Build block-line-map by finding NOTION_PAGE_ID lines and block text
      (let ((block-line-map (make-hash-table :test 'equal)))
        ;; Re-scan buffer to map block IDs to lines using the cache block list
        (save-excursion
          (goto-char (point-min))
          ;; Skip keywords
          (when (re-search-forward "^[^#]" nil t)
            (beginning-of-line)
            (let ((line (line-number-at-pos)))
              (dolist (block-info blocks)
                (when block-info
                  (let ((block-id (plist-get block-info :notion-id)))
                    (when block-id
                      (puthash block-id line block-line-map))))
                ;; Advance line (rough: one block per visual element)
                (forward-line 1)
                (when (looking-at "^$") (forward-line 1)) ;; skip blank lines
                (setq line (line-number-at-pos))))))
        ;; Add overlays for block-level comments
        (dolist (c comments)
          (when (equal (plist-get c :parent-type) "block_id")
            (let ((line (gethash (plist-get c :parent-id) block-line-map)))
              (when line
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (let* ((line-beg (line-beginning-position))
                         (line-end (line-end-position))
                         (tooltip (format "%s (%s): %s"
                                          (plist-get c :author)
                                          (plist-get c :date)
                                          (plist-get c :text)))
                         (indicator (propertize " [comment]"
                                                'face 'notion-bridge-comment-indicator-face))
                         (ov (make-overlay line-beg line-end)))
                    (overlay-put ov 'notion-bridge-comment t)
                    (overlay-put ov 'after-string indicator)
                    (overlay-put ov 'help-echo tooltip)
                    (overlay-put ov 'face 'notion-bridge-commented-line-face)
                    (overlay-put ov 'notion-bridge-discussion-id
                                 (plist-get c :discussion-id))))))))))))

;;; ============================================================
;;; Comment overlays
;;; ============================================================

(defface notion-bridge-comment-indicator-face
  '((t :foreground "#e67e22" :weight bold))
  "Face for the [N comments] indicator text."
  :group 'notion-bridge)

(defface notion-bridge-commented-line-face
  '((t :underline (:color "#27ae60" :style wave)))
  "Face for lines that have Notion comments.
Uses a wavy underline to indicate commented text."
  :group 'notion-bridge)

(defun notion-bridge-sync--add-comment-overlays (comments blocks block-line-map)
  "Add overlays for COMMENTS at the appropriate buffer lines.
Uses BLOCK-LINE-MAP to find which line each commented block is on.
Adds a wavy underline to the commented line and a count indicator at the end."
  ;; Group comments by their parent block_id
  (let ((block-comments (make-hash-table :test 'equal)))
    (dolist (comment comments)
      (let* ((parent (plist-get comment :parent))
             (parent-type (plist-get parent :type)))
        (when (equal parent-type "block_id")
          (let ((block-id (plist-get parent :block_id)))
            (puthash block-id
                     (append (gethash block-id block-comments) (list comment))
                     block-comments)))))
    ;; Add overlays for each commented block
    (maphash
     (lambda (block-id block-cmts)
       (let ((line (gethash block-id block-line-map)))
         (when line
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- line))
             (let* ((line-beg (line-beginning-position))
                    (line-end (line-end-position))
                    ;; Build tooltip text
                    (tooltip (mapconcat
                              (lambda (c)
                                (let* ((author (or (plist-get (plist-get c :created_by) :name) "Unknown"))
                                       (date (let ((ct (plist-get c :created_time)))
                                               (if ct (substring ct 0 10) "")))
                                       (rt (plist-get c :rich_text))
                                       (text (mapconcat
                                              (lambda (r) (or (plist-get r :plain_text) ""))
                                              rt "")))
                                  (format "%s (%s): %s" author date text)))
                              block-cmts "\n"))
                    (count (length block-cmts))
                    (indicator (propertize
                                (format " [%d comment%s]" count (if (> count 1) "s" ""))
                                'face 'notion-bridge-comment-indicator-face))
                    ;; Overlay spans the whole line so overlays-at works anywhere on it
                    (ov (make-overlay line-beg line-end)))
               (overlay-put ov 'notion-bridge-comment t)
               (overlay-put ov 'after-string indicator)
               (overlay-put ov 'help-echo tooltip)
               (overlay-put ov 'face 'notion-bridge-commented-line-face)
               ;; Store discussion-id for replying
               (overlay-put ov 'notion-bridge-discussion-id
                            (plist-get (car block-cmts) :discussion_id)))))))
     block-comments)))

;;; ============================================================
;;; Comments section (bottom of buffer)
;;; ============================================================

(defun notion-bridge-sync--insert-comments (comments)
  "Insert COMMENTS into the current buffer as an org section."
  ;; Group comments by discussion_id for threading
  (let ((threads (make-hash-table :test 'equal)))
    (dolist (comment comments)
      (let ((discussion-id (plist-get comment :discussion_id)))
        (puthash discussion-id
                 (append (gethash discussion-id threads) (list comment))
                 threads)))
    (insert "* Comments                                                      :NOTION_COMMENTS:\n")
    (if (= (hash-table-count threads) 0)
        (insert "No comments.\n")
      (let ((thread-num 0))
        (maphash
         (lambda (discussion-id thread-comments)
           (cl-incf thread-num)
           (insert (format "** Thread %d\n" thread-num))
           (insert (format ":PROPERTIES:\n:NOTION_DISCUSSION_ID: %s\n:END:\n"
                           discussion-id))
           (dolist (comment thread-comments)
             (let* ((author (plist-get (plist-get comment :created_by) :name))
                    (created (plist-get comment :created_time))
                    (date (if created (substring created 0 10) ""))
                    (rich-text (plist-get comment :rich_text))
                    (text (mapconcat
                           (lambda (rt) (or (plist-get rt :plain_text) ""))
                           rich-text "")))
               (insert (format "- *%s* (%s): %s\n"
                               (or author "Unknown") date text)))))
         threads)))))

(defun notion-bridge-sync-post-comment (text &optional discussion-id)
  "Post a comment with TEXT on the current page.
If DISCUSSION-ID is non-nil, reply to that thread."
  (unless notion-bridge--page-id
    (error "No Notion page associated with this buffer"))
  (notion-bridge-api-post-comment notion-bridge--page-id text discussion-id))

(defun notion-bridge-sync--append-blocks-recursive (parent-id blocks)
  "Append BLOCKS to PARENT-ID, handling nested children.
Notion API doesn't accept children in the append call, so we
create parent blocks first, then append children in a second pass."
  (let* (;; Strip children from blocks for the initial append
         (flat-blocks (mapcar
                       (lambda (b)
                         (let ((copy (cl-copy-list b)))
                           (cl-remf copy :children)
                           copy))
                       blocks))
         ;; Collect blocks that have children
         (blocks-with-children
          (cl-remove-if-not (lambda (b) (plist-get b :children)) blocks)))
    ;; First pass: append all blocks without children
    (let ((remaining flat-blocks))
      (while remaining
        (let ((batch (cl-subseq remaining 0 (min 100 (length remaining)))))
          (notion-bridge-api-append-blocks parent-id batch)
          (setq remaining (nthcdr (length batch) remaining)))))
    ;; Second pass: for blocks with children, find their new IDs and append children
    (when blocks-with-children
      (let ((created (notion-bridge-api-get-blocks-all parent-id)))
        ;; Match by position — created blocks are in same order
        (let ((created-offset (- (length created) (length blocks))))
          (cl-loop for block in blocks
                   for idx from 0
                   when (plist-get block :children)
                   do (let* ((created-idx (+ created-offset idx))
                             (created-block (nth created-idx created))
                             (created-id (plist-get created-block :id))
                             (children (cl-coerce (plist-get block :children) 'list)))
                        (when (and created-id children)
                          (notion-bridge-sync--append-blocks-recursive
                           created-id children)))))))))

(defun notion-bridge-sync--strip-comments-section (org-string)
  "Remove the * Comments :NOTION_COMMENTS: section from ORG-STRING."
  (replace-regexp-in-string
   "\\*+ Comments[[:space:]]*:NOTION_COMMENTS:\n\\(?:.*\n\\)*" ""
   org-string))

(defun notion-bridge-sync--check-conflicts (page-id)
  "Check if the remote page PAGE-ID changed since last pull.
Warns the user if there is a conflict."
  (let* ((page (notion-bridge-api-get-page page-id))
         (remote-edited (plist-get page :last_edited_time)))
    (when (and notion-bridge--last-edited
               remote-edited
               (not (equal notion-bridge--last-edited remote-edited)))
      (unless (yes-or-no-p
               (format "Remote page changed since last pull (%s vs %s).  Push anyway? "
                       notion-bridge--last-edited remote-edited))
        (user-error "Push aborted due to remote conflict")))))

(defun notion-bridge-sync--update-block (block-id new-block)
  "Update a single Notion block BLOCK-ID with NEW-BLOCK content."
  (let* ((type (plist-get new-block :type))
         (type-key (intern (concat ":" type)))
         (type-data (plist-get new-block type-key)))
    (notion-bridge-api-update-block
     block-id
     (list type-key type-data))))

(defun notion-bridge-sync--block-org-text (block)
  "Get the org-mode text representation of BLOCK for hashing."
  (notion-bridge-org--block-to-org block))

(defun notion-bridge-sync--update-keyword (keyword value)
  "Update or insert a file-level KEYWORD to VALUE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s:.*$" (regexp-quote keyword)) nil t)
        (replace-match (format "#+%s: %s" keyword value))
      ;; Insert after other keywords at top
      (goto-char (point-min))
      (if (re-search-forward "^[^#]" nil t)
          (progn (beginning-of-line)
                 (insert (format "#+%s: %s\n" keyword value)))
        (goto-char (point-max))
        (insert (format "\n#+%s: %s\n" keyword value))))))

;;; ============================================================
;;; Pull All: Notion -> Org directory
;;; ============================================================

(defvar notion-bridge-sync--page-index nil
  "Alist of (PAGE-ID . FILENAME) for the last pull-all.
Persisted to `notion-bridge-org-directory'/index.el.")

(defun notion-bridge-sync--slugify (title)
  "Convert TITLE to a filesystem-safe slug."
  (let ((slug (downcase (or title "untitled"))))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    (when (string-empty-p slug)
      (setq slug "untitled"))
    slug))

(defun notion-bridge-sync--unique-filename (slug dir existing)
  "Return a unique .org filename for SLUG in DIR.
EXISTING is a hash table of already-used filenames."
  (let ((base slug)
        (counter 1)
        (name (concat slug ".org")))
    (while (gethash name existing)
      (setq name (format "%s-%d.org" base counter))
      (cl-incf counter))
    (puthash name t existing)
    name))

(defun notion-bridge-sync-pull-all (org-directory)
  "Pull all accessible Notion pages into ORG-DIRECTORY as org files.
Creates inter-page links as org file links."
  (make-directory org-directory t)
  (message "Fetching all pages from Notion...")
  ;; 1. Fetch all pages
  (let* ((pages (notion-bridge-api-search-all
                 '(:value "page" :property "object")))
         (page-map (make-hash-table :test 'equal))
         (used-filenames (make-hash-table :test 'equal))
         (page-info '())
         (total (length pages))
         (count 0))
    (message "Found %d pages" total)
    ;; 2. Build page-id -> filename mapping
    (dolist (page pages)
      (let* ((page-id (plist-get page :id))
             (title (notion-bridge-sync--extract-title page))
             (slug (notion-bridge-sync--slugify title))
             (filename (notion-bridge-sync--unique-filename
                        slug org-directory used-filenames)))
        (puthash page-id filename page-map)
        (push (list :id page-id :title title :filename filename
                    :last-edited (plist-get page :last_edited_time))
              page-info)))
    (setq page-info (nreverse page-info))
    ;; 3. Set the page map for org conversion (enables inter-page links)
    (let ((notion-bridge-org--page-map page-map))
      ;; 4. Pull each page and save as org file
      (dolist (info page-info)
        (cl-incf count)
        (let* ((page-id (plist-get info :id))
               (title (plist-get info :title))
               (filename (plist-get info :filename))
               (filepath (expand-file-name filename org-directory)))
          (message "Pulling [%d/%d] %s..." count total (or title filename))
          (condition-case err
              (let* ((blocks (notion-bridge-api-get-blocks-recursive page-id))
                     (org-content (notion-bridge-org--blocks-to-org blocks))
                     (comments (notion-bridge-sync--fetch-all-comments page-id blocks))
                     (cache (notion-bridge-sync--build-cache-from-blocks
                             page-id (plist-get info :last-edited) blocks)))
                (with-temp-file filepath
                  (insert (format "#+TITLE: %s\n" (or title "")))
                  (insert (format "#+NOTION_PAGE_ID: %s\n" page-id))
                  (insert (format "#+NOTION_LAST_SYNCED: %s\n"
                                  (format-time-string "%FT%T%z")))
                  (insert "\n")
                  (insert org-content)
                  (when comments
                    (insert "\n")
                    (notion-bridge-sync--insert-comments comments))
                  (insert "\n"))
                ;; Save cache with comments
                (when comments
                  (setq cache (notion-bridge-cache-set-comments
                               cache (notion-bridge-sync--serialize-comments comments))))
                (notion-bridge-cache-save filepath cache))
            (error
             (message "Error pulling page %s: %s" page-id (error-message-string err)))))))
    ;; 5. Save the page index for future reference
    (let ((index-file (expand-file-name ".notion-index.el" org-directory)))
      (with-temp-file index-file
        (let ((print-level nil) (print-length nil))
          (insert ";; Notion Bridge page index\n")
          (insert ";; Maps page IDs to local filenames\n")
          (prin1 (let ((alist nil))
                   (maphash (lambda (k v) (push (cons k v) alist)) page-map)
                   alist)
                 (current-buffer))
          (insert "\n"))))
    (message "Pulled %d pages to %s" total org-directory)
    page-map))

(defun notion-bridge-sync-load-page-index (org-directory)
  "Load the page index from ORG-DIRECTORY.
Returns a hash table mapping page IDs to filenames."
  (let ((index-file (expand-file-name ".notion-index.el" org-directory)))
    (when (file-exists-p index-file)
      (let ((alist (with-temp-buffer
                     (insert-file-contents index-file)
                     (condition-case nil (read (current-buffer)) (error nil))))
            (page-map (make-hash-table :test 'equal)))
        (dolist (entry alist)
          (puthash (car entry) (cdr entry) page-map))
        page-map))))

(provide 'notion-bridge-sync)

;;; notion-bridge-sync.el ends here
