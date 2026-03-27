;;; notion-bridge.el --- Notion integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: notion-bridge
;; Keywords: tools, org, notion
;; Package-Requires: ((emacs "28.1") (transient "0.3"))

;;; Commentary:

;; Bidirectional sync between Notion pages and org-mode buffers.
;; Use `notion-bridge-pull' to fetch a page, `notion-bridge-push' to sync back.

;;; Code:

(require 'notion-bridge-api)
(require 'notion-bridge-org)
(require 'notion-bridge-sync)
(require 'notion-bridge-cache)
(require 'transient)
(require 'browse-url)

;;; Customization

(defcustom notion-bridge-org-directory (expand-file-name "~/notion/")
  "Default directory for saved Notion org files."
  :type 'directory
  :group 'notion-bridge)

;;; Interactive Commands

;;;###autoload
(defun notion-bridge-pull (page-id)
  "Pull a Notion page by PAGE-ID into an org buffer.
With prefix argument \\[universal-argument], always prompt for page ID
even if the current buffer has one."
  (interactive
   (list (let ((id (or notion-bridge--page-id
                       (notion-bridge--read-page-id-from-buffer))))
           (if (and id (not current-prefix-arg))
               id
             (read-string "Notion page ID: " id)))))
  ;; If current buffer already has this page ID, refresh in-place
  (let ((current-id (or notion-bridge--page-id
                        (notion-bridge--read-page-id-from-buffer))))
    (if (and current-id (equal current-id page-id))
        (notion-bridge-sync-refresh page-id)
      (notion-bridge-sync-pull page-id))))

;;;###autoload
(defun notion-bridge-push ()
  "Push the current org buffer to Notion."
  (interactive)
  ;; Ensure page ID is set from buffer keywords if needed
  (unless notion-bridge--page-id
    (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
  (notion-bridge-sync-push))

;;;###autoload
(defun notion-bridge-search (query)
  "Search Notion for QUERY and pull the selected page."
  (interactive "sSearch Notion: ")
  (let* ((resp (notion-bridge-api-search query
                                         '(:value "page" :property "object")))
         (results (plist-get resp :results))
         (candidates
          (mapcar (lambda (page)
                    (let* ((id (plist-get page :id))
                           (title (or (notion-bridge-sync--extract-title page)
                                      "(untitled)")))
                      (cons (format "%s  [%s]" title id) id)))
                  results)))
    (if (null candidates)
        (message "No results found for: %s" query)
      (let* ((choice (completing-read "Select page: " candidates nil t))
             (selected-id (cdr (assoc choice candidates))))
        (when selected-id
          (notion-bridge-sync-pull selected-id))))))

;;;###autoload
(defun notion-bridge-pull-all ()
  "Pull all accessible Notion pages into `notion-bridge-org-directory'.
Each page becomes an org file.  Links between Notion pages become
org file links between the local files."
  (interactive)
  (let ((dir (read-directory-name "Save Notion pages to: "
                                  notion-bridge-org-directory)))
    (notion-bridge-sync-pull-all dir)
    (dired dir)))

;;;###autoload
(defun notion-bridge-open-page ()
  "Open a previously-pulled Notion page from `notion-bridge-org-directory'.
Uses the page index to present titles for completion."
  (interactive)
  (let* ((dir notion-bridge-org-directory)
         (page-map (notion-bridge-sync-load-page-index dir)))
    (unless page-map
      (user-error "No page index found.  Run notion-bridge-pull-all first"))
    (let ((candidates nil))
      (maphash (lambda (_id filename)
                 (let ((filepath (expand-file-name filename dir)))
                   (when (file-exists-p filepath)
                     (push (cons filename filepath) candidates))))
               page-map)
      (if (null candidates)
          (user-error "No org files found in %s" dir)
        (let* ((choice (completing-read "Open page: " candidates nil t))
               (filepath (cdr (assoc choice candidates))))
          (find-file filepath))))))

;;;###autoload
(defun notion-bridge-link-buffer (page-id)
  "Link the current org buffer to a Notion PAGE-ID."
  (interactive "sNotion page ID to link: ")
  (setq notion-bridge--page-id page-id)
  (notion-bridge-sync--update-keyword "NOTION_PAGE_ID" page-id)
  (message "Buffer linked to Notion page: %s" page-id))

;;;###autoload
(defun notion-bridge-status ()
  "Show sync status for the current buffer in the minibuffer."
  (interactive)
  (unless notion-bridge--page-id
    (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
  (if notion-bridge--page-id
      (message "Notion page: %s | Last edited: %s | File: %s"
               notion-bridge--page-id
               (or notion-bridge--last-edited "unknown")
               (or buffer-file-name "(unsaved)"))
    (message "No Notion page linked to this buffer")))

;;;###autoload
(defun notion-bridge-comment (text)
  "Post a comment on the current Notion page.
If point is on a line with a comment overlay, reply to that thread.
If inside a comment heading with NOTION_DISCUSSION_ID, reply there.
Otherwise start a new thread."
  (interactive "sComment: ")
  (unless notion-bridge--page-id
    (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
  (unless notion-bridge--page-id
    (user-error "No Notion page linked to this buffer"))
  (let ((discussion-id (notion-bridge--get-discussion-id-at-point)))
    (notion-bridge-sync-post-comment text discussion-id)
    (message "Comment posted%s" (if discussion-id " (reply)" " (new thread)"))))

(defun notion-bridge--get-discussion-id-at-point ()
  "Get discussion ID from overlay on current line, or from org heading property."
  (or
   ;; Check for comment overlay anywhere on current line
   (cl-loop for ov in (overlays-in (line-beginning-position) (1+ (line-end-position)))
            when (overlay-get ov 'notion-bridge-discussion-id)
            return (overlay-get ov 'notion-bridge-discussion-id))
   ;; Check org heading property
   (save-excursion
     (or (org-entry-get nil "NOTION_DISCUSSION_ID")
         (when (org-up-heading-safe)
           (org-entry-get nil "NOTION_DISCUSSION_ID"))))))

;;;###autoload
(defun notion-bridge-browse ()
  "Open the current Notion page in a browser."
  (interactive)
  (unless notion-bridge--page-id
    (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
  (unless notion-bridge--page-id
    (user-error "No Notion page linked to this buffer"))
  (let ((url (format "https://notion.so/%s"
                      (replace-regexp-in-string "-" "" notion-bridge--page-id))))
    (browse-url url)))

;;;###autoload
(defun notion-bridge-refresh ()
  "Re-pull the current buffer from Notion in-place.
Confirms if the buffer has unsaved changes."
  (interactive)
  (unless notion-bridge--page-id
    (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
  (unless notion-bridge--page-id
    (user-error "No Notion page linked to this buffer"))
  (when (and (buffer-modified-p)
             (not (yes-or-no-p "Buffer has unsaved changes.  Refresh anyway? ")))
    (user-error "Refresh aborted"))
  (notion-bridge-sync-refresh notion-bridge--page-id))

(defun notion-bridge-set-token (token)
  "Set the Notion API TOKEN interactively.
Saves the token to the first auth-source file so it persists."
  (interactive "sNotion API token: ")
  (setq notion-bridge-api-token token)
  (require 'auth-source)
  (let ((auth-file (car (seq-filter #'stringp auth-sources))))
    (if auth-file
        (let ((file (expand-file-name auth-file)))
          (with-temp-buffer
            (when (file-exists-p file)
              (insert-file-contents file))
            ;; Remove any existing entry for api.notion.com
            (goto-char (point-min))
            (while (re-search-forward "^machine api\\.notion\\.com.*\n?" nil t)
              (replace-match ""))
            ;; Append new entry
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "machine api.notion.com password %s\n" token))
            (write-region (point-min) (point-max) file nil 'silent))
          ;; Clear auth-source cache so it picks up the new token
          (auth-source-forget-all-cached)
          (message "Notion API token saved to %s" file))
      (message "Notion API token set for this session (no auth-source file found)"))))

;;; Minor Mode

(defvar notion-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c n") #'notion-bridge-menu)
    map)
  "Keymap for `notion-bridge-mode'.")

(defun notion-bridge--read-page-id-from-buffer ()
  "Read the NOTION_PAGE_ID keyword from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+NOTION_PAGE_ID:\\s-*\\(\\S-+\\)" nil t)
      (match-string-no-properties 1))))

(defun notion-bridge--read-last-edited-from-buffer ()
  "Read the NOTION_LAST_SYNCED keyword from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+NOTION_LAST_SYNCED:\\s-*\\(\\S-+\\)" nil t)
      (match-string-no-properties 1))))

(defun notion-bridge--eldoc-function (&rest _)
  "Show comment text in echo area when cursor is on a commented line."
  (cl-loop for ov in (overlays-in (line-beginning-position) (1+ (line-end-position)))
           when (overlay-get ov 'notion-bridge-comment)
           return (overlay-get ov 'help-echo)))

;;;###autoload
(define-minor-mode notion-bridge-mode
  "Minor mode for Notion-synced org buffers.
Automatically detects NOTION_PAGE_ID from file keywords.
Shows comments in echo area via eldoc when cursor is on commented lines."
  :lighter " Notion"
  :keymap notion-bridge-mode-map
  (when notion-bridge-mode
    ;; Restore buffer-local vars from file keywords if not already set
    (unless notion-bridge--page-id
      (setq notion-bridge--page-id (notion-bridge--read-page-id-from-buffer)))
    (unless notion-bridge--last-edited
      (setq notion-bridge--last-edited (notion-bridge--read-last-edited-from-buffer)))
    ;; Restore comment overlays from cache
    (unless (cl-some (lambda (ov) (overlay-get ov 'notion-bridge-comment))
                     (overlays-in (point-min) (point-max)))
      (condition-case nil
          (notion-bridge-sync-restore-comments)
        (error nil)))
    ;; Enable eldoc for comment display
    (add-hook 'eldoc-documentation-functions #'notion-bridge--eldoc-function nil t)
    (eldoc-mode 1)))

;;; Transient Menu

;;;###autoload (autoload 'notion-bridge-menu "notion-bridge" nil t)
(transient-define-prefix notion-bridge-menu ()
  "Notion Bridge commands."
  ["Sync"
   ("p" "Pull page" notion-bridge-pull)
   ("a" "Pull all pages" notion-bridge-pull-all)
   ("P" "Push" notion-bridge-push)
   ("r" "Refresh" notion-bridge-refresh)]
  ["Navigate"
   ("s" "Search" notion-bridge-search)
   ("o" "Open local page" notion-bridge-open-page)
   ("b" "Browse in Notion" notion-bridge-browse)
   ("i" "Status" notion-bridge-status)
   ("c" "Comment" notion-bridge-comment)]
  ["Setup"
   ("t" "Set token" notion-bridge-set-token)
   ("l" "Link buffer" notion-bridge-link-buffer)])

(defun notion-bridge--maybe-enable ()
  "Enable `notion-bridge-mode' if the buffer has a NOTION_PAGE_ID keyword."
  (when (notion-bridge--read-page-id-from-buffer)
    (notion-bridge-mode 1)))

(add-hook 'org-mode-hook #'notion-bridge--maybe-enable)

(provide 'notion-bridge)

;;; notion-bridge.el ends here
