;;; notion-bridge-sync.el --- Sync orchestration for notion-bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: notion-bridge
;; Keywords: tools

;;; Commentary:

;; Pull and push between Notion pages and org buffers.
;;
;; Pulling records a "base": the tree of blocks the buffer was rendered
;; from, with each block's comparison key.  Pushing diffs the buffer
;; against that base, so only blocks you changed are written, and
;; checks every block it is about to touch against Notion first: a
;; block someone else changed since your pull is a conflict, never
;; silently overwritten.  Nothing is written until the whole plan has
;; been validated and confirmed, and every write is confined to the
;; open page by the API layer's write scope.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'notion-bridge-api)
(require 'notion-bridge-org)
(require 'notion-bridge-cache)

(defvar notion-bridge-org-directory)

;;; Buffer-local variables

(defvar-local notion-bridge--page-id nil
  "Notion page ID for the current buffer.")

(defvar-local notion-bridge--last-edited nil
  "Notion's last_edited_time for the page when it was last synced.")

(defvar notion-bridge-sync--writing-file nil
  "Non-nil while pull-all writes files itself instead of saving buffers.")

;;; ============================================================
;;; Pages and trees
;;; ============================================================

(defun notion-bridge-sync--title-property (page)
  "Return (NAME . TITLE) of PAGE's title property."
  (let ((properties (plist-get page :properties)) result)
    (cl-loop for (key val) on properties by #'cddr
             when (and (not result) (plistp val)
                       (equal (plist-get val :type) "title"))
             do (setq result
                      (cons (substring (symbol-name key) 1)
                            (let ((s (mapconcat
                                      (lambda (rt)
                                        (or (plist-get rt :plain_text)
                                            (plist-get (plist-get rt :text) :content)
                                            ""))
                                      (plist-get val :title) "")))
                              (if (string-empty-p s) nil s)))))
    result))

(defun notion-bridge-sync--extract-title (page)
  "Extract the title string from a Notion PAGE plist."
  (cdr (notion-bridge-sync--title-property page)))

(defun notion-bridge-sync--fetch (page-id)
  "Fetch PAGE-ID and its content tree."
  (let* ((page (notion-bridge-api-get-page page-id))
         (title (notion-bridge-sync--title-property page)))
    (list :page-id page-id
          :title (cdr title)
          :title-prop (or (car title) "title")
          :last-edited (plist-get page :last_edited_time)
          :blocks (notion-bridge-api-get-blocks-recursive page-id))))

(defun notion-bridge-sync--container-p (block depth)
  (and (notion-bridge-org--editable-p block depth)
       (not (equal (plist-get block :type) "table"))))

(defun notion-bridge-sync--base-tree (blocks &optional depth)
  "Base nodes for fetched BLOCKS at DEPTH."
  (let ((depth (or depth 0)))
    (mapcar (lambda (b)
              (list :id (plist-get b :id)
                    :type (plist-get b :type)
                    :key (notion-bridge-org--block-key b depth)
                    :children (and (notion-bridge-sync--container-p b depth)
                                   (notion-bridge-sync--base-tree
                                    (plist-get b :children) (1+ depth)))))
            blocks)))

(defun notion-bridge-sync--local-tree (blocks &optional depth)
  "Local nodes for BLOCKS parsed from the buffer, at DEPTH."
  (let ((depth (or depth 0)))
    (mapcar (lambda (b)
              (list :block b
                    :type (plist-get b :type)
                    :keep (plist-get b :keep-id)
                    :key (notion-bridge-org--block-key b depth)
                    :children (and (notion-bridge-sync--container-p b depth)
                                   (notion-bridge-sync--local-tree
                                    (plist-get b :children) (1+ depth)))))
            blocks)))

(defun notion-bridge-sync--buffer-blocks ()
  "Parse the current buffer into Notion blocks."
  (let ((notion-bridge-org--page-map (notion-bridge-sync--page-map)))
    (notion-bridge-org--org-to-blocks
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun notion-bridge-sync--page-map ()
  (or notion-bridge-org--page-map
      (and (boundp 'notion-bridge-org-directory)
           notion-bridge-org-directory
           (notion-bridge-sync-load-page-index notion-bridge-org-directory))))

;;; ============================================================
;;; Base cache
;;; ============================================================

(defun notion-bridge-sync--cache-key ()
  (or buffer-file-name (buffer-name)))

(defun notion-bridge-sync--save-base (key data &optional comments anchors)
  (notion-bridge-cache-save
   key
   (list :page-id (plist-get data :page-id)
         :title (plist-get data :title)
         :title-prop (plist-get data :title-prop)
         :last-edited (plist-get data :last-edited)
         :last-synced (format-time-string "%FT%T%z")
         :tree (plist-get data :tree)
         :comments comments
         :comment-anchors anchors)))

(defun notion-bridge-sync--load-base (key page-id)
  "The base saved under KEY for PAGE-ID, or nil."
  (let ((cache (notion-bridge-cache-load key)))
    (and cache
         (plist-member cache :tree)
         (equal (notion-bridge-api--norm (plist-get cache :page-id))
                (notion-bridge-api--norm page-id))
         cache)))

(defun notion-bridge-sync--migrate-cache ()
  "Move this buffer's base from its buffer name to its new file."
  (when (and notion-bridge--page-id buffer-file-name)
    (let ((by-name (notion-bridge-cache-load (buffer-name))))
      (when (and by-name (not (notion-bridge-cache-load buffer-file-name)))
        (notion-bridge-cache-save buffer-file-name by-name)))))

;;; ============================================================
;;; Pull
;;; ============================================================

(defun notion-bridge-sync--block-ids (blocks)
  (cl-loop for b in blocks
           collect (plist-get b :id)
           append (notion-bridge-sync--block-ids (plist-get b :children))))

(defun notion-bridge-sync--fetch-all-comments (page-id blocks &optional strict)
  "Fetch comments on PAGE-ID and every block in its fetched BLOCKS tree.
Comments are only displayed, so a failure is reported and yields nil,
unless STRICT, when it signals."
  (condition-case err
      (append (notion-bridge-api-get-comments page-id)
              (let ((ids (notion-bridge-sync--block-ids blocks)))
                (and ids (notion-bridge-api-get-comments-parallel ids))))
    (error
     (if strict (signal (car err) (cdr err))
       (message "notion-bridge: couldn't load comments: %s" (error-message-string err))
       nil))))

(defun notion-bridge-sync--find-block-starts (blocks depth acc)
  "Alist of block ID to the first line of its rendering."
  (dolist (b blocks)
    (push (cons (plist-get b :id)
                (car (split-string (notion-bridge-org--block-self-to-org b depth) "\n")))
          acc)
    (when (notion-bridge-sync--container-p b depth)
      (setq acc (notion-bridge-sync--find-block-starts
                 (plist-get b :children) (1+ depth) acc))))
  acc)

(defun notion-bridge-sync--render (data comments)
  "Replace the current buffer with DATA's page and COMMENTS.
Returns the comment anchors (block ID . first line) used."
  (let* ((inhibit-read-only t)
         (notion-bridge-org--page-map (notion-bridge-sync--page-map))
         (blocks (plist-get data :blocks)))
    (remove-overlays (point-min) (point-max) 'notion-bridge-comment t)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n" (or (plist-get data :title) "")))
    (insert (format "#+NOTION_PAGE_ID: %s\n" (plist-get data :page-id)))
    (insert (format "#+NOTION_LAST_SYNCED: %s\n" (format-time-string "%FT%T%z")))
    (insert "\n")
    (insert (notion-bridge-org--blocks-to-org blocks))
    (insert "\n")
    (when comments
      (insert "\n")
      (notion-bridge-sync--insert-comments comments))
    (let* ((commented (delete-dups
                       (cl-loop for c in comments
                                for parent = (plist-get c :parent)
                                when (equal (plist-get parent :type) "block_id")
                                collect (plist-get parent :block_id))))
           (starts (notion-bridge-sync--find-block-starts blocks 0 nil))
           (anchors (cl-loop for id in commented
                             for line = (cdr (assoc id starts))
                             when line collect (cons id line))))
      anchors)))

(defun notion-bridge-sync--pull-into (buffer page-id)
  "Pull PAGE-ID into BUFFER, record its base, and save BUFFER's file."
  (let* ((data (notion-bridge-sync--fetch page-id))
         (comments (notion-bridge-sync--fetch-all-comments
                    page-id (plist-get data :blocks)))
         (data (plist-put data :tree (notion-bridge-sync--base-tree
                                      (plist-get data :blocks)))))
    (with-current-buffer buffer
      (let ((anchors (notion-bridge-sync--render data comments)))
        (unless (derived-mode-p 'org-mode) (org-mode))
        (setq notion-bridge--page-id page-id
              notion-bridge--last-edited (plist-get data :last-edited))
        (notion-bridge-sync--save-base
         (notion-bridge-sync--cache-key) data
         (notion-bridge-sync--serialize-comments comments) anchors)
        (when (fboundp 'notion-bridge-mode) (notion-bridge-mode 1))
        (notion-bridge-sync--add-comment-overlays
         (notion-bridge-sync--serialize-comments comments) anchors)
        (if (and buffer-file-name (not notion-bridge-sync--writing-file))
            (save-buffer)
          (set-buffer-modified-p nil))))
    data))


(defun notion-bridge-sync--find-buffer-for-page (page-id)
  "Find an existing buffer that is linked to PAGE-ID, or nil."
  (cl-loop for buf in (buffer-list)
           when (equal (notion-bridge-api--norm
                        (buffer-local-value 'notion-bridge--page-id buf))
                       (notion-bridge-api--norm page-id))
           return buf))

(defun notion-bridge-sync-confirm-discard (buffer)
  "Ask before replacing BUFFER's content; signal `user-error' on no."
  (with-current-buffer buffer
    (let ((why (cond
                ((notion-bridge-sync-unpushed-changes)
                 "has changes that are not pushed to Notion")
                ((and (buffer-modified-p) (> (buffer-size) 0))
                 "has unsaved changes"))))
      (when (and why
                 (not (yes-or-no-p
                       (format "%s %s.  Replace it with Notion's version? "
                               (buffer-name) why))))
        (user-error "Kept %s as is" (buffer-name))))))

(defun notion-bridge-sync-pull (page-id)
  "Pull Notion page PAGE-ID into an org buffer and show it.
Reuses a buffer already showing the page, after confirming if it
holds changes that would be lost."
  (interactive "sNotion page ID: ")
  (let* ((existing (notion-bridge-sync--find-buffer-for-page page-id))
         (buf (or existing (generate-new-buffer (format "*notion: %s*" page-id)))))
    (when existing (notion-bridge-sync-confirm-discard existing))
    (let ((data (notion-bridge-sync--pull-into buf page-id)))
      (unless (or existing (buffer-file-name buf))
        (with-current-buffer buf
          (let ((record (notion-bridge-cache-load (buffer-name))))
            (rename-buffer (format "*notion: %s*" (or (plist-get data :title) page-id)) t)
            (notion-bridge-cache-save (buffer-name) record))))
      (unless existing
        (with-current-buffer buf (goto-char (point-min))))
      (pop-to-buffer-same-window buf)
      (message "Pulled Notion page: %s" (or (plist-get data :title) page-id))
      buf)))

(defun notion-bridge-sync-refresh (page-id)
  "Replace the current buffer with Notion's version of PAGE-ID.
Confirms first if that would lose unpushed or unsaved changes."
  (notion-bridge-sync-confirm-discard (current-buffer))
  (let ((pos (point))
        (data (notion-bridge-sync--pull-into (current-buffer) page-id)))
    (goto-char (min pos (point-max)))
    (message "Refreshed: %s" (or (plist-get data :title) page-id))))

;;; ============================================================
;;; Comments
;;; ============================================================

(defun notion-bridge-sync--comment-text (c)
  (mapconcat (lambda (r) (or (plist-get r :plain_text) "")) (plist-get c :rich_text) ""))

(defun notion-bridge-sync--serialize-comments (comments)
  "Convert raw API COMMENTS into a cache-friendly list of plists."
  (mapcar
   (lambda (c)
     (let ((parent (plist-get c :parent))
           (ct (plist-get c :created_time)))
       (list :parent-type (plist-get parent :type)
             :parent-id (or (plist-get parent :block_id)
                            (plist-get parent :page_id))
             :discussion-id (plist-get c :discussion_id)
             :author (or (plist-get (plist-get c :created_by) :name) "Unknown")
             :date (if ct (substring ct 0 10) "")
             :time (or ct "")
             :text (notion-bridge-sync--comment-text c))))
   comments))

(defun notion-bridge-sync--insert-comments (comments)
  "Insert COMMENTS as an org section, one subheading per thread."
  (let ((threads nil))
    (dolist (c comments)
      (let* ((id (plist-get c :discussion_id))
             (cell (assoc id threads)))
        (if cell (setcdr cell (append (cdr cell) (list c)))
          (setq threads (append threads (list (list id c)))))))
    (setq threads (sort threads (lambda (a b)
                                  (string< (or (plist-get (cadr a) :created_time) "")
                                           (or (plist-get (cadr b) :created_time) "")))))
    (insert "* Comments                                                      :NOTION_COMMENTS:\n")
    (cl-loop for (id . thread) in threads
             for n from 1
             do (insert (format "** Thread %d\n:PROPERTIES:\n:NOTION_DISCUSSION_ID: %s\n:END:\n" n id))
             (dolist (c thread)
               (let ((ct (plist-get c :created_time)))
                 (insert (format "- *%s* (%s): %s\n"
                                 (or (plist-get (plist-get c :created_by) :name) "Unknown")
                                 (if ct (substring ct 0 10) "")
                                 (replace-regexp-in-string
                                  "\n" " " (notion-bridge-sync--comment-text c)))))))))

(defface notion-bridge-comment-indicator-face
  '((t :foreground "#e67e22" :weight bold))
  "Face for the [N comments] indicator text."
  :group 'notion-bridge)

(defface notion-bridge-commented-line-face
  '((t :underline (:color "#27ae60" :style wave)))
  "Face for lines that have Notion comments."
  :group 'notion-bridge)

(defun notion-bridge-sync--add-comment-overlays (comments anchors)
  "Mark the lines of commented blocks, found by their exact text in ANCHORS."
  (remove-overlays (point-min) (point-max) 'notion-bridge-comment t)
  (save-excursion
    (dolist (anchor anchors)
      (let ((cs (cl-remove-if-not
                 (lambda (c) (equal (plist-get c :parent-id) (car anchor)))
                 comments)))
        (goto-char (point-min))
        (when (and cs (search-forward (cdr anchor) nil t))
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'notion-bridge-comment t)
            (overlay-put ov 'after-string
                         (propertize (format " [%d comment%s]" (length cs)
                                             (if (cdr cs) "s" ""))
                                     'face 'notion-bridge-comment-indicator-face))
            (overlay-put ov 'help-echo
                         (mapconcat (lambda (c) (format "%s (%s): %s"
                                                        (plist-get c :author)
                                                        (plist-get c :date)
                                                        (plist-get c :text)))
                                    cs "\n"))
            (overlay-put ov 'face 'notion-bridge-commented-line-face)
            (overlay-put ov 'notion-bridge-discussion-id
                         (plist-get (car cs) :discussion-id))))))))

(defun notion-bridge-sync-restore-comments ()
  "Restore comment overlays from the cached base for this buffer."
  (let ((cache (notion-bridge-cache-load (notion-bridge-sync--cache-key))))
    (when cache
      (notion-bridge-sync--add-comment-overlays
       (plist-get cache :comments) (plist-get cache :comment-anchors)))))

(defun notion-bridge-sync-post-comment (text &optional discussion-id)
  "Post a comment with TEXT on the current page, or reply to DISCUSSION-ID.
The thread must belong to this page; it is checked against Notion first."
  (unless notion-bridge--page-id
    (error "No Notion page associated with this buffer"))
  (let* ((page-id notion-bridge--page-id)
         (blocks (notion-bridge-api-get-blocks-recursive page-id))
         (discussions (delete-dups
                       (mapcar (lambda (c) (plist-get c :discussion_id))
                               (notion-bridge-sync--fetch-all-comments page-id blocks t)))))
    (when (and discussion-id
               (not (member (notion-bridge-api--norm discussion-id)
                            (mapcar #'notion-bridge-api--norm discussions))))
      (user-error "That thread is not on this page anymore; refresh first"))
    (notion-bridge-api-with-write-scope
        (notion-bridge-api-make-scope page-id nil discussions)
      (notion-bridge-api-post-comment page-id text discussion-id))))

;;; ============================================================
;;; Diff
;;; ============================================================

(defun notion-bridge-sync--lcs (a b test)
  "Index pairs of a longest common subsequence of vectors A and B under TEST."
  (let* ((n (length a)) (m (length b))
         (tbl (make-vector (* (1+ n) (1+ m)) 0)))
    (cl-flet ((at (i j) (aref tbl (+ (* i (1+ m)) j))))
      (cl-loop for i from (1- n) downto 0
               do (cl-loop for j from (1- m) downto 0
                           do (aset tbl (+ (* i (1+ m)) j)
                                    (if (funcall test (aref a i) (aref b j))
                                        (1+ (at (1+ i) (1+ j)))
                                      (max (at (1+ i) j) (at i (1+ j)))))))
      (let ((i 0) (j 0) pairs)
        (while (and (< i n) (< j m))
          (cond ((funcall test (aref a i) (aref b j))
                 (push (cons i j) pairs) (cl-incf i) (cl-incf j))
                ((>= (at (1+ i) j) (at i (1+ j))) (cl-incf i))
                (t (cl-incf j))))
        (nreverse pairs)))))

(defun notion-bridge-sync--summary (node)
  (let ((key (plist-get node :key)))
    (truncate-string-to-width
     (replace-regexp-in-string
      "[ \t\n]+" " "
      (if (string-prefix-p "keep:" key)
          (format "[%s %s]" (plist-get node :type) (substring key 5))
        (string-trim (replace-regexp-in-string
                      "​" "" (cadr (split-string key "\n"))))))
     60 nil nil "…")))

(defun notion-bridge-sync--recreatable-p (local)
  "Non-nil if LOCAL and its subtree can be created from the buffer."
  (and (not (plist-get local :keep))
       (cl-every #'notion-bridge-sync--recreatable-p (plist-get local :children))))

(defun notion-bridge-sync--diff (parent base local plan)
  "Diff BASE against LOCAL children of PARENT, adding operations to PLAN.
PLAN is a plist with :ops and :errors, returned updated."
  (let* ((bv (vconcat base)) (lv (vconcat local))
         (matches (notion-bridge-sync--lcs
                   bv lv (lambda (b l) (equal (plist-get b :key) (plist-get l :key)))))
         (fate (make-vector (length bv) nil))
         (lpair (make-vector (length lv) nil)))
    (cl-loop for (bi . li) in matches
             do (aset fate bi (cons 'match li)) (aset lpair li bi))
    (let ((bstart 0) (lstart 0))
      (dolist (seg (append matches (list (cons (length bv) (length lv)))))
        (let* ((bh (number-sequence bstart (1- (car seg))))
               (lh (number-sequence lstart (1- (cdr seg))))
               (pairs (notion-bridge-sync--lcs
                       (vconcat bh) (vconcat lh)
                       (lambda (bi li)
                         (let ((b (aref bv bi)) (l (aref lv li)))
                           (and (not (plist-get l :keep))
                                (not (string-prefix-p "keep:" (plist-get b :key)))
                                (equal (plist-get b :type) (plist-get l :type))))))))
          (cl-loop for (x . y) in pairs
                   do (let ((bi (nth x bh)) (li (nth y lh)))
                        (aset fate bi (cons 'pair li)) (aset lpair li bi))))
        (setq bstart (1+ (car seg)) lstart (1+ (cdr seg)))))
    (let ((fa (cl-position-if-not #'null lpair)))
      (when (and (> (length bv) 0) fa (eql (aref lpair fa) 0)
                 (cl-some (lambda (li) (not (plist-get (aref lv li) :keep)))
                          (number-sequence 0 (1- fa))))
        (if (notion-bridge-sync--recreatable-p (aref lv fa))
            (progn (aset fate 0 'recreate)
                   (aset lpair fa nil))
          (setq plan (plist-put
                      plan :errors
                      (cons (format "Can't add a block above \"%s\": Notion only inserts after an existing block.  Add it in Notion, or below it"
                                    (notion-bridge-sync--summary (aref bv 0)))
                            (plist-get plan :errors)))))))
    (cl-loop for b across bv
             for f across fate
             do (pcase f
                  (`(match . ,li)
                   (setq plan (notion-bridge-sync--diff
                               (plist-get b :id) (plist-get b :children)
                               (plist-get (aref lv li) :children) plan)))
                  (`(pair . ,li)
                   (let ((l (aref lv li)))
                     (if (equal (plist-get b :type) "table")
                         (setq plan (notion-bridge-sync--add-op
                                     plan (list :op 'replace :id (plist-get b :id)
                                                :parent parent :node l :base b)))
                       (setq plan (notion-bridge-sync--add-op
                                   plan (list :op 'update :id (plist-get b :id)
                                              :node l :base b)))
                       (setq plan (notion-bridge-sync--diff
                                   (plist-get b :id) (plist-get b :children)
                                   (plist-get l :children) plan)))))
                  ('recreate
                   (setq plan (notion-bridge-sync--add-op
                               plan (list :op 'delete :id (plist-get b :id)
                                          :base b :recreated t))))
                  (_ (setq plan (notion-bridge-sync--add-op
                                 plan (list :op 'delete :id (plist-get b :id) :base b))))))
    (let ((anchor (and (> (length bv) 0) (plist-get (aref bv 0) :id)))
          (group nil))
      (cl-flet ((flush ()
                  (when group
                    (setq plan (notion-bridge-sync--add-op
                                plan (list :op 'insert :parent parent
                                           :after anchor :nodes (reverse group))))
                    (setq group nil))))
        (cl-loop for l across lv
                 for li from 0
                 do (let ((bi (aref lpair li)))
                      (cond
                       (bi (flush) (setq anchor (plist-get (aref bv bi) :id)))
                       ((plist-get l :keep)
                        (setq plan (plist-put
                                    plan :errors
                                    (cons (format "The read-only block %s was moved; read-only blocks can only stay where they were or be removed"
                                                  (plist-get l :keep))
                                          (plist-get plan :errors)))))
                       (t (push l group)))))
        (flush)))
    plan))

(defun notion-bridge-sync--drop-ops-for (plan base-node)
  "Remove ops in PLAN that target BASE-NODE or its descendants."
  (let ((ids (notion-bridge-sync--tree-ids (list base-node))))
    (plist-put plan :ops
               (cl-remove-if (lambda (op)
                               (or (member (plist-get op :id) ids)
                                   (member (plist-get op :parent) ids)))
                             (plist-get plan :ops)))))

(defun notion-bridge-sync--tree-ids (nodes)
  (cl-loop for n in nodes
           collect (plist-get n :id)
           append (notion-bridge-sync--tree-ids (plist-get n :children))))

(defun notion-bridge-sync--add-op (plan op)
  (plist-put plan :ops (append (plist-get plan :ops) (list op))))

(defun notion-bridge-sync--plan (page-id base-tree local-tree)
  (notion-bridge-sync--diff page-id base-tree local-tree (list :ops nil :errors nil)))

(defun notion-bridge-sync--local-title ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
      (match-string-no-properties 1))))

(defun notion-bridge-sync-unpushed-changes ()
  "Non-nil if this buffer holds edits not yet pushed to Notion.
Returns nil when there is no sync record to compare against."
  (let* ((page-id (or notion-bridge--page-id
                      (save-excursion
                        (goto-char (point-min))
                        (and (re-search-forward "^#\\+NOTION_PAGE_ID:[ \t]*\\(\\S-+\\)" nil t)
                             (match-string-no-properties 1)))))
         (base (and page-id (notion-bridge-sync--load-base
                             (notion-bridge-sync--cache-key) page-id))))
    (when base
      (or (not (equal (or (notion-bridge-sync--local-title) "")
                      (or (plist-get base :title) "")))
          (plist-get (notion-bridge-sync--plan
                      page-id (plist-get base :tree)
                      (notion-bridge-sync--local-tree
                       (notion-bridge-sync--buffer-blocks)))
                     :ops)))))

;;; ============================================================
;;; Validation against Notion
;;; ============================================================

(defun notion-bridge-sync--index (nodes parent table)
  (dolist (n nodes)
    (puthash (notion-bridge-api--norm (plist-get n :id)) (cons n parent) table)
    (notion-bridge-sync--index (plist-get n :children) (plist-get n :id) table))
  table)

(defun notion-bridge-sync--same-subtree-p (a b)
  (and (equal (plist-get a :key) (plist-get b :key))
       (= (length (plist-get a :children)) (length (plist-get b :children)))
       (cl-every #'notion-bridge-sync--same-subtree-p
                 (plist-get a :children) (plist-get b :children))))

(defun notion-bridge-sync--validate (plan remote-tree scope)
  "Check PLAN against REMOTE-TREE and SCOPE.
Returns (ERRORS . CONFLICTS): problems that block the push, and
blocks changed in Notion since the pull that the push would touch."
  (let ((index (notion-bridge-sync--index remote-tree nil (make-hash-table :test #'equal)))
        (errors (plist-get plan :errors))
        (conflicts nil))
    (dolist (op (plist-get plan :ops))
      (let* ((id (plist-get op :id))
             (remote (and id (car (gethash (notion-bridge-api--norm id) index))))
             (base (plist-get op :base)))
        (pcase (plist-get op :op)
          ((or 'update 'replace 'delete)
           (cond
            ((null remote)
             (push (format "\"%s\" was removed in Notion since your pull"
                           (notion-bridge-sync--summary base))
                   errors))
            ((not (notion-bridge-sync--same-subtree-p
                   (if (eq (plist-get op :op) 'update)
                       (list :key (plist-get remote :key))
                     remote)
                   (if (eq (plist-get op :op) 'update)
                       (list :key (plist-get base :key))
                     base)))
             (push (notion-bridge-sync--summary base) conflicts)))
           (when (and (memq (plist-get op :op) '(replace delete))
                      remote
                      (not (gethash (notion-bridge-api--norm id) (plist-get scope :delete))))
             (push (format "Won't delete \"%s\": it is or contains a subpage, database, or synced block"
                           (notion-bridge-sync--summary base))
                   errors)))
          ('insert
           (let ((after (plist-get op :after))
                 (parent (plist-get op :parent)))
             (when (and (stringp after)
                        (not (gethash (notion-bridge-api--norm after) index)))
               (push (format "The block new content goes after was removed in Notion")
                     errors))
             (unless (or (equal (notion-bridge-api--norm parent) (plist-get scope :page))
                         (gethash (notion-bridge-api--norm parent) index))
               (push "A block new content goes into was removed in Notion" errors)))))))
    (cons (delete-dups (nreverse errors)) (delete-dups (nreverse conflicts)))))

;;; ============================================================
;;; Execution
;;; ============================================================

(defun notion-bridge-sync--resolve-after (after)
  (if (stringp after) after
    (or (plist-get (plist-get after :node) :created-id)
        (error "Internal: anchor block was not created"))))

(defun notion-bridge-sync--create (parent after nodes)
  "Create NODES under PARENT after AFTER, then their children.
Records each node's :created-id.  Returns the created base nodes."
  (let ((created nil) (remaining nodes) (anchor after))
    (while remaining
      (let* ((batch (cl-subseq remaining 0 (min 100 (length remaining))))
             (made (notion-bridge-api-append-blocks
                    parent
                    (mapcar (lambda (n) (notion-bridge-org--wire (plist-get n :block)))
                            batch)
                    anchor)))
        (unless (= (length made) (length batch))
          (error "Notion created %d of %d blocks under %s"
                 (length made) (length batch) parent))
        (cl-loop for n in batch
                 for m in made
                 do (plist-put n :created-id (plist-get m :id))
                 (push (list :id (plist-get m :id)
                             :type (plist-get n :type)
                             :key (plist-get n :key)
                             :children (and (plist-get n :children)
                                            (notion-bridge-sync--create
                                             (plist-get m :id) nil
                                             (plist-get n :children))))
                       created))
        (setq anchor (plist-get (car (last made)) :id)
              remaining (nthcdr (length batch) remaining))))
    (nreverse created)))

(defun notion-bridge-sync--update-payload (node)
  (let* ((wire (notion-bridge-org--wire (plist-get node :block)))
         (type (plist-get wire :type))
         (key (intern (concat ":" type))))
    (list key (let ((data (copy-sequence (plist-get wire key))))
                (unless (plist-get data :rich_text)
                  (setq data (plist-put data :rich_text [])))
                data))))

(defun notion-bridge-sync--base-map (fn nodes)
  "Rebuild base NODES, letting FN rewrite each level's children list."
  (funcall fn nil (mapcar (lambda (n)
                            (plist-put (copy-sequence n) :children
                                       (notion-bridge-sync--base-map-under
                                        fn (plist-get n :id) (plist-get n :children))))
                          nodes)))

(defun notion-bridge-sync--base-map-under (fn parent nodes)
  (funcall fn parent (mapcar (lambda (n)
                               (plist-put (copy-sequence n) :children
                                          (notion-bridge-sync--base-map-under
                                           fn (plist-get n :id) (plist-get n :children))))
                             nodes)))

(defun notion-bridge-sync--base-apply (tree page-id op created)
  "TREE after OP succeeded; CREATED are the base nodes it created."
  (let ((norm #'notion-bridge-api--norm))
    (notion-bridge-sync--base-map
     (lambda (parent nodes)
       (let ((parent (or parent page-id)))
         (pcase (plist-get op :op)
           ('update
            (mapcar (lambda (n)
                      (if (equal (funcall norm (plist-get n :id))
                                 (funcall norm (plist-get op :id)))
                          (plist-put (plist-put n :key (plist-get (plist-get op :node) :key))
                                     :type (plist-get (plist-get op :node) :type))
                        n))
                    nodes))
           ('delete
            (cl-remove-if (lambda (n) (equal (funcall norm (plist-get n :id))
                                             (funcall norm (plist-get op :id))))
                          nodes))
           ((or 'insert 'replace)
            (if (not (equal (funcall norm parent)
                            (funcall norm (plist-get op :parent))))
                nodes
              (let* ((after (if (eq (plist-get op :op) 'replace)
                                (plist-get op :id)
                              (plist-get op :resolved-after)))
                     (pos (and after (cl-position (funcall norm after) nodes
                                                  :key (lambda (n) (funcall norm (plist-get n :id)))
                                                  :test #'equal)))
                     (nodes (append (if pos (cl-subseq nodes 0 (1+ pos)) (if after nil nodes))
                                    created
                                    (if pos (nthcdr (1+ pos) nodes) (if after nodes nil)))))
                (if (eq (plist-get op :op) 'replace)
                    (cl-remove-if (lambda (n) (equal (funcall norm (plist-get n :id))
                                                     (funcall norm (plist-get op :id))))
                                  nodes)
                  nodes))))
           (_ nodes))))
     tree)))

(defun notion-bridge-sync--execute (plan page-id base-tree)
  "Run PLAN's operations; return the updated base tree.
On failure, signals with the base reflecting the writes that succeeded
stored in the error data as (notion-bridge-partial TREE DONE TOTAL)."
  (let* ((ops (plist-get plan :ops))
         (ordered (append (cl-remove-if-not (lambda (o) (eq (plist-get o :op) 'update)) ops)
                          (cl-remove-if-not (lambda (o) (memq (plist-get o :op) '(insert replace))) ops)
                          (cl-remove-if-not (lambda (o) (eq (plist-get o :op) 'delete)) ops)))
         (tree base-tree)
         (done 0))
    (condition-case err
        (dolist (op ordered)
          (let ((created nil))
            (pcase (plist-get op :op)
              ('update
               (notion-bridge-api-update-block
                (plist-get op :id)
                (notion-bridge-sync--update-payload (plist-get op :node))))
              ('insert
               (let ((after (and (plist-get op :after)
                                 (notion-bridge-sync--resolve-after (plist-get op :after)))))
                 (plist-put op :resolved-after after)
                 (setq created (notion-bridge-sync--create
                                (plist-get op :parent) after (plist-get op :nodes)))))
              ('replace
               (setq created (notion-bridge-sync--create
                              (plist-get op :parent) (plist-get op :id)
                              (list (plist-get op :node))))
               (notion-bridge-api-delete-block (plist-get op :id)))
              ('delete
               (notion-bridge-api-delete-block (plist-get op :id))))
            (setq tree (notion-bridge-sync--base-apply tree page-id op created))
            (cl-incf done)))
      (error (signal 'error (list (error-message-string err)
                                  'notion-bridge-partial tree done (length ordered)))))
    tree))

;;; ============================================================
;;; Push
;;; ============================================================

(defun notion-bridge-sync--describe (plan title-change)
  (let* ((ops (plist-get plan :ops))
         (count (lambda (kind) (cl-count-if (lambda (o) (eq (plist-get o :op) kind)) ops)))
         (inserted (apply #'+ (mapcar (lambda (o) (length (plist-get o :nodes)))
                                      (cl-remove-if-not (lambda (o) (eq (plist-get o :op) 'insert)) ops))))
         (deletes (cl-remove-if-not (lambda (o) (and (eq (plist-get o :op) 'delete)
                                                     (not (plist-get o :recreated))))
                                    ops)))
    (concat
     (string-join
      (delq nil (list (and title-change "new title")
                      (let ((n (+ (funcall count 'update) (funcall count 'replace))))
                        (and (> n 0) (format "%d edited block%s" n (if (= n 1) "" "s"))))
                      (and (> inserted 0) (format "%d new block%s" inserted (if (= inserted 1) "" "s")))
                      (and deletes (format "%d deleted block%s" (length deletes)
                                           (if (cdr deletes) "s" "")))))
      ", ")
     (mapconcat (lambda (o) (format "\n  - delete: %s"
                                    (notion-bridge-sync--summary (plist-get o :base))))
                deletes ""))))

(defun notion-bridge-sync-push ()
  "Push this buffer's changes to its Notion page.
Only blocks that changed since the last pull are written.  The plan
is checked against Notion and confirmed before anything is written."
  (interactive)
  (unless notion-bridge--page-id
    (user-error "No Notion page associated with this buffer.  Pull first"))
  (let* ((page-id notion-bridge--page-id)
         (key (notion-bridge-sync--cache-key))
         (base (notion-bridge-sync--load-base key page-id))
         (remote (notion-bridge-sync--fetch page-id))
         (remote-tree (notion-bridge-sync--base-tree (plist-get remote :blocks))))
    (unless base
      (if (plist-get remote :blocks)
          (user-error "No sync record for this buffer and the page isn't empty.  Pull it first")
        (setq base (list :page-id page-id :title (plist-get remote :title)
                         :title-prop (plist-get remote :title-prop) :tree nil))))
    (let* ((local-tree (notion-bridge-sync--local-tree (notion-bridge-sync--buffer-blocks)))
           (plan (notion-bridge-sync--plan page-id (plist-get base :tree) local-tree))
           (local-title (or (notion-bridge-sync--local-title) ""))
           (title-change (not (equal local-title (or (plist-get base :title) ""))))
           (scope (notion-bridge-api-make-scope page-id (plist-get remote :blocks)))
           (checked (notion-bridge-sync--validate plan remote-tree scope))
           (conflicts (cdr checked)))
      (when (car checked)
        (user-error "Can't push:\n  - %s" (string-join (car checked) "\n  - ")))
      (when (and title-change
                 (not (equal (or (plist-get remote :title) "") (or (plist-get base :title) ""))))
        (push "the page title" conflicts))
      (if (and (null (plist-get plan :ops)) (not title-change))
          (message "Nothing to push")
        (when (and conflicts
                   (not (yes-or-no-p
                         (format "Changed in Notion since your pull:\n  - %s\nOverwrite those with your version? "
                                 (string-join conflicts "\n  - ")))))
          (user-error "Push aborted; refresh to get Notion's changes"))
        (unless (yes-or-no-p (format "Push to \"%s\": %s\nProceed? "
                                     (or (plist-get remote :title) page-id)
                                     (notion-bridge-sync--describe plan title-change)))
          (user-error "Push aborted"))
        (let ((tree (plist-get base :tree))
              (was-modified (buffer-modified-p)))
          (notion-bridge-api-with-write-scope scope
            (condition-case err
                (setq tree (notion-bridge-sync--execute plan page-id tree))
              (error
               (let ((partial (memq 'notion-bridge-partial (cdr err))))
                 (when partial
                   (notion-bridge-sync--save-base
                    key (plist-put (copy-sequence base) :tree (nth 1 partial))
                    (plist-get base :comments) (plist-get base :comment-anchors)))
                 (user-error "Push stopped%s: %s.  Notion has the changes made so far; push again to continue"
                             (if partial (format " after %d of %d changes" (nth 2 partial) (nth 3 partial)) "")
                             (cadr err)))))
            (when title-change
              (notion-bridge-api-update-page-title page-id (plist-get remote :title-prop) local-title)))
          (let ((after (notion-bridge-api-get-page page-id)))
            (setq notion-bridge--last-edited (plist-get after :last_edited_time))
            (notion-bridge-sync--save-base
             key (list :page-id page-id :title local-title
                       :title-prop (plist-get remote :title-prop)
                       :last-edited (plist-get after :last_edited_time)
                       :tree tree)
             (plist-get base :comments) (plist-get base :comment-anchors)))
          (notion-bridge-sync--update-keyword "NOTION_LAST_SYNCED" (format-time-string "%FT%T%z"))
          (when (and buffer-file-name (not was-modified)) (save-buffer))
          (let ((others (notion-bridge-sync--remote-only-changes
                         (plist-get base :tree) remote-tree plan)))
            (message "Pushed to Notion%s"
                     (if (> others 0)
                         (format "; Notion has %d other change%s you haven't pulled (refresh to see them)"
                                 others (if (= others 1) "" "s"))
                       ""))))))))

(defun notion-bridge-sync--flatten (nodes)
  (cl-loop for n in nodes collect n append (notion-bridge-sync--flatten (plist-get n :children))))

(defun notion-bridge-sync--remote-only-changes (base-tree remote-tree plan)
  "Blocks changed in Notion since the pull that PLAN did not touch."
  (let* ((norm #'notion-bridge-api--norm)
         (touched (mapcar (lambda (op) (funcall norm (plist-get op :id)))
                          (plist-get plan :ops)))
         (base (notion-bridge-sync--index base-tree nil (make-hash-table :test #'equal)))
         (remote (notion-bridge-sync--index remote-tree nil (make-hash-table :test #'equal)))
         (count 0))
    (maphash (lambda (id entry)
               (let ((old (car (gethash id base))))
                 (unless (or (member id touched)
                             (and old (equal (plist-get old :key)
                                             (plist-get (car entry) :key))))
                   (cl-incf count))))
             remote)
    (maphash (lambda (id _)
               (unless (or (gethash id remote) (member id touched))
                 (cl-incf count)))
             base)
    count))

(defun notion-bridge-sync--update-keyword (keyword value)
  "Update or insert a file-level KEYWORD to VALUE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s:.*$" (regexp-quote keyword)) nil t)
        (replace-match (format "#+%s: %s" keyword value) t t)
      (goto-char (point-min))
      (if (re-search-forward "^[^#]" nil t)
          (progn (beginning-of-line)
                 (insert (format "#+%s: %s\n" keyword value)))
        (goto-char (point-max))
        (insert (format "\n#+%s: %s\n" keyword value))))))

;;; ============================================================
;;; Pull All: Notion -> Org directory
;;; ============================================================

(defun notion-bridge-sync--slugify (title)
  "Convert TITLE to a filesystem-safe slug."
  (let ((slug (downcase (or title "untitled"))))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    (if (string-empty-p slug) "untitled" slug)))

(defun notion-bridge-sync--unique-filename (slug dir existing)
  "Return a unique .org filename for SLUG in DIR.
EXISTING is a hash table of already-used filenames."
  (let ((counter 1)
        (name (concat slug ".org")))
    (while (or (gethash name existing)
               (and dir (file-exists-p (expand-file-name name dir))))
      (setq name (format "%s-%d.org" slug counter))
      (cl-incf counter))
    (puthash name t existing)
    name))

(defun notion-bridge-sync--file-unpushed-p (file)
  "Non-nil if FILE, or a buffer visiting it, holds changes Notion lacks."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (with-current-buffer buf
          (or (buffer-modified-p) (notion-bridge-sync-unpushed-changes)))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (unwind-protect
            (let ((org-mode-hook nil))
              (org-mode)
              (notion-bridge-sync-unpushed-changes))
          (setq buffer-file-name nil))))))

(defun notion-bridge-sync-pull-all (dir)
  "Pull all accessible Notion pages into DIR as org files.
Files keep their names across runs, and files with changes not yet
pushed to Notion are skipped rather than overwritten."
  (make-directory dir t)
  (message "Fetching all pages from Notion...")
  (let* ((pages (notion-bridge-api-search-all '(:value "page" :property "object")))
         (old-map (or (notion-bridge-sync-load-page-index dir)
                      (make-hash-table :test 'equal)))
         (page-map (make-hash-table :test 'equal))
         (used (make-hash-table :test 'equal))
         (skipped nil) (failed nil)
         (total (length pages)) (count 0))
    (maphash (lambda (id file) (puthash id file page-map) (puthash file t used)) old-map)
    (dolist (page pages)
      (unless (gethash (plist-get page :id) page-map)
        (puthash (plist-get page :id)
                 (notion-bridge-sync--unique-filename
                  (notion-bridge-sync--slugify (notion-bridge-sync--extract-title page))
                  dir used)
                 page-map)))
    (notion-bridge-sync--save-page-index dir page-map)
    (let ((notion-bridge-org--page-map page-map))
      (dolist (page pages)
        (cl-incf count)
        (let* ((page-id (plist-get page :id))
               (file (expand-file-name (gethash page-id page-map) dir)))
          (message "Pulling [%d/%d] %s..." count total (file-name-nondirectory file))
          (if (and (file-exists-p file) (notion-bridge-sync--file-unpushed-p file))
              (push (file-name-nondirectory file) skipped)
            (condition-case err
                (let ((buf (find-buffer-visiting file)))
                  (if buf
                      (notion-bridge-sync--pull-into buf page-id)
                    (with-temp-buffer
                      (setq buffer-file-name file)
                      (unwind-protect
                          (let ((org-mode-hook nil)
                                (notion-bridge-sync--writing-file t))
                            (notion-bridge-sync--pull-into (current-buffer) page-id)
                            (write-region (point-min) (point-max) file nil 'silent))
                        (set-buffer-modified-p nil)
                        (setq buffer-file-name nil)))))
              (error (push (format "%s (%s)" (file-name-nondirectory file)
                                   (error-message-string err))
                           failed)))))))
    (message "Pulled %d pages to %s%s%s"
             (- total (length skipped) (length failed)) dir
             (if skipped (format "; skipped %d with unpushed changes: %s"
                                 (length skipped) (string-join skipped ", "))
               "")
             (if failed (format "; %d failed: %s" (length failed)
                                (string-join failed ", "))
               ""))
    page-map))

(defun notion-bridge-sync--save-page-index (dir page-map)
  (with-temp-file (expand-file-name ".notion-index.el" dir)
    (let ((print-level nil) (print-length nil))
      (insert ";; Notion Bridge page index: page ID -> local file\n")
      (prin1 (let (alist) (maphash (lambda (k v) (push (cons k v) alist)) page-map) alist)
             (current-buffer))
      (insert "\n"))))

(defun notion-bridge-sync-load-page-index (dir)
  "Load the page index from DIR as a hash table, or nil."
  (let ((index-file (expand-file-name ".notion-index.el" dir)))
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
