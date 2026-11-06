;;; notion-bridge-push-test.el --- End-to-end sync tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Pull, edit, and push against the in-memory Notion in
;; notion-bridge-fake.el, checking what Notion ends up with and which
;; requests were made.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'notion-bridge)
(require 'notion-bridge-fake)

(defvar notion-bridge-org-directory)

(defvar nbt--page nil)
(defvar nbt--blocks nil "Alist of name -> block id in the fixture page.")

(defun nbt--fixture ()
  "A page with ordinary content plus a subpage, an image and a database."
  (setq nbt--page (fake-notion-add-page "Plan" "Name"))
  (setq nbt--blocks
        (list (cons 'intro (fake-notion-para nbt--page "Intro paragraph"))
              (cons 'mention (fake-notion-add nbt--page "paragraph"
                                              (list :rich_text
                                                    (vector (list :type "mention" :plain_text "Other page"
                                                                  :mention (list :type "page" :page (list :id "abc-123"))
                                                                  :annotations (list :bold :json-false))))))
              (cons 'head (fake-notion-add nbt--page "heading_1"
                                           (list :rich_text (vector (fake-notion-rt "Section")))))
              (cons 'list (fake-notion-add nbt--page "bulleted_list_item"
                                           (list :rich_text (vector (fake-notion-rt "First item")))
                                           (list (list "bulleted_list_item"
                                                       (list :rich_text (vector (fake-notion-rt "Nested")))))))
              (cons 'sub (fake-notion-add nbt--page "child_page" (list :title "Subpage")))
              (cons 'image (fake-notion-add nbt--page "image"
                                            (list :type "file" :file (list :url "https://s3/x.png"))))
              (cons 'db (fake-notion-add nbt--page "child_database" (list :title "Tasks")))
              (cons 'tech (fake-notion-para nbt--page "See src/pkg/engine.go and my_var_name, 2*3"))
              (cons 'last (fake-notion-para nbt--page "Last paragraph"))))
  (puthash (fake-notion--norm (cdr (assq 'sub nbt--blocks)))
           (list :object "page" :id (cdr (assq 'sub nbt--blocks)) :in_trash :json-false
                 :properties (list :title (list :type "title" :title [])))
           fake-notion--pages))

(defun nbt--id (name) (cdr (assq name nbt--blocks)))

(defmacro nbt--with-page (&rest body)
  "Pull the fixture page into a buffer and run BODY there."
  (declare (indent 0))
  `(with-fake-notion
     (let ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t)))
       (nbt--fixture)
       (let ((buf (notion-bridge-sync-pull nbt--page)))
         (unwind-protect
             (with-current-buffer buf
               (setq fake-notion--log nil)
               ,@body)
           (kill-buffer buf)
           (delete-directory notion-bridge-cache-directory t))))))

(defun nbt--replace (old new)
  (goto-char (point-min))
  (search-forward old)
  (replace-match new t t))

(defun nbt--push (&rest answers)
  (with-answers answers (notion-bridge-sync-push)))

(defun nbt--write-paths ()
  (mapcar (lambda (w) (concat (car w) " " (cadr w))) (fake-notion-writes)))

;;; ------------------------------------------------------------
;;; No-op and minimal writes

(ert-deftest nbt-unchanged-push-writes-nothing ()
  (nbt--with-page
    (with-answers nil (notion-bridge-sync-push))
    (should (null (fake-notion-writes)))
    (should-not (notion-bridge-sync-unpushed-changes))))

(ert-deftest nbt-edit-writes-only-that-block ()
  (nbt--with-page
    (nbt--replace "Intro paragraph" "Intro, edited")
    (should (notion-bridge-sync-unpushed-changes))
    (nbt--push t)
    (should (equal (nbt--write-paths)
                   (list (concat "PATCH /blocks/" (nbt--id 'intro)))))
    (should (equal (fake-notion-text (nbt--id 'intro)) "Intro, edited"))
    (should (equal (fake-notion-text (nbt--id 'tech))
                   "See src/pkg/engine.go and my_var_name, 2*3"))
    (should (equal (plist-get (aref (plist-get (plist-get (fake-notion-block (nbt--id 'mention)) :paragraph)
                                               :rich_text)
                                    0)
                              :type)
                   "mention"))
    (should-not (notion-bridge-sync-unpushed-changes))
    (with-answers nil (notion-bridge-sync-push))))

(ert-deftest nbt-insert-goes-after-the-right-block ()
  (nbt--with-page
    (nbt--replace "Intro paragraph" "Intro paragraph\n\nA new thought")
    (nbt--push t)
    (should (equal (length (fake-notion-writes)) 1))
    (should (equal (plist-get (nth 2 (car (fake-notion-writes))) :after) (nbt--id 'intro)))
    (should (equal (seq-take (fake-notion-texts nbt--page) 2)
                   '("Intro paragraph" "A new thought")))))

(ert-deftest nbt-nested-insert-and-edit ()
  (nbt--with-page
    (nbt--replace "  - Nested" "  - Nested, edited\n  - Another nested")
    (nbt--push t)
    (should (equal (fake-notion-texts (nbt--id 'list)) '("Nested, edited" "Another nested")))
    (should-not (notion-bridge-sync-unpushed-changes))))

(ert-deftest nbt-insert-at-top-recreates-first-block ()
  (nbt--with-page
    (nbt--replace "Intro paragraph" "Brand new opening\n\nIntro paragraph")
    (nbt--push t)
    (should (equal (seq-take (fake-notion-texts nbt--page) 2)
                   '("Brand new opening" "Intro paragraph")))
    (should-not (member (fake-notion--norm (nbt--id 'intro))
                        (mapcar #'fake-notion--norm (fake-notion-children nbt--page))))
    (should-not (notion-bridge-sync-unpushed-changes))))

(ert-deftest nbt-delete-paragraph ()
  (nbt--with-page
    (nbt--replace "\n\nLast paragraph" "")
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (nbt--push t))
      (should (equal msg "Pushed to Notion")))
    (should (equal (nbt--write-paths) (list (concat "DELETE /blocks/" (nbt--id 'last)))))
    (should (string-match-p "delete: Last paragraph" (car fake-notion--questions)))))

(ert-deftest nbt-reports-unpulled-remote-changes ()
  (nbt--with-page
    (plist-put (fake-notion-block (nbt--id 'last)) :paragraph
               (list :rich_text (vector (fake-notion-rt "Remote change"))))
    (fake-notion-para nbt--page "Remote addition")
    (nbt--replace "Intro paragraph" "My edit")
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (nbt--push t))
      (should (string-match-p "Notion has 2 other changes" msg)))))

(ert-deftest nbt-type-change-is-delete-plus-insert ()
  (nbt--with-page
    (nbt--replace "Last paragraph" "* Last paragraph")
    (nbt--push t)
    (should (equal (plist-get (fake-notion-block (car (last (fake-notion-children nbt--page)))) :type)
                   "heading_1"))
    (should-not (fake-notion-block (nbt--id 'last)))))

(ert-deftest nbt-content-after-comments-section-is-pushed ()
  (with-fake-notion
    (let ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
          (page (fake-notion-add-page "C")))
      (fake-notion-para page "Body")
      (push (list :object "comment" :id "c1" :discussion_id "d1"
                  :parent (list :type "page_id" :page_id page)
                  :created_time "2026-11-01T00:00:00.000Z" :created_by (list :name "bob")
                  :rich_text (vector (fake-notion-rt "hi")))
            fake-notion--comments)
      (with-current-buffer (notion-bridge-sync-pull page)
        (should (= (point) (point-min)))
        (should (search-forward ":NOTION_COMMENTS:" nil t))
        (goto-char (point-max))
        (insert "\n* Written after pulling\nimportant notes\n")
        (with-answers '(t) (notion-bridge-sync-push))
        (should (equal (fake-notion-texts page)
                       '("Body" "Written after pulling" "important notes")))
        (kill-buffer)))))

;;; ------------------------------------------------------------
;;; Never touching other pages

(ert-deftest nbt-removing-subpage-line-is-refused ()
  (nbt--with-page
    (nbt--replace (format "#+NOTION_BLOCK: %s child_page Subpage" (nbt--id 'sub)) "")
    (should-error (nbt--push t) :type 'user-error)
    (should (null (fake-notion-writes)))
    (should-not (fake-notion-page-trashed-p (nbt--id 'sub)))))

(ert-deftest nbt-removing-database-line-is-refused ()
  (nbt--with-page
    (nbt--replace (format "#+NOTION_BLOCK: %s child_database Tasks" (nbt--id 'db)) "")
    (should-error (nbt--push t) :type 'user-error)
    (should (null (fake-notion-writes)))))

(ert-deftest nbt-removing-image-is-a-normal-block-delete ()
  (nbt--with-page
    (goto-char (point-min))
    (search-forward (format "#+NOTION_BLOCK: %s image" (nbt--id 'image)))
    (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
    (when (looking-at "# .*\n") (replace-match ""))
    (nbt--push t)
    (should (equal (nbt--write-paths) (list (concat "DELETE /blocks/" (nbt--id 'image)))))))

(ert-deftest nbt-guard-refuses-writes-outside-scope ()
  (with-fake-notion
    (let* ((page (fake-notion-add-page "A"))
           (other (fake-notion-add-page "B"))
           (mine (fake-notion-para page "mine"))
           (theirs (fake-notion-para other "theirs"))
           (sub (fake-notion-add page "child_page" (list :title "Sub")))
           (inner (fake-notion-para sub "inside subpage"))
           (scope (notion-bridge-api-make-scope
                   page (notion-bridge-api-get-blocks-recursive page))))
      (should-error (notion-bridge-api-update-block mine '(:paragraph (:rich_text []))))
      (notion-bridge-api-with-write-scope scope
        (should (notion-bridge-api-update-block mine '(:paragraph (:rich_text []))))
        (should-error (notion-bridge-api-update-block theirs '(:paragraph (:rich_text []))))
        (should-error (notion-bridge-api-delete-block theirs))
        (should-error (notion-bridge-api-delete-block sub))
        (should-error (notion-bridge-api-delete-block inner))
        (should-error (notion-bridge-api-update-block sub '(:child_page (:title "x"))))
        (should-error (notion-bridge-api-append-blocks other (list '(:type "divider" :divider nil))))
        (should-error (notion-bridge-api--request-sync "DELETE" (format "/pages/%s" page)))
        (should-error (notion-bridge-api--request-sync "PATCH" (format "/pages/%s" page)
                                                       '(:archived t)))
        (should-error (notion-bridge-api--request-sync "PATCH" (format "/pages/%s" other)
                                                       '(:properties (:title nil))))
        (should-error (notion-bridge-api--request-sync "PATCH" (format "/blocks/%s" mine)
                                                       '(:archived t)))
        (should-error (notion-bridge-api--request-sync "POST" "/pages" '(:parent nil))))
      (should (equal (fake-notion-text theirs) "theirs"))
      (should-not (fake-notion-page-trashed-p sub)))))

(ert-deftest nbt-container-with-subpage-cannot-be-deleted ()
  (with-fake-notion
    (let* ((page (fake-notion-add-page "A"))
           (toggle (fake-notion-add page "toggle" (list :rich_text (vector (fake-notion-rt "T")))
                                    (list (list "child_page" (list :title "Deep")))))
           (scope (notion-bridge-api-make-scope
                   page (notion-bridge-api-get-blocks-recursive page))))
      (notion-bridge-api-with-write-scope scope
        (should-error (notion-bridge-api-delete-block toggle))))))

(ert-deftest nbt-comment-replies-only-to-this-pages-threads ()
  (with-fake-notion
    (let* ((page (fake-notion-add-page "A"))
           (other (fake-notion-add-page "B")))
      (fake-notion-para page "x")
      (push (list :object "comment" :id "c9" :discussion_id "elsewhere"
                  :parent (list :type "page_id" :page_id other)
                  :created_time "2026-11-01T00:00:00.000Z" :created_by (list :name "z")
                  :rich_text (vector (fake-notion-rt "other page thread")))
            fake-notion--comments)
      (with-temp-buffer
        (setq notion-bridge--page-id page)
        (should-error (notion-bridge-sync-post-comment "hi" "elsewhere") :type 'user-error)
        (notion-bridge-sync-post-comment "new thread")
        (should (equal (plist-get (plist-get (nth 2 (car (fake-notion-writes))) :parent) :page_id)
                       page))))))

;;; ------------------------------------------------------------
;;; Conflicts

(ert-deftest nbt-remote-edit-to-same-block-is-a-conflict ()
  (nbt--with-page
    (plist-put (fake-notion-block (nbt--id 'intro)) :paragraph
               (list :rich_text (vector (fake-notion-rt "Someone else's edit"))))
    (nbt--replace "Intro paragraph" "My edit")
    (should-error (nbt--push nil) :type 'user-error)
    (should (string-match-p "Changed in Notion" (car fake-notion--questions)))
    (should (null (fake-notion-writes)))
    (should (equal (fake-notion-text (nbt--id 'intro)) "Someone else's edit"))))

(ert-deftest nbt-remote-edit-elsewhere-is-preserved ()
  (nbt--with-page
    (plist-put (fake-notion-block (nbt--id 'last)) :paragraph
               (list :rich_text (vector (fake-notion-rt "Remote change"))))
    (nbt--replace "Intro paragraph" "My edit")
    (nbt--push t)
    (should (equal (fake-notion-text (nbt--id 'intro)) "My edit"))
    (should (equal (fake-notion-text (nbt--id 'last)) "Remote change"))
    (should (equal (length (fake-notion-writes)) 1))))

(ert-deftest nbt-remote-removal-of-edited-block-blocks-push ()
  (nbt--with-page
    (fake-notion--delete (nbt--id 'intro))
    (nbt--replace "Intro paragraph" "My edit")
    (should-error (nbt--push) :type 'user-error)
    (should (null (fake-notion-writes)))))

(ert-deftest nbt-conflict-check-survives-reopening-the-file ()
  (with-fake-notion
    (let* ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
           (dir (make-temp-file "nbt-dir" t))
           (file (expand-file-name "plan.org" dir)))
      (nbt--fixture)
      (with-current-buffer (find-file-noselect file)
        (notion-bridge-sync--pull-into (current-buffer) nbt--page)
        (kill-buffer))
      (with-current-buffer (find-file-noselect file)
        (should notion-bridge-mode)
        (setq fake-notion--log nil)
        (with-answers nil (notion-bridge-sync-push))
        (should (null (fake-notion-writes)))
        (nbt--replace "Intro paragraph" "Edited after reopening")
        (nbt--push t)
        (should (equal (fake-notion-text (nbt--id 'intro)) "Edited after reopening"))
        (set-buffer-modified-p nil)
        (kill-buffer))
      (delete-directory dir t))))

;;; ------------------------------------------------------------
;;; Failures and limits

(ert-deftest nbt-partial-failure-resumes-without-duplicates ()
  (nbt--with-page
    (nbt--replace "Intro paragraph" "Intro edited\n\nInserted one")
    (nbt--replace "Last paragraph" "Last edited")
    (setq fake-notion--fail-write-number (+ fake-notion--writes 2))
    (should-error (nbt--push t) :type 'user-error)
    (setq fake-notion--fail-write-number nil)
    (nbt--push t)
    (should (equal (cl-count "Inserted one" (fake-notion-texts nbt--page) :test #'equal) 1))
    (should (equal (fake-notion-text (nbt--id 'intro)) "Intro edited"))
    (should (equal (fake-notion-text (nbt--id 'last)) "Last edited"))
    (should-not (notion-bridge-sync-unpushed-changes))))

(ert-deftest nbt-long-lines-are-split-for-notion ()
  (nbt--with-page
    (nbt--replace "Last paragraph" (make-string 4500 ?x))
    (nbt--push t)
    (should (equal (fake-notion-text (nbt--id 'last)) (make-string 4500 ?x)))))

(ert-deftest nbt-many-new-blocks-are-batched ()
  (nbt--with-page
    (nbt--replace "Last paragraph"
                  (mapconcat (lambda (i) (format "Para %d %s" i (make-string 1400 ?y)))
                             (number-sequence 1 150) "\n\n"))
    (nbt--push t)
    (should (equal (length (fake-notion-children nbt--page)) (+ 9 149)))
    (should (cl-every (lambda (w) (<= (length (plist-get (nth 2 w) :children)) 100))
                      (fake-notion-writes)))))

(ert-deftest nbt-append-response-with-all-children ()
  (nbt--with-page
    (setq fake-notion--append-returns-all t)
    (nbt--replace "- First item" "- First item\n  - Added under first")
    (nbt--replace "Intro paragraph" "Intro paragraph\n\n- new list\n  - with child")
    (nbt--push t)
    (should-not (notion-bridge-sync-unpushed-changes))
    (let ((new (cl-find "new list" (fake-notion-children nbt--page)
                        :key #'fake-notion-text :test #'equal)))
      (should (equal (fake-notion-texts new) '("with child"))))))

(ert-deftest nbt-links-that-are-not-urls-are-not-sent-as-links ()
  (nbt--with-page
    (nbt--replace "Last paragraph" "see [[file:other.org][the notes]] and [[id:xyz][x]]")
    (nbt--push t)
    (should (equal (fake-notion-text (nbt--id 'last)) "see the notes and x"))))

(ert-deftest nbt-title-change-uses-the-title-property ()
  (nbt--with-page
    (nbt--replace "#+TITLE: Plan" "#+TITLE: Better plan")
    (nbt--push t)
    (should (equal (nbt--write-paths) (list (concat "PATCH /pages/" nbt--page))))
    (should (equal (notion-bridge-sync--extract-title
                    (gethash (fake-notion--norm nbt--page) fake-notion--pages))
                   "Better plan"))))

(ert-deftest nbt-new-table-and-code ()
  (nbt--with-page
    (nbt--replace "Last paragraph"
                  "| a | b |\n|---+---|\n| 1 | 2 |\n\n#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src")
    (nbt--push t)
    (let* ((kids (last (fake-notion-children nbt--page) 2)))
      (should (equal (plist-get (fake-notion-block (car kids)) :type) "table"))
      (should (equal (length (fake-notion-children (car kids))) 2))
      (should (equal (plist-get (plist-get (fake-notion-block (cadr kids)) :code) :language) "lisp")))
    (should-not (notion-bridge-sync-unpushed-changes))))

(ert-deftest nbt-empty-page-can-be-linked-and-pushed ()
  (with-fake-notion
    (let ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
          (page (fake-notion-add-page "Empty")))
      (with-temp-buffer
        (org-mode)
        (insert "#+TITLE: Empty\n\nHello\n\n- one\n- two\n")
        (setq notion-bridge--page-id page)
        (with-answers '(t) (notion-bridge-sync-push))
        (should (equal (fake-notion-texts page) '("Hello" "one" "two")))))))

(ert-deftest nbt-linked-nonempty-page-refuses-push ()
  (with-fake-notion
    (let ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
          (page (fake-notion-add-page "Full")))
      (fake-notion-para page "existing")
      (with-temp-buffer
        (org-mode)
        (insert "#+TITLE: Full\n\nreplacement\n")
        (setq notion-bridge--page-id page)
        (should-error (with-answers nil (notion-bridge-sync-push)) :type 'user-error)
        (should (null (fake-notion-writes)))))))

;;; ------------------------------------------------------------
;;; Local safety

(ert-deftest nbt-refresh-asks-before-dropping-unpushed-edits ()
  (nbt--with-page
    (nbt--replace "Intro paragraph" "Unpushed edit")
    (set-buffer-modified-p nil)
    (should-error (with-answers '(nil) (notion-bridge-sync-refresh nbt--page)) :type 'user-error)
    (goto-char (point-min))
    (should (search-forward "Unpushed edit" nil t))
    (with-answers '(t) (notion-bridge-sync-refresh nbt--page))
    (goto-char (point-min))
    (should (search-forward "Intro paragraph" nil t))))

(ert-deftest nbt-refresh-saves-file-backed-buffers ()
  (with-fake-notion
    (let* ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
           (dir (make-temp-file "nbt-dir" t))
           (file (expand-file-name "p.org" dir)))
      (nbt--fixture)
      (with-current-buffer (find-file-noselect file)
        (notion-bridge-sync--pull-into (current-buffer) nbt--page)
        (plist-put (fake-notion-block (nbt--id 'last)) :paragraph
                   (list :rich_text (vector (fake-notion-rt "Changed remotely"))))
        (with-answers nil (notion-bridge-sync-refresh nbt--page))
        (should-not (buffer-modified-p))
        (should (string-match-p "Changed remotely"
                                (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (kill-buffer))
      (delete-directory dir t))))

(ert-deftest nbt-pull-all-skips-unpushed-files-and-keeps-names ()
  (with-fake-notion
    (let* ((notion-bridge-cache-directory (make-temp-file "nbt-cache" t))
           (dir (make-temp-file "nbt-dir" t))
           (a (fake-notion-add-page "Notes"))
           (b (fake-notion-add-page "Notes")))
      (fake-notion-para a "page a")
      (fake-notion-para b "page b")
      (let ((map1 (notion-bridge-sync-pull-all dir))
            (file-a nil))
        (setq file-a (expand-file-name (gethash a map1) dir))
        (with-temp-buffer
          (insert-file-contents file-a)
          (goto-char (point-min))
          (search-forward "page a")
          (replace-match "local unpushed edit")
          (write-region (point-min) (point-max) file-a nil 'silent))
        (fake-notion-para b "more in b")
        (let ((map2 (notion-bridge-sync-pull-all dir)))
          (should (equal (gethash a map1) (gethash a map2)))
          (should (equal (gethash b map1) (gethash b map2)))
          (should (string-match-p "local unpushed edit"
                                  (with-temp-buffer (insert-file-contents file-a) (buffer-string))))
          (should (string-match-p "more in b"
                                  (with-temp-buffer
                                    (insert-file-contents (expand-file-name (gethash b map2) dir))
                                    (buffer-string))))))
      (delete-directory dir t))))

(provide 'notion-bridge-push-test)
;;; notion-bridge-push-test.el ends here
