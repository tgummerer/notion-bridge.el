;;; notion-bridge-sync-test.el --- Tests for notion-bridge-sync -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notion-bridge-sync)

;;; ============================================================
;;; Title extraction
;;; ============================================================

(ert-deftest notion-bridge-sync-test-extract-title ()
  "Extract title from a Notion page plist with :title property."
  (let ((page '(:properties
                (:title (:type "title"
                         :title ((:type "text"
                                  :text (:content "My Page")
                                  :plain_text "My Page")))))))
    (should (equal (notion-bridge-sync--extract-title page) "My Page"))))

(ert-deftest notion-bridge-sync-test-extract-title-dynamic ()
  "Extract title from any property with type=title (e.g. custom name)."
  (let ((page (list :properties
                    (list :Name (list :type "select" :select nil)
                          :DocTitle (list :type "title"
                                         :title (list (list :type "text"
                                                            :text (list :content "Strategy Doc")
                                                            :plain_text "Strategy Doc")))))))
    (should (equal (notion-bridge-sync--extract-title page) "Strategy Doc"))))

(ert-deftest notion-bridge-sync-test-extract-title-missing ()
  "Return nil when no title property."
  (let ((page '(:properties (:status (:type "select" :select (:name "Done"))))))
    (should-not (notion-bridge-sync--extract-title page))))

;;; ============================================================
;;; Build cache from blocks
;;; ============================================================

;;; ============================================================
;;; Keyword update
;;; ============================================================

(ert-deftest notion-bridge-sync-test-update-keyword ()
  "update-keyword inserts or replaces file-level keywords."
  (with-temp-buffer
    (insert "#+TITLE: Test\n#+NOTION_PAGE_ID: abc\n\nBody text\n")
    (notion-bridge-sync--update-keyword "NOTION_LAST_SYNCED" "2026-03-01")
    (goto-char (point-min))
    (should (search-forward "#+NOTION_LAST_SYNCED: 2026-03-01" nil t))))

(ert-deftest notion-bridge-sync-test-update-keyword-replace ()
  "update-keyword replaces existing keyword value."
  (with-temp-buffer
    (insert "#+TITLE: Test\n#+NOTION_LAST_SYNCED: old-value\n\nBody\n")
    (notion-bridge-sync--update-keyword "NOTION_LAST_SYNCED" "new-value")
    (goto-char (point-min))
    (should (search-forward "#+NOTION_LAST_SYNCED: new-value" nil t))
    (goto-char (point-min))
    (should-not (search-forward "old-value" nil t))))

;;; ============================================================
;;; Comment stripping
;;; ============================================================

;;; ============================================================
;;; Comment serialization
;;; ============================================================

(ert-deftest notion-bridge-sync-test-serialize-comments ()
  "Serialize raw API comments into cache-friendly format."
  (let* ((raw-comments (list
                        (list :parent (list :type "block_id" :block_id "b1")
                              :discussion_id "d1"
                              :created_by (list :name "Alice")
                              :created_time "2026-03-25T10:00:00Z"
                              :rich_text (list (list :plain_text "Great work!")))
                        (list :parent (list :type "page_id" :page_id "p1")
                              :discussion_id "d2"
                              :created_by (list :name "Bob")
                              :created_time "2026-03-25T11:00:00Z"
                              :rich_text (list (list :plain_text "Page note")))))
         (serialized (notion-bridge-sync--serialize-comments raw-comments)))
    (should (= (length serialized) 2))
    (let ((first (car serialized)))
      (should (equal (plist-get first :parent-type) "block_id"))
      (should (equal (plist-get first :parent-id) "b1"))
      (should (equal (plist-get first :author) "Alice"))
      (should (equal (plist-get first :text) "Great work!"))
      (should (equal (plist-get first :date) "2026-03-25")))
    (let ((second (cadr serialized)))
      (should (equal (plist-get second :parent-type) "page_id"))
      (should (equal (plist-get second :author) "Bob")))))

;;; ============================================================
;;; Slugify
;;; ============================================================

(ert-deftest notion-bridge-sync-test-slugify ()
  "Slugify converts titles to filesystem-safe names."
  (should (equal (notion-bridge-sync--slugify "My Page Title") "my-page-title"))
  (should (equal (notion-bridge-sync--slugify "Hello World!") "hello-world"))
  (should (equal (notion-bridge-sync--slugify nil) "untitled"))
  (should (equal (notion-bridge-sync--slugify "") "untitled")))

;;; ============================================================
;;; Conflict detection
;;; ============================================================

;;; ============================================================
;;; Comment serialization with mixed parent types
;;; ============================================================

(ert-deftest notion-bridge-sync-test-serialize-mixed-comments ()
  "Serialize a mix of page-level and block-level comments."
  (let* ((raw (list
               (list :parent (list :type "page_id" :page_id "p1")
                     :discussion_id "d1"
                     :created_by (list :name "Alice")
                     :created_time "2026-03-20T09:00:00Z"
                     :rich_text (list (list :plain_text "Page comment")))
               (list :parent (list :type "block_id" :block_id "b1")
                     :discussion_id "d2"
                     :created_by (list :name "Bob")
                     :created_time "2026-03-21T14:30:00Z"
                     :rich_text (list (list :plain_text "Block comment")))
               (list :parent (list :type "block_id" :block_id "b2")
                     :discussion_id "d3"
                     :created_by nil
                     :created_time nil
                     :rich_text (list (list :plain_text "Anonymous")))))
         (serialized (notion-bridge-sync--serialize-comments raw)))
    (should (= (length serialized) 3))
    ;; Page comment
    (let ((c1 (nth 0 serialized)))
      (should (equal (plist-get c1 :parent-type) "page_id"))
      (should (equal (plist-get c1 :parent-id) "p1"))
      (should (equal (plist-get c1 :text) "Page comment")))
    ;; Block comment
    (let ((c2 (nth 1 serialized)))
      (should (equal (plist-get c2 :parent-type) "block_id"))
      (should (equal (plist-get c2 :parent-id) "b1"))
      (should (equal (plist-get c2 :date) "2026-03-21")))
    ;; Missing created_by falls back to "Unknown"
    (let ((c3 (nth 2 serialized)))
      (should (equal (plist-get c3 :author) "Unknown"))
      (should (equal (plist-get c3 :date) "")))))

(ert-deftest notion-bridge-sync-test-serialize-multi-segment-rich-text ()
  "Comment with multiple rich_text segments concatenates them."
  (let* ((raw (list
               (list :parent (list :type "page_id" :page_id "p1")
                     :discussion_id "d1"
                     :created_by (list :name "Eve")
                     :created_time "2026-01-01T00:00:00Z"
                     :rich_text (list (list :plain_text "Hello ")
                                      (list :plain_text "world")))))
         (serialized (notion-bridge-sync--serialize-comments raw)))
    (should (equal (plist-get (car serialized) :text) "Hello world"))))

;;; ============================================================
;;; Strip comments with no comments section (identity)
;;; ============================================================

;;; ============================================================
;;; Slugify edge cases
;;; ============================================================

(ert-deftest notion-bridge-sync-test-slugify-unicode ()
  "Unicode characters are replaced with hyphens."
  (should (equal (notion-bridge-sync--slugify "Cafe\u0301 au lait") "cafe-au-lait")))

(ert-deftest notion-bridge-sync-test-slugify-special-chars ()
  "Special characters are stripped."
  (should (equal (notion-bridge-sync--slugify "Hello & World! @#$%") "hello-world")))

(ert-deftest notion-bridge-sync-test-slugify-leading-trailing-hyphens ()
  "Leading/trailing hyphens are stripped."
  (should (equal (notion-bridge-sync--slugify "---title---") "title")))

(ert-deftest notion-bridge-sync-test-slugify-all-special ()
  "All-special-char title becomes 'untitled'."
  (should (equal (notion-bridge-sync--slugify "!@#$%^&*()") "untitled")))

(ert-deftest notion-bridge-sync-test-slugify-spaces-and-mixed ()
  "Spaces and mixed case are handled."
  (should (equal (notion-bridge-sync--slugify "  My   Document  ") "my-document")))

(ert-deftest notion-bridge-sync-test-slugify-numbers ()
  "Numbers are preserved."
  (should (equal (notion-bridge-sync--slugify "Chapter 3: Results") "chapter-3-results")))

;;; ============================================================
;;; Unique filename dedup
;;; ============================================================

(ert-deftest notion-bridge-sync-test-unique-filename-no-conflict ()
  "No conflict returns slug.org directly."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (notion-bridge-sync--unique-filename "my-page" "/tmp/" used)
                   "my-page.org"))
    ;; Should be registered in used table
    (should (gethash "my-page.org" used))))

(ert-deftest notion-bridge-sync-test-unique-filename-dedup ()
  "Duplicate slug gets incrementing suffix."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (notion-bridge-sync--unique-filename "page" "/tmp/" used)
                   "page.org"))
    (should (equal (notion-bridge-sync--unique-filename "page" "/tmp/" used)
                   "page-1.org"))
    (should (equal (notion-bridge-sync--unique-filename "page" "/tmp/" used)
                   "page-2.org"))))

(ert-deftest notion-bridge-sync-test-unique-filename-different-slugs ()
  "Different slugs don't conflict."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (notion-bridge-sync--unique-filename "alpha" "/tmp/" used)
                   "alpha.org"))
    (should (equal (notion-bridge-sync--unique-filename "beta" "/tmp/" used)
                   "beta.org"))))

(provide 'notion-bridge-sync-test)
;;; notion-bridge-sync-test.el ends here
