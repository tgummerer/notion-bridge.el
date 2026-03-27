;;; notion-bridge-cache-test.el --- Tests for notion-bridge-cache -*- lexical-binding: t; -*-

(require 'ert)
(require 'notion-bridge-cache)

;;; Helpers

(defmacro notion-bridge-cache-test--with-temp-dir (&rest body)
  "Run BODY with `notion-bridge-cache-directory' set to a temp dir."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "notion-bridge-cache-test-" t))
          (notion-bridge-cache-directory (file-name-as-directory temp-dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;;; Cache file path

(ert-deftest notion-bridge-cache-test-file-path-uses-md5 ()
  "Cache path should be md5 of absolute path plus .el extension."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/home/user/notes.org")
           (path (notion-bridge-cache--file-path org-file))
           (expected-hash (md5 (expand-file-name org-file))))
      (should (string-match-p (regexp-quote expected-hash) path))
      (should (string-suffix-p ".el" path)))))

(ert-deftest notion-bridge-cache-test-file-path-deterministic ()
  "Same org file should always produce the same cache path."
  (notion-bridge-cache-test--with-temp-dir
    (let ((org-file "/tmp/test.org"))
      (should (equal (notion-bridge-cache--file-path org-file)
                     (notion-bridge-cache--file-path org-file))))))

(ert-deftest notion-bridge-cache-test-file-path-different-for-different-files ()
  "Different org files should produce different cache paths."
  (notion-bridge-cache-test--with-temp-dir
    (should-not (equal (notion-bridge-cache--file-path "/a.org")
                       (notion-bridge-cache--file-path "/b.org")))))

;;; Save and load roundtrip

(ert-deftest notion-bridge-cache-test-save-load-roundtrip ()
  "Saving and loading a cache should produce the same data."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/roundtrip-test.org")
           (cache (list :page-id "abc-123"
                        :last-synced "2026-01-01T00:00:00+0000"
                        :last-edited "2026-01-01T00:00:00+0000"
                        :blocks (list (list :org-hash "hash1"
                                            :notion-id "block-1"
                                            :type "paragraph"
                                            :position 0)
                                      (list :org-hash "hash2"
                                            :notion-id "block-2"
                                            :type "heading_1"
                                            :position 1)))))
      (notion-bridge-cache-save org-file cache)
      (let ((loaded (notion-bridge-cache-load org-file)))
        (should loaded)
        (should (equal (plist-get loaded :page-id) "abc-123"))
        (should (equal (length (plist-get loaded :blocks)) 2))
        (should (equal (plist-get (car (plist-get loaded :blocks)) :notion-id)
                       "block-1"))))))

(ert-deftest notion-bridge-cache-test-save-creates-directories ()
  "Save should create cache directory if it doesn't exist."
  (let* ((temp-dir (make-temp-file "notion-bridge-cache-test-" t))
         (notion-bridge-cache-directory
          (file-name-as-directory (expand-file-name "sub/deep/" temp-dir)))
         (org-file "/tmp/dirtest.org"))
    (unwind-protect
        (progn
          (should-not (file-exists-p notion-bridge-cache-directory))
          (notion-bridge-cache-save org-file (list :page-id "test"))
          (should (file-exists-p notion-bridge-cache-directory)))
      (delete-directory temp-dir t))))

;;; Load missing and corrupt

(ert-deftest notion-bridge-cache-test-load-missing-returns-nil ()
  "Loading a cache for a file with no cache should return nil."
  (notion-bridge-cache-test--with-temp-dir
    (should-not (notion-bridge-cache-load "/nonexistent/file.org"))))

(ert-deftest notion-bridge-cache-test-load-corrupt-returns-nil ()
  "Loading a corrupt cache file should return nil, not signal an error."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/corrupt-test.org")
           (cache-file (notion-bridge-cache--file-path org-file)))
      (make-directory (file-name-directory cache-file) t)
      (with-temp-file cache-file
        (insert "#invalid!@#$%^&*"))
      (should-not (notion-bridge-cache-load org-file)))))

;;; Block ID lookup

(ert-deftest notion-bridge-cache-test-get-block-id ()
  "Should return the notion-id for a valid index."
  (let ((cache (list :blocks (list (list :notion-id "id-0" :position 0)
                                   (list :notion-id "id-1" :position 1)
                                   (list :notion-id "id-2" :position 2)))))
    (should (equal (notion-bridge-cache-get-block-id cache 0) "id-0"))
    (should (equal (notion-bridge-cache-get-block-id cache 1) "id-1"))
    (should (equal (notion-bridge-cache-get-block-id cache 2) "id-2"))))

(ert-deftest notion-bridge-cache-test-get-block-id-out-of-range ()
  "Should return nil for an out-of-range index."
  (let ((cache (list :blocks (list (list :notion-id "id-0" :position 0)))))
    (should-not (notion-bridge-cache-get-block-id cache 5))))

(ert-deftest notion-bridge-cache-test-get-block-id-empty-blocks ()
  "Should return nil when blocks list is empty or missing."
  (should-not (notion-bridge-cache-get-block-id (list :blocks nil) 0))
  (should-not (notion-bridge-cache-get-block-id nil 0)))

;;; Block update

(ert-deftest notion-bridge-cache-test-update-block ()
  "Should update existing block mapping."
  (let* ((cache (list :blocks (list (list :notion-id "old-id"
                                          :org-hash "old-hash"
                                          :type "paragraph"
                                          :position 0))))
         (updated (notion-bridge-cache-update-block cache 0 "new-id" "new-hash" "heading_1")))
    (should (equal (plist-get (car (plist-get updated :blocks)) :notion-id) "new-id"))
    (should (equal (plist-get (car (plist-get updated :blocks)) :org-hash) "new-hash"))
    (should (equal (plist-get (car (plist-get updated :blocks)) :type) "heading_1"))))

(ert-deftest notion-bridge-cache-test-update-block-extends-list ()
  "Should extend blocks list when index exceeds current length."
  (let* ((cache (list :blocks nil))
         (updated (notion-bridge-cache-update-block cache 2 "id-2" "hash-2" "paragraph")))
    (should (= (length (plist-get updated :blocks)) 3))
    (should (equal (plist-get (nth 2 (plist-get updated :blocks)) :notion-id) "id-2"))))

;;; Page info

(ert-deftest notion-bridge-cache-test-set-page-info ()
  "Should set page-id, last-edited, and last-synced."
  (let* ((cache (list :blocks nil))
         (updated (notion-bridge-cache-set-page-info cache "page-abc" "2026-01-15T10:00:00Z")))
    (should (equal (plist-get updated :page-id) "page-abc"))
    (should (equal (plist-get updated :last-edited) "2026-01-15T10:00:00Z"))
    (should (stringp (plist-get updated :last-synced)))))

;;; Compute hash

(ert-deftest notion-bridge-cache-test-compute-hash ()
  "Should compute a sha256 hash string."
  (let ((hash (notion-bridge-cache-compute-hash "hello world")))
    (should (stringp hash))
    (should (= (length hash) 64))  ;; sha256 hex = 64 chars
    ;; Known sha256 of "hello world"
    (should (equal hash "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"))))

(ert-deftest notion-bridge-cache-test-compute-hash-deterministic ()
  "Same input should produce the same hash."
  (should (equal (notion-bridge-cache-compute-hash "test")
                 (notion-bridge-cache-compute-hash "test"))))

(ert-deftest notion-bridge-cache-test-compute-hash-different-inputs ()
  "Different inputs should produce different hashes."
  (should-not (equal (notion-bridge-cache-compute-hash "foo")
                     (notion-bridge-cache-compute-hash "bar"))))

;;; Comment storage

(ert-deftest notion-bridge-cache-test-set-get-comments ()
  "Comments should round-trip through cache."
  (let* ((comments (list (list :parent-type "block_id"
                               :parent-id "block-1"
                               :discussion-id "disc-1"
                               :author "Alice"
                               :date "2026-03-25"
                               :text "Looks good!")
                         (list :parent-type "page_id"
                               :parent-id "page-1"
                               :discussion-id "disc-2"
                               :author "Bob"
                               :date "2026-03-25"
                               :text "Page-level note")))
         (cache (list :blocks nil))
         (updated (notion-bridge-cache-set-comments cache comments)))
    (should (equal (length (notion-bridge-cache-get-comments updated)) 2))
    (should (equal (plist-get (car (notion-bridge-cache-get-comments updated)) :author) "Alice"))))

(ert-deftest notion-bridge-cache-test-comments-persist ()
  "Comments should persist through save/load cycle."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/comment-persist-test.org")
           (comments (list (list :parent-type "block_id"
                                 :parent-id "b1"
                                 :discussion-id "d1"
                                 :author "Alice"
                                 :date "2026-03-25"
                                 :text "Test comment")))
           (cache (list :page-id "p1" :blocks nil)))
      (setq cache (notion-bridge-cache-set-comments cache comments))
      (notion-bridge-cache-save org-file cache)
      (let* ((loaded (notion-bridge-cache-load org-file))
             (loaded-comments (notion-bridge-cache-get-comments loaded)))
        (should loaded-comments)
        (should (= (length loaded-comments) 1))
        (should (equal (plist-get (car loaded-comments) :text) "Test comment"))))))

;;; Comment storage/retrieval roundtrip through save/load

(ert-deftest notion-bridge-cache-test-comments-roundtrip-multiple ()
  "Multiple comments round-trip through save/load preserving all fields."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/multi-comment-test.org")
           (comments (list (list :parent-type "block_id"
                                 :parent-id "block-1"
                                 :discussion-id "disc-1"
                                 :author "Alice"
                                 :date "2026-03-01"
                                 :text "First comment")
                           (list :parent-type "page_id"
                                 :parent-id "page-1"
                                 :discussion-id "disc-2"
                                 :author "Bob"
                                 :date "2026-03-02"
                                 :text "Second comment")
                           (list :parent-type "block_id"
                                 :parent-id "block-2"
                                 :discussion-id "disc-3"
                                 :author "Charlie"
                                 :date "2026-03-03"
                                 :text "Third comment")))
           (cache (list :page-id "p1" :blocks nil)))
      (setq cache (notion-bridge-cache-set-comments cache comments))
      (notion-bridge-cache-save org-file cache)
      (let* ((loaded (notion-bridge-cache-load org-file))
             (loaded-comments (notion-bridge-cache-get-comments loaded)))
        (should (= (length loaded-comments) 3))
        ;; Check each comment preserved all fields
        (let ((c2 (nth 1 loaded-comments)))
          (should (equal (plist-get c2 :parent-type) "page_id"))
          (should (equal (plist-get c2 :parent-id) "page-1"))
          (should (equal (plist-get c2 :discussion-id) "disc-2"))
          (should (equal (plist-get c2 :author) "Bob"))
          (should (equal (plist-get c2 :date) "2026-03-02"))
          (should (equal (plist-get c2 :text) "Second comment")))))))

;;; Cache with both blocks and comments together

(ert-deftest notion-bridge-cache-test-blocks-and-comments-together ()
  "Cache with both blocks and comments persists and loads correctly."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/blocks-comments-test.org")
           (cache (list :blocks nil)))
      ;; Set page info
      (setq cache (notion-bridge-cache-set-page-info cache "page-99" "2026-03-15T00:00:00Z"))
      ;; Add blocks
      (setq cache (notion-bridge-cache-update-block cache 0 "blk-1" "hash-a" "paragraph"))
      (setq cache (notion-bridge-cache-update-block cache 1 "blk-2" "hash-b" "heading_1"))
      ;; Add comments
      (setq cache (notion-bridge-cache-set-comments
                   cache
                   (list (list :parent-type "block_id"
                               :parent-id "blk-1"
                               :discussion-id "d1"
                               :author "Tester"
                               :date "2026-03-15"
                               :text "A test comment"))))
      ;; Save and load
      (notion-bridge-cache-save org-file cache)
      (let ((loaded (notion-bridge-cache-load org-file)))
        ;; Verify page info
        (should (equal (plist-get loaded :page-id) "page-99"))
        ;; Verify blocks
        (let ((blocks (plist-get loaded :blocks)))
          (should (= (length blocks) 2))
          (should (equal (plist-get (nth 0 blocks) :notion-id) "blk-1"))
          (should (equal (plist-get (nth 1 blocks) :notion-id) "blk-2")))
        ;; Verify comments
        (let ((comments (notion-bridge-cache-get-comments loaded)))
          (should (= (length comments) 1))
          (should (equal (plist-get (car comments) :text) "A test comment")))))))

(ert-deftest notion-bridge-cache-test-empty-comments ()
  "Empty comments list persists correctly."
  (notion-bridge-cache-test--with-temp-dir
    (let* ((org-file "/tmp/empty-comments-test.org")
           (cache (list :page-id "p1" :blocks nil)))
      (setq cache (notion-bridge-cache-set-comments cache nil))
      (notion-bridge-cache-save org-file cache)
      (let ((loaded (notion-bridge-cache-load org-file)))
        (should-not (notion-bridge-cache-get-comments loaded))))))

(provide 'notion-bridge-cache-test)
;;; notion-bridge-cache-test.el ends here
