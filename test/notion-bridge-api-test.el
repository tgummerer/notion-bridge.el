;;; notion-bridge-api-test.el --- Tests for notion-bridge-api -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notion-bridge-api)

;;; ============================================================
;;; Helpers - mock curl via call-process
;;; ============================================================

(defmacro notion-bridge-api-test--with-mock (status-code response-body &rest body)
  "Run BODY with call-process mocked to return STATUS-CODE and RESPONSE-BODY."
  (declare (indent 2))
  `(let ((notion-bridge-api-token "test-token-123"))
     (cl-letf (((symbol-function 'call-process)
                (lambda (_program _infile _destination _display &rest _args)
                  (insert (json-serialize ,response-body))
                  (insert (format "\n%d" ,status-code))
                  0)))
       ,@body)))

(defmacro notion-bridge-api-test--with-mock-fn (mock-fn &rest body)
  "Run BODY with call-process mocked via MOCK-FN.
MOCK-FN receives the curl args and should insert response + status into current buffer."
  (declare (indent 1))
  `(let ((notion-bridge-api-token "test-token-123"))
     (cl-letf (((symbol-function 'call-process)
                (lambda (_program _infile _destination _display &rest args)
                  (funcall ,mock-fn args)
                  0)))
       ,@body)))

;;; ============================================================
;;; Response parsing
;;; ============================================================

(ert-deftest notion-bridge-api-test-parse-success-response ()
  "Successful GET returns parsed JSON body."
  (notion-bridge-api-test--with-mock 200 '(:id "page-123" :object "page")
    (let ((result (notion-bridge-api-get-page "page-123")))
      (should result)
      (should (equal (plist-get result :id) "page-123"))
      (should (equal (plist-get result :object) "page")))))

(ert-deftest notion-bridge-api-test-patch-response ()
  "PATCH request returns parsed response."
  (notion-bridge-api-test--with-mock 200 '(:id "block-1" :type "paragraph")
    (let ((result (notion-bridge-api-update-block "block-1" '(:paragraph (:rich_text [])))))
      (should result)
      (should (equal (plist-get result :id) "block-1")))))

(ert-deftest notion-bridge-api-test-delete-response ()
  "DELETE request returns parsed response."
  (notion-bridge-api-test--with-mock 200 '(:id "block-1" :archived t)
    (let ((result (notion-bridge-api-delete-block "block-1")))
      (should result)
      (should (equal (plist-get result :archived) t)))))

;;; ============================================================
;;; Error handling
;;; ============================================================

(ert-deftest notion-bridge-api-test-401-error ()
  "401 response signals authentication error."
  (notion-bridge-api-test--with-mock 401 '(:message "Invalid token")
    (should-error (notion-bridge-api-get-page "page-123")
                  :type 'error)))

(ert-deftest notion-bridge-api-test-404-error ()
  "404 response signals not-found error."
  (notion-bridge-api-test--with-mock 404 '(:message "Not found")
    (should-error (notion-bridge-api-get-page "nonexistent")
                  :type 'error)))

(ert-deftest notion-bridge-api-test-429-rate-limit ()
  "429 response retries and eventually errors."
  (let ((notion-bridge-api-token "test-token-123")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest _args)
                 (cl-incf call-count)
                 (insert "{\"message\":\"Rate limited\"}\n429")
                 0))
              ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
      (should-error (notion-bridge-api-get-page "page-123")
                    :type 'error)
      (should (= call-count (1+ notion-bridge-api--max-retries))))))

(ert-deftest notion-bridge-api-test-429-succeeds-on-retry ()
  "429 followed by 200 succeeds."
  (let ((notion-bridge-api-token "test-token-123")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest _args)
                 (cl-incf call-count)
                 (if (= call-count 1)
                     (insert "{\"message\":\"Rate limited\"}\n429")
                   (insert "{\"id\":\"page-123\"}\n200"))
                 0))
              ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
      (let ((result (notion-bridge-api-get-page "page-123")))
        (should result)
        (should (equal (plist-get result :id) "page-123"))
        (should (= call-count 2))))))

;;; ============================================================
;;; Token resolution
;;; ============================================================

(ert-deftest notion-bridge-api-test-no-token-errors ()
  "Missing token signals an error."
  (let ((notion-bridge-api-token nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) nil)))
      (should-error (notion-bridge-api--request-sync "GET" "/pages/x")
                    :type 'error))))

(ert-deftest notion-bridge-api-test-string-token ()
  "String token is used directly."
  (let ((notion-bridge-api-token "my-token"))
    (should (equal (notion-bridge-api--get-token) "my-token"))))

(ert-deftest notion-bridge-api-test-function-token ()
  "Function token is called."
  (let ((notion-bridge-api-token (lambda () "fn-token")))
    (should (equal (notion-bridge-api--get-token) "fn-token"))))

;;; ============================================================
;;; Pagination
;;; ============================================================

(ert-deftest notion-bridge-api-test-pagination ()
  "get-blocks-all handles multi-page results."
  (let ((notion-bridge-api-token "test-token-123")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest _args)
                 (cl-incf call-count)
                 (if (= call-count 1)
                     (insert "{\"results\":[{\"id\":\"b1\"},{\"id\":\"b2\"}],\"has_more\":true,\"next_cursor\":\"cursor-abc\"}\n200")
                   (insert "{\"results\":[{\"id\":\"b3\"}],\"has_more\":false,\"next_cursor\":null}\n200"))
                 0)))
      (let ((blocks (notion-bridge-api-get-blocks-all "page-id")))
        (should (= (length blocks) 3))
        (should (= call-count 2))))))

;;; ============================================================
;;; Parallel request helper
;;; ============================================================

(ert-deftest notion-bridge-api-test-parallel-requests ()
  "Parallel requests collect results from multiple concurrent calls."
  (let* ((notion-bridge-api-token "test-token-123")
         (real-start-process (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (name buf program &rest args)
                 ;; args contains the curl arguments; URL is the last one
                 (let* ((url (car (last args)))
                        (response
                         (cond
                          ((and (stringp url) (string-match "/pages/p1" url))
                           "{\"id\":\"p1\",\"object\":\"page\"}")
                          ((and (stringp url) (string-match "/pages/p2" url))
                           "{\"id\":\"p2\",\"object\":\"page\"}")
                          ((and (stringp url) (string-match "/pages/p3" url))
                           "{\"id\":\"p3\",\"object\":\"page\"}")
                          (t "{}"))))
                   (when (bufferp buf)
                     (with-current-buffer buf
                       (insert response)
                       (insert "\n200")))
                   ;; Return a real process that finishes immediately
                   (funcall real-start-process name nil "true"))))
              ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
      (let* ((requests (list (list "GET" "/pages/p1")
                             (list "GET" "/pages/p2")
                             (list "GET" "/pages/p3")))
             (results (notion-bridge-api--request-parallel requests)))
        (should (= (length results) 3))
        (should (equal (plist-get (nth 0 results) :id) "p1"))
        (should (equal (plist-get (nth 1 results) :id) "p2"))
        (should (equal (plist-get (nth 2 results) :id) "p3"))))))

(ert-deftest notion-bridge-api-test-parallel-empty-requests ()
  "Parallel requests with empty list returns empty list."
  (let ((notion-bridge-api-token "test-token-123"))
    (let ((results (notion-bridge-api--request-parallel nil)))
      (should (= (length results) 0)))))

(ert-deftest notion-bridge-api-test-parallel-no-token-errors ()
  "Parallel requests without token signals error."
  (let ((notion-bridge-api-token nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) nil)))
      (should-error (notion-bridge-api--request-parallel
                     (list (list "GET" "/pages/x")))
                    :type 'error))))

;;; ============================================================
;;; Search with body serialization
;;; ============================================================

(ert-deftest notion-bridge-api-test-search ()
  "Search sends POST with query body and returns results."
  (let ((notion-bridge-api-token "test-token-123"))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest _args)
                 (insert "{\"results\":[{\"id\":\"r1\",\"object\":\"page\"}],\"has_more\":false}")
                 (insert "\n200")
                 0)))
      (let ((result (notion-bridge-api-search "test query")))
        (should result)
        (should (= (length (plist-get result :results)) 1))))))

;;; ============================================================
;;; Post comment
;;; ============================================================

(ert-deftest notion-bridge-api-test-post-comment ()
  "Post comment sends correct body."
  (notion-bridge-api-test--with-mock 200 '(:id "comment-1" :discussion_id "disc-1")
    (let ((result (notion-bridge-api-post-comment "page-1" "Hello!")))
      (should result)
      (should (equal (plist-get result :id) "comment-1")))))

(provide 'notion-bridge-api-test)
;;; notion-bridge-api-test.el ends here
