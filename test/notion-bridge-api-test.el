;;; notion-bridge-api-test.el --- Tests for notion-bridge-api -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notion-bridge-api)

;;; ============================================================
;;; Helpers: replace the transport
;;; ============================================================

(defmacro notion-bridge-api-test--with-responses (responses &rest body)
  "Run BODY with each request answered by the next of RESPONSES.
Each response is (STATUS PLIST [RETRY-AFTER]).  Requests made are
collected in `notion-bridge-api-test--requests'."
  (declare (indent 1))
  `(let ((queue ,responses)
         (notion-bridge-api-token "test-token-123"))
     (setq notion-bridge-api-test--requests nil)
     (cl-letf (((symbol-function 'notion-bridge-api--curl)
                (lambda (method url &optional body)
                  (push (list method url body) notion-bridge-api-test--requests)
                  (let ((r (or (pop queue) (error "No response queued"))))
                    (list (nth 0 r) (nth 2 r) (json-serialize (nth 1 r))))))
               ((symbol-function 'sleep-for)
                (lambda (s) (push s notion-bridge-api-test--sleeps))))
       ,@body)))

(defvar notion-bridge-api-test--requests nil)
(defvar notion-bridge-api-test--sleeps nil)

;;; ============================================================
;;; Responses and errors
;;; ============================================================

(ert-deftest notion-bridge-api-test-parse-success-response ()
  (notion-bridge-api-test--with-responses '((200 (:id "page-123" :object "page")))
    (let ((result (notion-bridge-api-get-page "page-123")))
      (should (equal (plist-get result :id) "page-123")))))

(ert-deftest notion-bridge-api-test-401-error ()
  (notion-bridge-api-test--with-responses '((401 (:message "Invalid token")))
    (should-error (notion-bridge-api-get-page "page-123"))))

(ert-deftest notion-bridge-api-test-404-error ()
  (notion-bridge-api-test--with-responses '((404 (:message "Not found")))
    (should-error (notion-bridge-api-get-page "page-123"))))

(ert-deftest notion-bridge-api-test-429-honours-retry-after ()
  (setq notion-bridge-api-test--sleeps nil)
  (notion-bridge-api-test--with-responses '((429 (:message "slow down") 7)
                                            (200 (:id "page-123")))
    (should (equal (plist-get (notion-bridge-api-get-page "page-123") :id) "page-123"))
    (should (equal notion-bridge-api-test--sleeps '(7)))))

(ert-deftest notion-bridge-api-test-429-gives-up ()
  (notion-bridge-api-test--with-responses
      (make-list (1+ notion-bridge-api--max-retries) '(429 (:message "slow down")))
    (should-error (notion-bridge-api-get-page "page-123"))))

(ert-deftest notion-bridge-api-test-unreachable-retries-then-errors ()
  (notion-bridge-api-test--with-responses
      (make-list (1+ notion-bridge-api--max-retries) '(0 nil))
    (let ((err (should-error (notion-bridge-api-get-page "page-123"))))
      (should (string-match-p "unreachable" (cadr err))))))

(ert-deftest notion-bridge-api-test-no-token-errors ()
  (let ((notion-bridge-api-token nil))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should-error (notion-bridge-api--curl "GET" "https://api.notion.com/v1/x")))))

(ert-deftest notion-bridge-api-test-string-token ()
  (let ((notion-bridge-api-token "secret_abc"))
    (should (equal (notion-bridge-api--get-token) "secret_abc"))))

(ert-deftest notion-bridge-api-test-function-token ()
  (let ((notion-bridge-api-token (lambda () "secret_from_fn")))
    (should (equal (notion-bridge-api--get-token) "secret_from_fn"))))

(ert-deftest notion-bridge-api-test-pagination ()
  (notion-bridge-api-test--with-responses
      '((200 (:results [(:id "b1") (:id "b2")] :has_more t :next_cursor "cur"))
        (200 (:results [(:id "b3")] :has_more :false :next_cursor nil)))
    (should (equal (mapcar (lambda (b) (plist-get b :id))
                           (notion-bridge-api-get-blocks-all "page"))
                   '("b1" "b2" "b3")))
    (should (string-match-p "start_cursor=cur" (nth 1 (car notion-bridge-api-test--requests))))))

(ert-deftest notion-bridge-api-test-recursive-skips-other-pages ()
  (notion-bridge-api-test--with-responses
      '((200 (:results [(:id "sub" :type "child_page" :has_children t)
                        (:id "tog" :type "toggle" :has_children t)]
                       :has_more :false))
        (200 (:results [(:id "in-toggle" :type "paragraph")] :has_more :false)))
    (let ((tree (notion-bridge-api-get-blocks-recursive "page")))
      (should (= (length notion-bridge-api-test--requests) 2))
      (should-not (plist-get (car tree) :children))
      (should (equal (plist-get (car (plist-get (cadr tree) :children)) :id) "in-toggle")))))

(ert-deftest notion-bridge-api-test-search-needs-no-scope ()
  (notion-bridge-api-test--with-responses '((200 (:results [] :has_more :false)))
    (should (notion-bridge-api-search "query"))))

;;; ============================================================
;;; Write scope
;;; ============================================================

(defun notion-bridge-api-test--scope ()
  (notion-bridge-api-make-scope
   "page-1"
   (list (list :id "para-1" :type "paragraph")
         (list :id "sub-1" :type "child_page" :has_children t)
         (list :id "tog-1" :type "toggle" :has_children t
               :children (list (list :id "db-1" :type "child_database")))
         (list :id "tog-2" :type "toggle" :has_children t
               :children (list (list :id "para-2" :type "paragraph"))))
   (list "disc-1")))

(ert-deftest notion-bridge-api-test-writes-need-scope ()
  (notion-bridge-api-test--with-responses '((200 (:id "para-1")))
    (should-error (notion-bridge-api-update-block "para-1" '(:paragraph (:rich_text []))))
    (should (null notion-bridge-api-test--requests))))

(ert-deftest notion-bridge-api-test-scope-allows-own-blocks ()
  (notion-bridge-api-test--with-responses '((200 (:id "para-1")) (200 (:id "tog-2")) (200 (:ok t)))
    (notion-bridge-api-with-write-scope (notion-bridge-api-test--scope)
      (should (notion-bridge-api-update-block "para-1" '(:paragraph (:rich_text []))))
      (should (notion-bridge-api-delete-block "tog-2"))
      (should (notion-bridge-api-post-comment "page-1" "hi")))))

(ert-deftest notion-bridge-api-test-scope-refusals ()
  (notion-bridge-api-test--with-responses nil
    (notion-bridge-api-with-write-scope (notion-bridge-api-test--scope)
      (dolist (attempt
               (list (lambda () (notion-bridge-api-delete-block "sub-1"))
                     (lambda () (notion-bridge-api-delete-block "tog-1"))
                     (lambda () (notion-bridge-api-delete-block "db-1"))
                     (lambda () (notion-bridge-api-delete-block "other"))
                     (lambda () (notion-bridge-api-update-block "sub-1" '(:child_page (:title "x"))))
                     (lambda () (notion-bridge-api-update-block "other" '(:paragraph (:rich_text []))))
                     (lambda () (notion-bridge-api-update-block "para-1" '(:archived t)))
                     (lambda () (notion-bridge-api-append-blocks "sub-1" (list '(:type "divider"))))
                     (lambda () (notion-bridge-api--request-sync "DELETE" "/pages/page-1"))
                     (lambda () (notion-bridge-api--request-sync "PATCH" "/pages/page-1" '(:in_trash t)))
                     (lambda () (notion-bridge-api--request-sync "PATCH" "/pages/page-2" '(:properties nil)))
                     (lambda () (notion-bridge-api--request-sync "POST" "/pages" '(:parent nil)))
                     (lambda () (notion-bridge-api-post-comment "page-2" "hi"))
                     (lambda () (notion-bridge-api-post-comment "page-1" "hi" "disc-other"))))
        (should-error (funcall attempt))))
    (should (null notion-bridge-api-test--requests))))

(ert-deftest notion-bridge-api-test-created-blocks-join-scope ()
  (notion-bridge-api-test--with-responses
      '((200 (:results [(:id "new-1" :type "paragraph")]))
        (200 (:id "new-1")))
    (notion-bridge-api-with-write-scope (notion-bridge-api-test--scope)
      (notion-bridge-api-append-blocks "page-1" (list '(:type "paragraph" :paragraph (:rich_text []))))
      (should (notion-bridge-api-update-block "new-1" '(:paragraph (:rich_text [])))))))

(ert-deftest notion-bridge-api-test-append-after-extracts-new-blocks ()
  (notion-bridge-api-test--with-responses
      '((200 (:results [(:id "a") (:id "anchor") (:id "n1") (:id "n2") (:id "z")])))
    (notion-bridge-api-with-write-scope (notion-bridge-api-make-scope "p" nil)
      (should (equal (mapcar (lambda (b) (plist-get b :id))
                             (notion-bridge-api-append-blocks
                              "p" (list '(:type "divider") '(:type "divider")) "anchor"))
                     '("n1" "n2"))))))

(ert-deftest notion-bridge-api-test-parallel-is-read-only ()
  (should-error (notion-bridge-api--request-parallel '(("DELETE" "/blocks/x")))))

;;; ============================================================
;;; Transport
;;; ============================================================

(ert-deftest notion-bridge-api-test-curl-keeps-secrets-off-the-command-line ()
  (let ((notion-bridge-api-token "secret_TOKEN")
        seen-args seen-input config-mode config-text)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (input _end _prog _delete _buffer _display &rest args)
                 (setq seen-args args seen-input input)
                 (let ((cfg (cadr (member "-K" args))))
                   (setq config-mode (file-modes cfg)
                         config-text (with-temp-buffer (insert-file-contents cfg)
                                                       (buffer-string))))
                 (insert "{\"ok\":true}\n\n200")
                 0)))
      (let ((result (notion-bridge-api--curl "PATCH" "https://api.notion.com/v1/blocks/x"
                                             "{\"secret_body\":1}")))
        (should (equal (car result) 200))
        (should-not (cl-some (lambda (a) (string-match-p "secret" a)) seen-args))
        (should (member "@-" seen-args))
        (should (string-match-p "secret_body" seen-input))
        (should (= config-mode #o600))
        (should (string-match-p "Bearer secret_TOKEN" config-text))
        (should-not (file-exists-p (cadr (member "-K" seen-args))))
        (should (member "--max-time" seen-args))))))

(ert-deftest notion-bridge-api-test-split-output ()
  (should (equal (notion-bridge-api--split-output "{\"a\":1}\n12\n429")
                 '(429 12 "{\"a\":1}")))
  (should (equal (notion-bridge-api--split-output "{\"a\":1}\n\n200")
                 '(200 nil "{\"a\":1}")))
  (should (equal (car (notion-bridge-api--split-output "")) 0)))

(provide 'notion-bridge-api-test)
;;; notion-bridge-api-test.el ends here
