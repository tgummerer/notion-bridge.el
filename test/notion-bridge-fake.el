;;; notion-bridge-fake.el --- In-memory Notion for tests -*- lexical-binding: t; -*-

;;; Commentary:

;; A small in-memory model of the Notion API, plugged in underneath
;; `notion-bridge-api--curl' so that everything above the transport,
;; including the write guard, runs for real.  It enforces the API rules
;; that matter for safety: deleting a child_page block trashes that
;; page, text objects are limited to 2000 characters, links must be
;; URLs, code languages must be known, and appends honour `after'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'notion-bridge-api)
(require 'notion-bridge-org)

(defvar fake-notion--pages nil)
(defvar fake-notion--blocks nil)
(defvar fake-notion--children nil)
(defvar fake-notion--comments nil)
(defvar fake-notion--log nil
  "Every request as (METHOD PATH BODY), newest first.")
(defvar fake-notion--counter 0)
(defvar fake-notion--page-size 100)
(defvar fake-notion--fail-write-number nil
  "When set to N, the Nth write request fails with a 400.")
(defvar fake-notion--writes 0)
(defvar fake-notion--append-returns-all nil
  "Non-nil to answer appends with all children, like older API versions.")

(defun fake-notion-reset ()
  (setq fake-notion--pages (make-hash-table :test #'equal)
        fake-notion--blocks (make-hash-table :test #'equal)
        fake-notion--children (make-hash-table :test #'equal)
        fake-notion--comments nil
        fake-notion--log nil
        fake-notion--counter 0
        fake-notion--page-size 100
        fake-notion--fail-write-number nil
        fake-notion--writes 0
        fake-notion--append-returns-all nil))

(defun fake-notion--new-id ()
  (format "00000000-0000-0000-0000-%012d" (cl-incf fake-notion--counter)))

(defun fake-notion--norm (id) (notion-bridge-api--norm id))

(defun fake-notion--touch (page-id)
  (let ((page (gethash (fake-notion--norm page-id) fake-notion--pages)))
    (when page
      (plist-put page :last_edited_time
                 (format "2026-11-06T10:%02d:00.000Z" (mod fake-notion--counter 60))))))

(defun fake-notion-add-page (title &optional prop-name)
  "Create a page titled TITLE; return its id."
  (let ((id (fake-notion--new-id)))
    (puthash (fake-notion--norm id)
             (list :object "page" :id id :in_trash :json-false
                   :last_edited_time "2026-11-06T09:00:00.000Z"
                   :properties
                   (list (intern (concat ":" (or prop-name "title")))
                         (list :type "title"
                               :title (vector (list :type "text" :plain_text title
                                                    :text (list :content title))))))
             fake-notion--pages)
    (puthash (fake-notion--norm id) nil fake-notion--children)
    id))

(defun fake-notion-rt (text &rest props)
  "A rich text object for TEXT; PROPS may include :bold, :href."
  (let ((rt (list :type "text" :plain_text text
                  :text (list :content text)
                  :annotations (list :bold (if (plist-get props :bold) t :json-false)
                                     :italic :json-false :strikethrough :json-false
                                     :underline :json-false :code :json-false
                                     :color "default"))))
    (when (plist-get props :href)
      (setq rt (plist-put rt :text (list :content text
                                         :link (list :url (plist-get props :href))))))
    rt))

(defun fake-notion-add (parent type data &optional children)
  "Add a block of TYPE with DATA under PARENT, then CHILDREN specs.
CHILDREN is a list of (TYPE DATA CHILDREN).  Returns the new id."
  (let ((id (fake-notion--new-id)))
    (puthash (fake-notion--norm id)
             (list :object "block" :id id :type type
                   (intern (concat ":" type)) data
                   :parent parent :archived :json-false)
             fake-notion--blocks)
    (puthash (fake-notion--norm parent)
             (append (gethash (fake-notion--norm parent) fake-notion--children) (list id))
             fake-notion--children)
    (puthash (fake-notion--norm id) nil fake-notion--children)
    (dolist (c children)
      (apply #'fake-notion-add id c))
    id))

(defun fake-notion-para (parent text)
  (fake-notion-add parent "paragraph" (list :rich_text (vector (fake-notion-rt text)))))

(defun fake-notion-children (id)
  (gethash (fake-notion--norm id) fake-notion--children))

(defun fake-notion-block (id)
  (gethash (fake-notion--norm id) fake-notion--blocks))

(defun fake-notion-text (id)
  "Plain text of block ID."
  (let* ((b (fake-notion-block id))
         (data (plist-get b (intern (concat ":" (plist-get b :type))))))
    (mapconcat (lambda (rt) (or (plist-get rt :plain_text)
                                (plist-get (plist-get rt :text) :content) ""))
               (plist-get data :rich_text) "")))

(defun fake-notion-texts (parent)
  "Plain texts of PARENT's children, in order."
  (mapcar #'fake-notion-text (fake-notion-children parent)))

(defun fake-notion-page-trashed-p (id)
  (eq (plist-get (gethash (fake-notion--norm id) fake-notion--pages) :in_trash) t))

(defun fake-notion-writes ()
  (cl-remove-if (lambda (r) (equal (car r) "GET"))
                (reverse fake-notion--log)))

(defun fake-notion--page-of (id)
  "The page that block ID belongs to."
  (let ((cur id) (guard 0))
    (while (and (gethash (fake-notion--norm cur) fake-notion--blocks) (< guard 100))
      (setq cur (plist-get (gethash (fake-notion--norm cur) fake-notion--blocks) :parent)
            guard (1+ guard)))
    cur))

(defun fake-notion--block-json (b)
  (let ((type (plist-get b :type)))
    (list :object "block" :id (plist-get b :id) :type type
          :has_children (if (fake-notion-children (plist-get b :id)) t :json-false)
          :archived :json-false
          (intern (concat ":" type)) (or (plist-get b (intern (concat ":" type)))
                                         (make-hash-table)))))

(defun fake-notion--reply (status obj)
  (list status nil (json-serialize obj :null-object :null :false-object :json-false)))

(defun fake-notion--error (status msg)
  (fake-notion--reply status (list :object "error" :status status :message msg)))

(defun fake-notion--validate-rich (rich)
  (cl-loop for rt across (or rich [])
           do (let ((content (plist-get (plist-get rt :text) :content))
                    (url (plist-get (plist-get (plist-get rt :text) :link) :url)))
                (when (and content (> (length content) 2000))
                  (throw 'invalid "text.content.length should be ≤ 2000"))
                (when (and url (not (string-match-p "\\`\\(https?\\|mailto\\):" url)))
                  (throw 'invalid (format "Invalid URL for link: %s" url)))
                (when (plist-member rt :plain_text)
                  (throw 'invalid "plain_text should not be present")))))

(defun fake-notion--validate-block (b)
  (let* ((type (plist-get b :type))
         (data (plist-get b (intern (concat ":" type)))))
    (when (plist-member b :children)
      (throw 'invalid "children at block level are not accepted here"))
    (when (and (listp data) (plist-get data :rich_text))
      (fake-notion--validate-rich (plist-get data :rich_text)))
    (when (equal type "code")
      (unless (member (plist-get data :language) notion-bridge-org--code-languages)
        (throw 'invalid (format "Invalid code language %s" (plist-get data :language)))))
    (when (equal type "table")
      (cl-loop for row across (plist-get data :children)
               do (cl-loop for cell across (plist-get (plist-get row :table_row) :cells)
                           do (fake-notion--validate-rich cell))))))

(defun fake-notion--create (parent b)
  (let* ((type (plist-get b :type))
         (data (copy-sequence (plist-get b (intern (concat ":" type)))))
         (rows (and (equal type "table") (plist-get data :children))))
    (when rows (cl-remf data :children))
    (let ((id (fake-notion--new-id)))
      (puthash (fake-notion--norm id)
               (list :object "block" :id id :type type (intern (concat ":" type)) data
                     :parent parent)
               fake-notion--blocks)
      (puthash (fake-notion--norm id) nil fake-notion--children)
      (when rows
        (cl-loop for row across rows
                 do (let ((rid (fake-notion--new-id)))
                      (puthash (fake-notion--norm rid)
                               (list :object "block" :id rid :type "table_row"
                                     :table_row (plist-get row :table_row) :parent id)
                               fake-notion--blocks)
                      (puthash (fake-notion--norm id)
                               (append (fake-notion-children id) (list rid))
                               fake-notion--children))))
      id)))

(defun fake-notion--delete (id)
  (let* ((b (fake-notion-block id))
         (parent (plist-get b :parent)))
    (when (equal (plist-get b :type) "child_page")
      (plist-put (gethash (fake-notion--norm id) fake-notion--pages) :in_trash t))
    (puthash (fake-notion--norm parent)
             (cl-remove (fake-notion--norm id) (fake-notion-children parent)
                        :key #'fake-notion--norm :test #'equal)
             fake-notion--children)
    (remhash (fake-notion--norm id) fake-notion--blocks)))

(defun fake-notion--curl (method url &optional body-string)
  "Answer one request against the in-memory Notion."
  (let* ((rest (substring url (length notion-bridge-api--base-url)))
         (path (car (split-string rest "?")))
         (query (cadr (split-string rest "?")))
         (params (and query (mapcar (lambda (kv) (split-string kv "="))
                                    (split-string query "&"))))
         (body (and body-string (json-parse-string body-string :object-type 'plist
                                                   :array-type 'array
                                                   :null-object nil
                                                   :false-object :json-false))))
    (push (list method path body) fake-notion--log)
    (unless (equal method "GET")
      (cl-incf fake-notion--writes))
    (if (and fake-notion--fail-write-number (not (equal method "GET"))
             (not (equal path "/search"))
             (= fake-notion--writes fake-notion--fail-write-number))
        (fake-notion--error 400 "Injected failure")
      (let ((result
             (catch 'invalid
               (pcase (list method path)
                 (`("GET" ,(rx "/pages/" (let id (+ (not "/"))) eos))
                  (let ((page (gethash (fake-notion--norm id) fake-notion--pages)))
                    (if page (fake-notion--reply 200 page)
                      (fake-notion--error 404 "Could not find page"))))
                 (`("PATCH" ,(rx "/pages/" (let id (+ (not "/"))) eos))
                  (let ((page (gethash (fake-notion--norm id) fake-notion--pages)))
                    (when (or (plist-member body :archived) (plist-member body :in_trash))
                      (plist-put page :in_trash t))
                    (cl-loop for (k v) on (plist-get body :properties) by #'cddr
                             do (unless (plist-get (plist-get page :properties) k)
                                  (throw 'invalid (format "%s is not a property that exists" k)))
                             (plist-put (plist-get page :properties) k
                                        (plist-put (copy-sequence v) :type "title")))
                    (fake-notion--touch id)
                    (fake-notion--reply 200 page)))
                 (`("GET" ,(rx "/blocks/" (let id (+ (not "/"))) "/children"))
                  (let* ((all (fake-notion-children id))
                         (cursor (cadr (assoc "start_cursor" params)))
                         (start (if cursor (string-to-number cursor) 0))
                         (page (seq-take (nthcdr start all) fake-notion--page-size))
                         (more (> (length all) (+ start fake-notion--page-size))))
                    (fake-notion--reply
                     200 (list :object "list"
                               :results (vconcat (mapcar (lambda (c) (fake-notion--block-json
                                                                      (fake-notion-block c)))
                                                         page))
                               :has_more (if more t :json-false)
                               :next_cursor (if more (number-to-string (+ start fake-notion--page-size))
                                              :null)))))
                 (`("PATCH" ,(rx "/blocks/" (let id (+ (not "/"))) "/children"))
                  (let* ((children (append (plist-get body :children) nil))
                         (after (plist-get body :after))
                         (siblings (fake-notion-children id)))
                    (when (> (length children) 100)
                      (throw 'invalid "children.length should be ≤ 100"))
                    (when (and after (not (member (fake-notion--norm after)
                                                  (mapcar #'fake-notion--norm siblings))))
                      (throw 'invalid "after block is not a child of the parent"))
                    (mapc #'fake-notion--validate-block children)
                    (let* ((ids (mapcar (lambda (c) (fake-notion--create id c)) children))
                           (pos (if after
                                    (1+ (cl-position (fake-notion--norm after) siblings
                                                     :key #'fake-notion--norm :test #'equal))
                                  (length siblings))))
                      (puthash (fake-notion--norm id)
                               (append (seq-take siblings pos) ids (nthcdr pos siblings))
                               fake-notion--children)
                      (fake-notion--touch (fake-notion--page-of id))
                      (fake-notion--reply
                       200 (list :object "list"
                                 :results (vconcat
                                           (mapcar (lambda (c) (fake-notion--block-json
                                                                (fake-notion-block c)))
                                                   (if fake-notion--append-returns-all
                                                       (fake-notion-children id)
                                                     ids))))))))
                 (`("PATCH" ,(rx "/blocks/" (let id (+ (not "/"))) eos))
                  (let* ((b (fake-notion-block id))
                         (key (intern (concat ":" (plist-get b :type)))))
                    (unless b (throw 'invalid "Could not find block"))
                    (when (plist-member body :archived)
                      (fake-notion--delete id))
                    (cl-loop for (k _) on body by #'cddr
                             unless (memq k (list key :archived))
                             do (throw 'invalid (format "Can't change block type to %s" k)))
                    (when (plist-get body key)
                      (fake-notion--validate-rich (plist-get (plist-get body key) :rich_text))
                      (when (equal (plist-get b :type) "code")
                        (unless (member (plist-get (plist-get body key) :language)
                                        notion-bridge-org--code-languages)
                          (throw 'invalid "Invalid code language")))
                      (plist-put b key (plist-get body key)))
                    (fake-notion--touch (fake-notion--page-of id))
                    (fake-notion--reply 200 (fake-notion--block-json b))))
                 (`("DELETE" ,(rx "/blocks/" (let id (+ (not "/"))) eos))
                  (let ((b (fake-notion-block id))
                        (page (fake-notion--page-of id)))
                    (unless b (throw 'invalid "Could not find block"))
                    (fake-notion--delete id)
                    (fake-notion--touch page)
                    (fake-notion--reply 200 (list :object "block" :id id :archived t))))
                 (`("GET" "/comments")
                  (let ((bid (cadr (assoc "block_id" params))))
                    (fake-notion--reply
                     200 (list :object "list" :has_more :json-false
                               :results (vconcat
                                         (cl-remove-if-not
                                          (lambda (c) (equal (fake-notion--norm
                                                              (or (plist-get (plist-get c :parent) :block_id)
                                                                  (plist-get (plist-get c :parent) :page_id)))
                                                             (fake-notion--norm bid)))
                                          fake-notion--comments))))))
                 (`("POST" "/comments")
                  (let ((c (list :object "comment" :id (fake-notion--new-id)
                                 :discussion_id (or (plist-get body :discussion_id) (fake-notion--new-id))
                                 :parent (or (plist-get body :parent)
                                             (let ((d (cl-find (plist-get body :discussion_id)
                                                               fake-notion--comments
                                                               :key (lambda (c) (plist-get c :discussion_id))
                                                               :test #'equal)))
                                               (plist-get d :parent)))
                                 :created_time "2026-11-06T10:00:00.000Z"
                                 :created_by (list :name "me")
                                 :rich_text (plist-get body :rich_text))))
                    (setq fake-notion--comments (append fake-notion--comments (list c)))
                    (fake-notion--reply 200 c)))
                 (`("POST" "/search")
                  (fake-notion--reply
                   200 (list :object "list" :has_more :json-false
                             :results (let (all)
                                        (maphash (lambda (_ p) (push p all)) fake-notion--pages)
                                        (vconcat (nreverse all))))))
                 (_ (fake-notion--error 400 (format "Unsupported %s %s" method path)))))))
        (if (stringp result) (fake-notion--error 400 result) result)))))

(defmacro with-fake-notion (&rest body)
  "Run BODY against a fresh in-memory Notion."
  (declare (indent 0))
  `(progn
     (fake-notion-reset)
     (let ((notion-bridge-api-token "fake-token")
           (notion-bridge-org-directory nil)
           (notion-bridge-org--page-map nil))
       (cl-letf (((symbol-function 'notion-bridge-api--curl) #'fake-notion--curl)
                 ((symbol-function 'notion-bridge-api--request-parallel)
                  (lambda (requests)
                    (mapcar (lambda (r) (notion-bridge-api--request-sync (car r) (cadr r)))
                            requests)))
                 ((symbol-function 'sleep-for) #'ignore))
         ,@body))))

(defvar fake-notion--answers nil
  "Queue of answers for yes-or-no-p / y-or-n-p; each is t or nil.")
(defvar fake-notion--questions nil "Questions asked, newest first.")

(defmacro with-answers (answers &rest body)
  "Run BODY answering prompts from ANSWERS in order; unanswered prompts fail."
  (declare (indent 1))
  `(let ((fake-notion--answers ,answers))
     (setq fake-notion--questions nil)
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (q) (push q fake-notion--questions)
                  (if fake-notion--answers (pop fake-notion--answers)
                    (error "Unexpected prompt: %s" q))))
               ((symbol-function 'y-or-n-p)
                (lambda (q) (push q fake-notion--questions)
                  (if fake-notion--answers (pop fake-notion--answers)
                    (error "Unexpected prompt: %s" q)))))
       ,@body)))

(provide 'notion-bridge-fake)
;;; notion-bridge-fake.el ends here
