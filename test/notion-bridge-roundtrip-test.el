;;; notion-bridge-roundtrip-test.el --- Conversion fidelity tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Push compares blocks by their org rendering, so a page that is
;; pulled and parsed back without edits must produce identical keys,
;; block for block.  Any difference here would make push rewrite
;; blocks nobody touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'notion-bridge-org)

(defun nbr--rt (text &rest anns)
  (list :type "text" :text (list :content text) :plain_text text
        :annotations (list :bold (if (memq :bold anns) t :json-false)
                           :italic (if (memq :italic anns) t :json-false)
                           :strikethrough :json-false
                           :underline (if (memq :underline anns) t :json-false)
                           :code (if (memq :code anns) t :json-false))))

(defun nbr--link (text url)
  (let ((r (nbr--rt text))) (plist-put r :text (list :content text :link (list :url url)))))

(defun nbr--blk (type id rich &rest more)
  (append (list :type type :id id
                (intern (concat ":" type)) (append (list :rich_text rich) (plist-get more :data)))
          (when (plist-get more :children)
            (list :children (plist-get more :children) :has_children t))))

(defconst nbr--fixtures
  (list
   (nbr--blk "paragraph" "p1" (list (nbr--rt "See src/pkg/engine.go and my_var_name, 2*3*4 and a+b+c")))
   (nbr--blk "paragraph" "p2" (list (nbr--rt "plain ") (nbr--rt "bold" :bold) (nbr--rt " and ")
                                    (nbr--rt "it" :italic) (nbr--rt "alic" :bold)))
   (nbr--blk "paragraph" "p3" (list (nbr--rt "* not a heading")))
   (nbr--blk "paragraph" "p4" (list (nbr--rt "- not a list\n1. nor this\n# nor a comment")))
   (nbr--blk "paragraph" "p5" nil)
   (nbr--blk "paragraph" "p6" (list (nbr--rt "a ") (nbr--link "link" "https://example.com/x[1]") (nbr--rt " b")))
   (nbr--blk "paragraph" "p7" (list (nbr--rt " padded bold " :bold) (nbr--rt "x")))
   (nbr--blk "paragraph" "p8" (list (list :type "mention" :plain_text "Other page"
                                          :mention (list :type "page" :page (list :id "abc-123")))))
   (nbr--blk "paragraph" "p9" (list (list :type "equation" :plain_text "a *b* + c"
                                          :equation (list :expression "a *b* + c"))))
   (nbr--blk "paragraph" "p10" (list (nbr--rt "[[not a link]] and [ ] box and :drawer:")))
   (nbr--blk "paragraph" "p11" (list (nbr--rt "DEADLINE: soon")))
   (nbr--blk "heading_1" "h1" (list (nbr--rt "TODO launch :tag:")))
   (nbr--blk "heading_2" "h2" (list (nbr--rt "Two")))
   (nbr--blk "heading_3" "h3" (list (nbr--rt "Three ") (nbr--rt "code" :code)))
   (nbr--blk "bulleted_list_item" "l1" (list (nbr--rt "item with\nsoft break"))
             :children (list (nbr--blk "bulleted_list_item" "l1a" (list (nbr--rt "nested")))
                             (nbr--blk "paragraph" "l1b" (list (nbr--rt "para in item")))))
   (nbr--blk "bulleted_list_item" "l2" (list (nbr--rt "[ ] literal brackets")))
   (nbr--blk "to_do" "t1" (list (nbr--rt "done thing")) :data (list :checked t))
   (nbr--blk "to_do" "t2" (list (nbr--rt "open thing")) :data (list :checked :json-false))
   (nbr--blk "numbered_list_item" "n1" (list (nbr--rt "first")))
   (nbr--blk "numbered_list_item" "n2" (list (nbr--rt "second"))
             :children (list (nbr--blk "code" "n2c" (list (nbr--rt "  indented()\n* star line\n#+end_src"))
                                       :data (list :language "go"))))
   (nbr--blk "code" "c1" (list (nbr--rt "(defun x ()\n  1)\n")) :data (list :language "lisp"))
   (nbr--blk "code" "c2" (list (nbr--rt "echo hi")) :data (list :language "plain text"))
   (nbr--blk "quote" "q1" (list (nbr--rt "quoted\n* starry line")))
   (list :type "divider" :id "d1" :divider nil)
   (nbr--blk "toggle" "tg1" (list (nbr--rt "Toggle title"))
             :children (list (nbr--blk "paragraph" "tg1a" (list (nbr--rt "inside")))
                             (nbr--blk "bulleted_list_item" "tg1b" (list (nbr--rt "list inside")))))
   (list :type "table" :id "tb1" :has_children t
         :table (list :table_width 2 :has_column_header t)
         :children (list (list :type "table_row" :id "r1"
                               :table_row (list :cells (list (list (nbr--rt "a|b")) (list (nbr--rt "c")))))
                         (list :type "table_row" :id "r2"
                               :table_row (list :cells (list (list (nbr--rt "1" :bold)) (list (nbr--rt "2")))))))
   (list :type "child_page" :id "cp1" :child_page (list :title "Subpage") :has_children t)
   (list :type "image" :id "im1" :image (list :type "file" :file (list :url "https://s3/x.png")))
   (nbr--blk "callout" "co1" (list (nbr--rt "Callout text")))
   (nbr--blk "heading_2" "th1" (list (nbr--rt "Toggle heading")) :data (list :is_toggleable t)
             :children (list (nbr--blk "paragraph" "th1a" (list (nbr--rt "hidden")))))
   (nbr--blk "paragraph" "pc1" (list (nbr--rt "indented parent"))
             :children (list (nbr--blk "paragraph" "pc1a" (list (nbr--rt "child")))))
   (nbr--blk "paragraph" "last" (list (nbr--rt "end")))))

(defun nbr--keys (blocks depth)
  (mapcar (lambda (b)
            (cons (notion-bridge-org--block-key b depth)
                  (and (notion-bridge-org--editable-p b depth)
                       (not (member (plist-get b :type) '("table" "notion_keep")))
                       (nbr--keys (plist-get b :children) (1+ depth)))))
          blocks))

(ert-deftest nbr-pull-then-parse-is-identity ()
  (let* ((org (notion-bridge-org--blocks-to-org nbr--fixtures))
         (parsed (notion-bridge-org--org-to-blocks org)))
    (should (equal (nbr--keys parsed 0) (nbr--keys nbr--fixtures 0)))))

(defun nbr--normalize (org)
  "ORG without placeholders, and without the edge spaces org drops."
  (replace-regexp-in-string
   "^ \\([^ ]\\)" "\\1"
   (replace-regexp-in-string "#\\+NOTION_BLOCK.*\n\\(# .*\n\\)*" "" org)))

(ert-deftest nbr-render-is-stable ()
  (let* ((org (notion-bridge-org--blocks-to-org nbr--fixtures))
         (again (notion-bridge-org--blocks-to-org (notion-bridge-org--org-to-blocks org))))
    (should (equal (nbr--normalize again) (nbr--normalize org)))))

(defun nbr--texts (org)
  (mapcar (lambda (b)
            (let ((data (plist-get b (intern (concat ":" (plist-get b :type))))))
              (cons (plist-get b :type)
                    (mapcar (lambda (rt)
                              (list (or (plist-get (plist-get rt :text) :content)
                                        (plist-get rt :plain_text))
                                    (cl-loop for k in '(:bold :italic :underline :strikethrough :code)
                                             when (eq (plist-get (plist-get rt :annotations) k) t)
                                             collect k)))
                            (plist-get data :rich_text)))))
          (notion-bridge-org--org-to-blocks org)))

(ert-deftest nbr-technical-text-is-not-emphasis ()
  (should (equal (nbr--texts "See src/pkg/engine.go and my_var_name, 2*3*4 and a+b+c\n")
                 '(("paragraph" ("See src/pkg/engine.go and my_var_name, 2*3*4 and a+b+c" nil))))))

(ert-deftest nbr-real-emphasis-still-works ()
  (should (equal (nbr--texts "a *bold* and /it/ and ~code~.\n")
                 '(("paragraph" ("a " nil) ("bold" (:bold)) (" and " nil) ("it" (:italic))
                    (" and " nil) ("code" (:code)) ("." nil))))))

(ert-deftest nbr-list-item-keeps-all-its-text ()
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "- first line of item\n  wrapped continuation\n\n  second paragraph in item\n- next\n")))
    (should (= (length blocks) 2))
    (should (equal (notion-bridge-org--plain-text
                    (plist-get (plist-get (car blocks) :bulleted_list_item) :rich_text))
                   "first line of item wrapped continuation"))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (plist-get (car blocks) :children))
                   '("paragraph")))))

(ert-deftest nbr-tables-examples-and-fixed-width-are-kept ()
  (should (equal (mapcar (lambda (b) (plist-get b :type))
                         (notion-bridge-org--org-to-blocks
                          "| a | b |\n|---+---|\n| 1 | 2 |\n\n#+begin_example\nkept\n#+end_example\n\n: fixed\n"))
                 '("table" "code" "code"))))

(ert-deftest nbr-comments-section-is-skipped-but-what-follows-is-not ()
  (should (equal (mapcar #'car (nbr--texts
                                (concat "* Intro\ntext\n* Comments :NOTION_COMMENTS:\n** Thread 1\n"
                                        "- *bob*: hi\n* New section\nnotes\n")))
                 '("heading_1" "paragraph" "heading_1" "paragraph"))))

(ert-deftest nbr-long-text-is-chunked ()
  (let ((rich (notion-bridge-org--parse-inline-markup (make-string 4500 ?a))))
    (should (equal (mapcar (lambda (r) (length (plist-get (plist-get r :text) :content)))
                           (append rich nil))
                   '(2000 2000 500)))))

(ert-deftest nbr-unsupported-blocks-become-placeholders ()
  (dolist (b (list '(:type "child_page" :id "x1" :child_page (:title "S"))
                   '(:type "child_database" :id "x2" :child_database (:title "D"))
                   '(:type "image" :id "x3" :image (:type "file" :file (:url "u")))
                   '(:type "column_list" :id "x4" :column_list nil :has_children t)
                   '(:type "synced_block" :id "x5" :synced_block nil :has_children t)))
    (should (string-prefix-p (format "#+NOTION_BLOCK: %s" (plist-get b :id))
                             (notion-bridge-org--block-to-org b)))))

(ert-deftest nbr-code-languages-map-to-notion ()
  (should (equal (notion-bridge-org--code-language-to-notion "emacs-lisp") "lisp"))
  (should (equal (notion-bridge-org--code-language-to-notion "sh") "shell"))
  (should (equal (notion-bridge-org--code-language-to-notion "go") "go"))
  (should (equal (notion-bridge-org--code-language-to-notion "weird") "plain text")))

(provide 'notion-bridge-roundtrip-test)
;;; notion-bridge-roundtrip-test.el ends here
