;;; notion-bridge-org-test.el --- Tests for notion-bridge-org -*- lexical-binding: t; -*-

(require 'ert)
(require 'notion-bridge-org)

;;; ============================================================
;;; Helpers for building Notion block plists
;;; ============================================================

(defun notion-bridge-org-test--make-rich-text (content &optional annotations)
  "Build a rich_text element with CONTENT and optional ANNOTATIONS plist."
  (let ((ann (list :bold (if (plist-get annotations :bold) t :json-false)
                   :italic (if (plist-get annotations :italic) t :json-false)
                   :strikethrough (if (plist-get annotations :strikethrough) t :json-false)
                   :underline (if (plist-get annotations :underline) t :json-false)
                   :code (if (plist-get annotations :code) t :json-false)))
        (text-obj (list :content content)))
    (when (plist-get annotations :href)
      (setq text-obj (plist-put text-obj :link (list :url (plist-get annotations :href)))))
    (list :type "text" :text text-obj :annotations ann)))

(defun notion-bridge-org-test--make-block (type rich-text-list &optional extra)
  "Build a Notion block of TYPE with RICH-TEXT-LIST.
EXTRA is additional plist entries to merge into the type data."
  (let ((type-data (list :rich_text rich-text-list)))
    (when extra
      (cl-loop for (k v) on extra by #'cddr
               do (setq type-data (plist-put type-data k v))))
    (list :type type (intern (concat ":" type)) type-data)))

;;; ============================================================
;;; Notion -> Org: Block type conversion
;;; ============================================================

(ert-deftest notion-bridge-org-test-paragraph ()
  "Convert paragraph block to org text."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Hello world")))
         (block (notion-bridge-org-test--make-block "paragraph" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "Hello world"))))

(ert-deftest notion-bridge-org-test-heading-1 ()
  "Convert heading_1 to org level 1 heading."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Top Heading")))
         (block (notion-bridge-org-test--make-block "heading_1" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "* Top Heading"))))

(ert-deftest notion-bridge-org-test-heading-2 ()
  "Convert heading_2 to org level 2 heading."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Sub Heading")))
         (block (notion-bridge-org-test--make-block "heading_2" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "** Sub Heading"))))

(ert-deftest notion-bridge-org-test-heading-3 ()
  "Convert heading_3 to org level 3 heading."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Minor Heading")))
         (block (notion-bridge-org-test--make-block "heading_3" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "*** Minor Heading"))))

(ert-deftest notion-bridge-org-test-bulleted-list ()
  "Convert bulleted_list_item to org unordered list."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "List item")))
         (block (notion-bridge-org-test--make-block "bulleted_list_item" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "- List item"))))

(ert-deftest notion-bridge-org-test-numbered-list ()
  "Convert numbered_list_item to org ordered list."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Step one")))
         (block (notion-bridge-org-test--make-block "numbered_list_item" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "1. Step one"))))

(ert-deftest notion-bridge-org-test-todo-unchecked ()
  "Convert unchecked to_do to org checkbox."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Buy milk")))
         (block (notion-bridge-org-test--make-block "to_do" rt
                  (list :checked :json-false)))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "- [ ] Buy milk"))))

(ert-deftest notion-bridge-org-test-todo-checked ()
  "Convert checked to_do to org checked checkbox."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Done task")))
         (block (notion-bridge-org-test--make-block "to_do" rt
                  (list :checked t)))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "- [X] Done task"))))

(ert-deftest notion-bridge-org-test-code-block ()
  "Convert code block to org src block."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "print(\"hi\")")))
         (block (notion-bridge-org-test--make-block "code" rt
                  (list :language "python")))
         (result (notion-bridge-org--block-to-org block)))
    (should (string-match-p "#\\+begin_src python" result))
    (should (string-match-p "print(\"hi\")" result))
    (should (string-match-p "#\\+end_src" result))))

(ert-deftest notion-bridge-org-test-quote-block ()
  "Convert quote block to org quote."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "A wise saying")))
         (block (notion-bridge-org-test--make-block "quote" rt))
         (result (notion-bridge-org--block-to-org block)))
    (should (string-match-p "#\\+begin_quote" result))
    (should (string-match-p "A wise saying" result))
    (should (string-match-p "#\\+end_quote" result))))

(ert-deftest notion-bridge-org-test-divider ()
  "Convert divider to org horizontal rule."
  (let* ((block (list :type "divider" :divider nil))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "-----"))))

;;; ============================================================
;;; Notion -> Org: Rich text annotations
;;; ============================================================

(ert-deftest notion-bridge-org-test-rich-text-bold ()
  "Bold annotation wraps text in *."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "bold text" '(:bold t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "*bold text*"))))

(ert-deftest notion-bridge-org-test-rich-text-italic ()
  "Italic annotation wraps text in /."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "italic text" '(:italic t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "/italic text/"))))

(ert-deftest notion-bridge-org-test-rich-text-strikethrough ()
  "Strikethrough annotation wraps text in +."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "struck" '(:strikethrough t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "+struck+"))))

(ert-deftest notion-bridge-org-test-rich-text-code ()
  "Code annotation wraps text in ~."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "code" '(:code t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "~code~"))))

(ert-deftest notion-bridge-org-test-rich-text-underline ()
  "Underline annotation wraps text in _."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "underlined" '(:underline t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "_underlined_"))))

(ert-deftest notion-bridge-org-test-rich-text-link ()
  "Link wraps text in org link syntax."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "click" '(:href "https://example.com"))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "[[https://example.com][click]]"))))

(ert-deftest notion-bridge-org-test-rich-text-combined ()
  "Multiple annotations combine correctly."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "both" '(:bold t :italic t))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    ;; Bold wraps italic: */.../...*
    (should (string-match-p "\\*" result))
    (should (string-match-p "/" result))
    (should (string-match-p "both" result))))

(ert-deftest notion-bridge-org-test-rich-text-multiple-segments ()
  "Multiple rich text segments concatenate correctly."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "plain ")
                   (notion-bridge-org-test--make-rich-text "bold" '(:bold t))
                   (notion-bridge-org-test--make-rich-text " end")))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "plain *bold* end"))))

;;; ============================================================
;;; Org -> Notion: Element conversion
;;; ============================================================

(ert-deftest notion-bridge-org-test-org-to-blocks-headline ()
  "Org headline converts to Notion heading block."
  (let ((blocks (notion-bridge-org--org-to-blocks "* My Heading\n")))
    (should (>= (length blocks) 1))
    (let ((block (car blocks)))
      (should (equal (plist-get block :type) "heading_1")))))

(ert-deftest notion-bridge-org-test-org-to-blocks-headline-levels ()
  "Different headline levels map to heading_1/2/3."
  (let ((blocks (notion-bridge-org--org-to-blocks "* H1\n** H2\n*** H3\n")))
    (should (cl-some (lambda (b) (equal (plist-get b :type) "heading_1")) blocks))
    (should (cl-some (lambda (b) (equal (plist-get b :type) "heading_2")) blocks))
    (should (cl-some (lambda (b) (equal (plist-get b :type) "heading_3")) blocks))))

(ert-deftest notion-bridge-org-test-org-to-blocks-paragraph ()
  "Org paragraph text converts to Notion paragraph block."
  (let ((blocks (notion-bridge-org--org-to-blocks "Some plain text.\n")))
    (should (>= (length blocks) 1))
    (should (cl-some (lambda (b) (equal (plist-get b :type) "paragraph")) blocks))))

(ert-deftest notion-bridge-org-test-org-to-blocks-unordered-list ()
  "Org unordered list converts to bulleted_list_item blocks."
  (let ((blocks (notion-bridge-org--org-to-blocks "- Item A\n- Item B\n")))
    (should (>= (length blocks) 2))
    (should (cl-every (lambda (b) (equal (plist-get b :type) "bulleted_list_item")) blocks))))

(ert-deftest notion-bridge-org-test-org-to-blocks-ordered-list ()
  "Org ordered list converts to numbered_list_item blocks."
  (let ((blocks (notion-bridge-org--org-to-blocks "1. First\n2. Second\n")))
    (should (>= (length blocks) 2))
    (should (cl-every (lambda (b) (equal (plist-get b :type) "numbered_list_item")) blocks))))

(ert-deftest notion-bridge-org-test-org-to-blocks-checkbox ()
  "Org checkbox list converts to to_do blocks."
  (let ((blocks (notion-bridge-org--org-to-blocks "- [ ] Unchecked\n- [X] Checked\n")))
    (should (>= (length blocks) 2))
    (should (cl-every (lambda (b) (equal (plist-get b :type) "to_do")) blocks))))

(ert-deftest notion-bridge-org-test-org-to-blocks-src-block ()
  "Org src block converts to Notion code block."
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "#+begin_src python\nprint(1)\n#+end_src\n")))
    (should (>= (length blocks) 1))
    (let ((code-block (cl-find "code" blocks :key (lambda (b) (plist-get b :type)) :test #'equal)))
      (should code-block)
      (should (equal (plist-get (plist-get code-block :code) :language) "python")))))

(ert-deftest notion-bridge-org-test-org-to-blocks-quote ()
  "Org quote block converts to Notion quote block."
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "#+begin_quote\nWise words\n#+end_quote\n")))
    (should (>= (length blocks) 1))
    (should (cl-some (lambda (b) (equal (plist-get b :type) "quote")) blocks))))

;;; ============================================================
;;; Inline markup parsing (org -> Notion rich_text)
;;; ============================================================

(ert-deftest notion-bridge-org-test-parse-plain-text ()
  "Plain text produces a single rich_text element."
  (let ((rt (notion-bridge-org--parse-inline-markup "hello")))
    (should (>= (length rt) 1))
    (should (equal (plist-get (plist-get (aref rt 0) :text) :content) "hello"))))

(ert-deftest notion-bridge-org-test-parse-bold ()
  "Bold markup *text* produces bold annotation."
  (let ((rt (notion-bridge-org--parse-inline-markup "*bold*")))
    (should (>= (length rt) 1))
    (let ((elem (aref rt 0)))
      (should (equal (plist-get (plist-get elem :text) :content) "bold"))
      (should (eq (plist-get (plist-get elem :annotations) :bold) t)))))

(ert-deftest notion-bridge-org-test-parse-italic ()
  "Italic markup /text/ produces italic annotation."
  (let ((rt (notion-bridge-org--parse-inline-markup "/italic/")))
    (should (>= (length rt) 1))
    (let ((elem (aref rt 0)))
      (should (equal (plist-get (plist-get elem :text) :content) "italic"))
      (should (eq (plist-get (plist-get elem :annotations) :italic) t)))))

(ert-deftest notion-bridge-org-test-parse-link ()
  "Org link [[url][text]] produces link in rich_text."
  (let ((rt (notion-bridge-org--parse-inline-markup "[[https://example.com][Example]]")))
    (should (>= (length rt) 1))
    (let* ((elem (aref rt 0))
           (text-obj (plist-get elem :text)))
      (should (equal (plist-get text-obj :content) "Example"))
      (should (equal (plist-get (plist-get text-obj :link) :url) "https://example.com")))))

(ert-deftest notion-bridge-org-test-parse-empty-string ()
  "Empty string produces empty rich_text array."
  (let ((rt (notion-bridge-org--parse-inline-markup "")))
    (should (= (length rt) 0))))

(ert-deftest notion-bridge-org-test-parse-nil ()
  "Nil produces empty rich_text array."
  (let ((rt (notion-bridge-org--parse-inline-markup nil)))
    (should (= (length rt) 0))))

;;; ============================================================
;;; Edge cases
;;; ============================================================

(ert-deftest notion-bridge-org-test-empty-paragraph ()
  "Empty paragraph renders as an invisible line so it survives a round trip."
  (let* ((block (list :type "paragraph" :paragraph (list :rich_text nil)))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result "​"))
    (let ((back (notion-bridge-org--org-to-blocks result)))
      (should (= (length back) 1))
      (should (equal (plist-get (car back) :type) "paragraph"))
      (should (equal (length (plist-get (plist-get (car back) :paragraph) :rich_text)) 0)))))

(ert-deftest notion-bridge-org-test-unicode-content ()
  "Unicode text passes through correctly."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Hello 世界 🌍")))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "Hello 世界 🌍"))))

(ert-deftest notion-bridge-org-test-multiple-blocks ()
  "Multiple blocks join with newlines."
  (let* ((blocks (list (list :type "heading_1"
                             :heading_1 (list :rich_text
                                              (list (notion-bridge-org-test--make-rich-text "Title"))))
                       (list :type "paragraph"
                             :paragraph (list :rich_text
                                              (list (notion-bridge-org-test--make-rich-text "Body"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "\\* Title" result))
    (should (string-match-p "Body" result))))

(ert-deftest notion-bridge-org-test-nested-bulleted-list ()
  "Nested bulleted list items get indented."
  (let* ((child-rt (list (notion-bridge-org-test--make-rich-text "Child item")))
         (child-block (notion-bridge-org-test--make-block "bulleted_list_item" child-rt))
         (parent-rt (list (notion-bridge-org-test--make-rich-text "Parent item")))
         (parent-block (notion-bridge-org-test--make-block "bulleted_list_item" parent-rt)))
    (plist-put parent-block :children (list child-block))
    (let ((result (notion-bridge-org--block-to-org parent-block)))
      (should (string-match-p "- Parent item" result))
      (should (string-match-p "  - Child item" result)))))

;;; ============================================================
;;; Push serialization (org -> Notion -> JSON)
;;; ============================================================

(ert-deftest notion-bridge-org-test-push-paragraph-serializes ()
  "Paragraph block from org-to-blocks must serialize to valid JSON."
  (let* ((blocks (notion-bridge-org--org-to-blocks "Hello world\n"))
         (block (car blocks))
         (type (plist-get block :type))
         (type-key (intern (concat ":" type)))
         (type-data (plist-get block type-key))
         (update-body (list type-key type-data)))
    (should (stringp (json-serialize update-body :false-object :json-false)))))

(ert-deftest notion-bridge-org-test-push-children-serializes ()
  "A list of blocks as children must serialize to valid JSON array."
  (let* ((blocks (notion-bridge-org--org-to-blocks "Hello\n\nWorld\n"))
         (body (list :children (vconcat blocks))))
    (should (stringp (json-serialize body :false-object :json-false)))))

(ert-deftest notion-bridge-org-test-push-heading-serializes ()
  "Heading block from org-to-blocks must serialize to valid JSON."
  (let* ((blocks (notion-bridge-org--org-to-blocks "* Heading\nBody text\n"))
         (body (list :children (vconcat blocks))))
    (should (stringp (json-serialize body :false-object :json-false)))))

(ert-deftest notion-bridge-org-test-push-mixed-content-serializes ()
  "Mixed content (headings, lists, code) must all serialize."
  (let* ((org-text "* Title\nSome text\n- Item 1\n- Item 2\n#+begin_src python\nprint(1)\n#+end_src\n")
         (blocks (notion-bridge-org--org-to-blocks org-text))
         (body (list :children (vconcat blocks))))
    (should (stringp (json-serialize body :false-object :json-false)))))

(ert-deftest notion-bridge-org-test-push-todo-serializes ()
  "Checkbox items must serialize with correct checked boolean."
  (let* ((blocks (notion-bridge-org--org-to-blocks "- [ ] Unchecked\n- [X] Checked\n"))
         (body (list :children (vconcat blocks)))
         (json-str (json-serialize body :false-object :json-false)))
    (should (stringp json-str))
    (should (string-match-p "\"checked\":false" json-str))
    (should (string-match-p "\"checked\":true" json-str))))

;;; ============================================================
;;; Toggle blocks
;;; ============================================================

(ert-deftest notion-bridge-org-test-toggle-to-org ()
  "Toggle block renders as #+begin_details/#+end_details."
  (let* ((rt (list (notion-bridge-org-test--make-rich-text "Click me")))
         (child-rt (list (notion-bridge-org-test--make-rich-text "Hidden content")))
         (child (notion-bridge-org-test--make-block "paragraph" child-rt))
         (block (notion-bridge-org-test--make-block "toggle" rt)))
    (plist-put block :children (list child))
    (let ((result (notion-bridge-org--block-to-org block)))
      (should (string-match-p "begin_details Click me" result))
      (should (string-match-p "Hidden content" result))
      (should (string-match-p "end_details" result)))))

(ert-deftest notion-bridge-org-test-toggle-from-org ()
  "#+begin_details converts to toggle block with children."
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "#+begin_details Click me\nHidden content\n#+end_details\n")))
    (should (>= (length blocks) 1))
    (let ((block (car blocks)))
      (should (equal (plist-get block :type) "toggle"))
      (should (plist-get block :children))
      (should (> (length (plist-get block :children)) 0)))))

(ert-deftest notion-bridge-org-test-toggle-serializes ()
  "Toggle block serializes to valid JSON as sent to the API."
  (let* ((blocks (notion-bridge-org--org-to-blocks
                  "#+begin_details Title\nContent\n#+end_details\n"))
         (body (list :children (vconcat (mapcar #'notion-bridge-org--wire blocks)))))
    (should (stringp (json-serialize body :false-object :json-false)))))

;;; ============================================================
;;; Paragraph newline splitting
;;; ============================================================

(ert-deftest notion-bridge-org-test-multiline-paragraph-joins ()
  "A filled paragraph is one block; only \\\\ line ends are hard breaks."
  (let ((blocks (notion-bridge-org--org-to-blocks "Line one\nLine two\\\\\nLine three\n")))
    (should (= (length blocks) 1))
    (should (equal (notion-bridge-org--plain-text
                    (plist-get (plist-get (car blocks) :paragraph) :rich_text))
                   "Line one Line two\nLine three"))))

(ert-deftest notion-bridge-org-test-blank-line-paragraphs ()
  "Paragraphs separated by blank lines are separate blocks."
  (let ((blocks (notion-bridge-org--org-to-blocks "Para one.\n\nPara two.\n")))
    (should (= (length blocks) 2))))

;;; ============================================================
;;; Nested lists
;;; ============================================================

(ert-deftest notion-bridge-org-test-nested-checkbox-from-org ()
  "Nested checkboxes produce blocks with children."
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "- [ ] Top\n  - [ ] Nested A\n  - [X] Nested B\n- [ ] Another\n")))
    (should (= (length blocks) 2))
    (let ((first (car blocks)))
      (should (equal (plist-get first :type) "to_do"))
      (should (plist-get first :children))
      (should (= (length (plist-get first :children)) 2)))))

(ert-deftest notion-bridge-org-test-nested-bullet-from-org ()
  "Nested bullet lists produce blocks with children."
  (let ((blocks (notion-bridge-org--org-to-blocks
                 "- Parent\n  - Child A\n  - Child B\n")))
    (should (= (length blocks) 1))
    (should (plist-get (car blocks) :children))
    (should (= (length (plist-get (car blocks) :children)) 2))))

(ert-deftest notion-bridge-org-test-nested-list-serializes ()
  "Nested lists serialize to valid JSON."
  (let* ((blocks (notion-bridge-org--org-to-blocks
                  "- [ ] Top\n  - [ ] Sub\n"))
         (body (list :children (vconcat (mapcar #'notion-bridge-org--wire blocks)))))
    (should (stringp (json-serialize body :false-object :json-false)))))

;;; ============================================================
;;; Block separator (pull direction)
;;; ============================================================

(ert-deftest notion-bridge-org-test-paragraph-double-newline ()
  "Consecutive paragraphs are separated by blank lines."
  (let* ((blocks (list (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Para 1"))))
                       (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Para 2"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "Para 1\n\nPara 2" result))))

(ert-deftest notion-bridge-org-test-list-items-single-newline ()
  "Consecutive list items are separated by single newlines."
  (let* ((blocks (list (list :type "bulleted_list_item" :bulleted_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Item 1"))))
                       (list :type "bulleted_list_item" :bulleted_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Item 2"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "- Item 1\n- Item 2" result))
    (should-not (string-match-p "- Item 1\n\n- Item 2" result))))

;;; ============================================================
;;; Blocks-to-org separator logic
;;; ============================================================

(ert-deftest notion-bridge-org-test-separator-paragraph-to-list ()
  "Paragraph followed by list item gets double newline separator."
  (let* ((blocks (list (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Intro text"))))
                       (list :type "bulleted_list_item" :bulleted_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Item 1"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "Intro text\n\n- Item 1" result))))

(ert-deftest notion-bridge-org-test-separator-list-to-paragraph ()
  "List item followed by paragraph gets double newline separator."
  (let* ((blocks (list (list :type "bulleted_list_item" :bulleted_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Item"))))
                       (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "After"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "- Item\n\nAfter" result))))

(ert-deftest notion-bridge-org-test-separator-mixed-list-types ()
  "Different list types in sequence use single newline separator."
  (let* ((blocks (list (list :type "bulleted_list_item" :bulleted_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Bullet"))))
                       (list :type "numbered_list_item" :numbered_list_item
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Number"))))
                       (list :type "to_do" :to_do
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Task"))
                                   :checked :json-false))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "- Bullet\n1\\. Number\n- \\[ \\] Task" result))
    (should-not (string-match-p "Bullet\n\n1\\." result))))

(ert-deftest notion-bridge-org-test-separator-heading-to-paragraph ()
  "Heading followed by paragraph gets double newline."
  (let* ((blocks (list (list :type "heading_1" :heading_1
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Title"))))
                       (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Body"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (string-match-p "\\* Title\n\nBody" result))))

(ert-deftest notion-bridge-org-test-separator-single-block ()
  "Single block has no separator prefix."
  (let* ((blocks (list (list :type "paragraph" :paragraph
                             (list :rich_text (list (notion-bridge-org-test--make-rich-text "Only"))))))
         (result (notion-bridge-org--blocks-to-org blocks)))
    (should (equal result "Only"))))

;;; ============================================================
;;; Toggle block with nested children roundtrip
;;; ============================================================

(ert-deftest notion-bridge-org-test-toggle-nested-list-children ()
  "Toggle block with nested list children round-trips correctly."
  (let* ((child1-rt (list (notion-bridge-org-test--make-rich-text "Sub item 1")))
         (child2-rt (list (notion-bridge-org-test--make-rich-text "Sub item 2")))
         (child1 (notion-bridge-org-test--make-block "bulleted_list_item" child1-rt))
         (child2 (notion-bridge-org-test--make-block "bulleted_list_item" child2-rt))
         (toggle-rt (list (notion-bridge-org-test--make-rich-text "Expand me")))
         (toggle (notion-bridge-org-test--make-block "toggle" toggle-rt)))
    (plist-put toggle :children (list child1 child2))
    (let ((result (notion-bridge-org--block-to-org toggle)))
      (should (string-match-p "begin_details Expand me" result))
      (should (string-match-p "- Sub item 1" result))
      (should (string-match-p "- Sub item 2" result))
      (should (string-match-p "end_details" result)))))

(ert-deftest notion-bridge-org-test-toggle-nested-paragraph-children ()
  "Toggle block with paragraph children renders correctly."
  (let* ((child-rt (list (notion-bridge-org-test--make-rich-text "Inner paragraph")))
         (child (notion-bridge-org-test--make-block "paragraph" child-rt))
         (toggle-rt (list (notion-bridge-org-test--make-rich-text "Details")))
         (toggle (notion-bridge-org-test--make-block "toggle" toggle-rt)))
    (plist-put toggle :children (list child))
    (let ((result (notion-bridge-org--block-to-org toggle)))
      (should (string-match-p "begin_details Details" result))
      (should (string-match-p "Inner paragraph" result))
      (should (string-match-p "end_details" result)))))

;;; ============================================================
;;; Page mention rich text handling
;;; ============================================================

(ert-deftest notion-bridge-org-test-page-mention-no-page-map ()
  "Page mention without page-map uses notion: link."
  (let* ((notion-bridge-org--page-map nil)
         (rt (list (list :type "mention"
                         :mention (list :type "page"
                                        :page (list :id "abc-123"))
                         :plain_text "Linked Page")))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "[[notion:abc-123][Linked Page]]"))))

(ert-deftest notion-bridge-org-test-page-mention-with-page-map ()
  "Page mention with page-map uses file: link."
  (let* ((notion-bridge-org--page-map (make-hash-table :test 'equal)))
    (puthash "abc-123" "linked-page.org" notion-bridge-org--page-map)
    (let* ((rt (list (list :type "mention"
                           :mention (list :type "page"
                                          :page (list :id "abc-123"))
                           :plain_text "Linked Page")))
           (result (notion-bridge-org--rich-text-to-org rt)))
      (should (equal result "[[file:linked-page.org][Linked Page]]")))))

(ert-deftest notion-bridge-org-test-page-mention-missing-plain-text ()
  "Page mention with no plain_text falls back to 'link'."
  (let* ((notion-bridge-org--page-map nil)
         (rt (list (list :type "mention"
                         :mention (list :type "page"
                                        :page (list :id "xyz-456")))))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "[[notion:xyz-456][link]]"))))

(ert-deftest notion-bridge-org-test-non-page-mention ()
  "Non-page mention (e.g. user mention) renders plain_text."
  (let* ((rt (list (list :type "mention"
                         :mention (list :type "user"
                                        :user (list :id "user-1"))
                         :plain_text "@Alice")))
         (result (notion-bridge-org--rich-text-to-org rt)))
    (should (equal result "@Alice"))))

;;; ============================================================
;;; child_page block rendering
;;; ============================================================

(ert-deftest notion-bridge-org-test-child-page-no-page-map ()
  "child_page renders as a read-only placeholder linking to the subpage."
  (let* ((notion-bridge-org--page-map nil)
         (block (list :type "child_page"
                      :id "child-page-id"
                      :child_page (list :title "Sub Page")))
         (result (notion-bridge-org--block-to-org block)))
    (should (equal result (concat "#+NOTION_BLOCK: child-page-id child_page Sub Page\n"
                                  "# [[notion:child-page-id][open subpage]]")))
    (should (equal (notion-bridge-org--org-to-blocks result)
                   '((:type "notion_keep" :keep-id "child-page-id"))))))

(ert-deftest notion-bridge-org-test-child-page-with-page-map ()
  "child_page block with page-map renders as file: link."
  (let* ((notion-bridge-org--page-map (make-hash-table :test 'equal)))
    (puthash "child-page-id" "sub-page.org" notion-bridge-org--page-map)
    (let* ((block (list :type "child_page"
                        :id "child-page-id"
                        :child_page (list :title "Sub Page")))
           (result (notion-bridge-org--block-to-org block)))
      (should (equal result (concat "#+NOTION_BLOCK: child-page-id child_page Sub Page\n"
                                    "# [[file:sub-page.org][open subpage]]"))))))

(ert-deftest notion-bridge-org-test-child-page-no-title ()
  "child_page block with nil title renders as Untitled."
  (let* ((notion-bridge-org--page-map nil)
         (block (list :type "child_page"
                      :id "child-page-id"
                      :child_page (list :title nil)))
         (result (notion-bridge-org--block-to-org block)))
    (should (string-match-p "Untitled" result))))

(provide 'notion-bridge-org-test)
;;; notion-bridge-org-test.el ends here
