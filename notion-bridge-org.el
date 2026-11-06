;;; notion-bridge-org.el --- Bidirectional org-mode/Notion conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: notion-bridge
;; Keywords: org, notion

;;; Commentary:

;; Converts between Notion block JSON (plists) and org-mode text.
;;
;; Only block types that survive a round trip are rendered as editable
;; org.  Everything else (subpages, databases, images, callouts, ...)
;; becomes a `#+NOTION_BLOCK:' placeholder line, optionally followed
;; by a read-only preview in org comment lines.  A placeholder keeps
;; its block untouched on push; removing the line deletes the block.
;;
;; Text that org would misread (stray emphasis markers, lines starting
;; like a headline or list) is escaped with zero-width spaces, which
;; are stripped again when parsing.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)

(defvar notion-bridge-org--page-map nil
  "Hash table mapping Notion page IDs to local org filenames.
Set by `notion-bridge-sync-pull-all' before conversion so that
page mentions and child_page blocks become file links.")

(defconst notion-bridge-org--zwsp "​")

(defconst notion-bridge-org--markers '(?* ?/ ?+ ?~ ?= ?_))

(defconst notion-bridge-org--pre-chars " \t\n-('\"{​"
  "Characters that may precede an opening emphasis marker.")

(defconst notion-bridge-org--post-chars " \t\n-.,:!?;'\")}[​"
  "Characters that may follow a closing emphasis marker.")

(defconst notion-bridge-org--editable-types
  '("paragraph" "heading_1" "heading_2" "heading_3"
    "bulleted_list_item" "numbered_list_item" "to_do"
    "code" "quote" "divider" "toggle" "table"))

(defconst notion-bridge-org--list-types
  '("bulleted_list_item" "numbered_list_item" "to_do"))

(defconst notion-bridge-org--code-languages
  '("abap" "arduino" "bash" "basic" "c" "clojure" "coffeescript" "c++" "c#"
    "css" "dart" "diff" "docker" "elixir" "elm" "erlang" "flow" "fortran"
    "f#" "gherkin" "glsl" "go" "graphql" "groovy" "haskell" "html" "java"
    "javascript" "json" "julia" "kotlin" "latex" "less" "lisp" "livescript"
    "lua" "makefile" "markdown" "markup" "matlab" "mermaid" "nix"
    "objective-c" "ocaml" "pascal" "perl" "php" "plain text" "powershell"
    "prolog" "protobuf" "python" "r" "reason" "ruby" "rust" "sass" "scala"
    "scheme" "scss" "shell" "sql" "swift" "typescript" "vb.net" "verilog"
    "vhdl" "visual basic" "webassembly" "xml" "yaml" "java/c/c++/c#")
  "Languages Notion accepts for code blocks.")

(defconst notion-bridge-org--language-aliases
  '(("emacs-lisp" . "lisp") ("elisp" . "lisp") ("sh" . "shell")
    ("zsh" . "shell") ("js" . "javascript") ("ts" . "typescript")
    ("py" . "python") ("yml" . "yaml") ("cpp" . "c++") ("csharp" . "c#")
    ("dockerfile" . "docker") ("text" . "plain text") ("" . "plain text")
    ("example" . "plain text") ("hcl" . "plain text") ("toml" . "plain text"))
  "Org source block languages mapped to Notion's names.")

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun notion-bridge-org--type-data (block)
  (let ((type (plist-get block :type)))
    (and type (plist-get block (intern (concat ":" type))))))

(defun notion-bridge-org--editable-p (block depth)
  "Non-nil if BLOCK at DEPTH can be rendered as editable org."
  (let* ((type (plist-get block :type))
         (data (notion-bridge-org--type-data block))
         (children (plist-get block :children)))
    (and (member type notion-bridge-org--editable-types)
         (not (and (eq (plist-get block :has_children) t) (null children)))
         (pcase type
           ((or "heading_1" "heading_2" "heading_3")
            (and (= depth 0) (not (eq (plist-get data :is_toggleable) t))))
           ((or "paragraph" "code" "quote" "divider") (null children))
           ("table" (cl-every (lambda (row) (equal (plist-get row :type) "table_row"))
                              children))
           (_ t)))))

(defun notion-bridge-org--code-language-to-org (lang)
  (if (or (null lang) (equal lang "plain text")) "text" lang))

(defun notion-bridge-org--code-language-to-notion (lang)
  (let ((l (downcase (or lang ""))))
    (cond
     ((member l notion-bridge-org--code-languages) l)
     ((assoc l notion-bridge-org--language-aliases)
      (cdr (assoc l notion-bridge-org--language-aliases)))
     (t "plain text"))))

;;; ============================================================
;;; Notion -> Org
;;; ============================================================

(defun notion-bridge-org--escape-markup (text)
  "Break emphasis openers and link brackets in plain TEXT with ZWSP."
  (let ((out nil) (len (length text)))
    (dotimes (i len)
      (let ((c (aref text i)))
        (push (string c) out)
        (when (and (memq c notion-bridge-org--markers)
                   (or (= i 0)
                       (seq-contains-p notion-bridge-org--pre-chars (aref text (1- i))))
                   (< (1+ i) len)
                   (not (memq (aref text (1+ i)) '(?\s ?\t ?\n))))
          (push notion-bridge-org--zwsp out))
        (when (and (eq c ?\[) (< (1+ i) len) (eq (aref text (1+ i)) ?\[))
          (push notion-bridge-org--zwsp out))))
    (apply #'concat (nreverse out))))

(defun notion-bridge-org--escape-line-start (line)
  "Prefix LINE with ZWSP if org would read it as structure."
  (if (let ((case-fold-search nil)) (string-match-p
       (concat "\\`\\(\\*+[ \t]\\|[-+][ \t]\\|[-+]\\'\\|[0-9]+[.)]\\([ \t]\\|\\'\\)"
               "\\|#\\|:[ \t]\\|:\\'\\|:[[:alnum:]_-]+:\\||\\|-----"
               "\\|\\[\\(?:[ X-]\\]\\|fn:\\)\\|\\\\begin{"
               "\\|\\(?:CLOCK\\|DEADLINE\\|SCHEDULED\\|CLOSED\\):\\)")
       line))
      (concat notion-bridge-org--zwsp line)
    line))

(defun notion-bridge-org--rich-text-to-org (rich-text-array &optional cont-indent)
  "Convert Notion RICH-TEXT-ARRAY to an org string.
Soft line breaks become org's \\\\ line breaks, continued with
CONT-INDENT."
  (let ((out "") (prev-emphasis nil))
    (dolist (rt (append rich-text-array nil))
      (let* ((rt-type (plist-get rt :type))
             (ann (plist-get rt :annotations))
             (piece
              (cond
               ((equal rt-type "mention")
                (let* ((mention (plist-get rt :mention))
                       (text (or (plist-get rt :plain_text) "link")))
                  (if (equal (plist-get mention :type) "page")
                      (let* ((page-id (plist-get (plist-get mention :page) :id))
                             (local (and notion-bridge-org--page-map
                                         (gethash page-id notion-bridge-org--page-map))))
                        (format "[[%s][%s]]"
                                (if local (concat "file:" local)
                                  (concat "notion:" page-id))
                                (notion-bridge-org--link-desc text)))
                    (notion-bridge-org--escape-markup text))))
               ((equal rt-type "equation")
                (notion-bridge-org--escape-markup
                 (format "\\(%s\\)" (or (plist-get (plist-get rt :equation) :expression) ""))))
               (t
                (let* ((text-obj (plist-get rt :text))
                       (content (or (plist-get text-obj :content)
                                    (plist-get rt :plain_text) ""))
                       (href (plist-get (plist-get text-obj :link) :url))
                       (wrapped (notion-bridge-org--wrap-annotations
                                 (if (eq (plist-get ann :code) t)
                                     content
                                   (notion-bridge-org--escape-markup content))
                                 ann)))
                  (if href
                      (let ((page-id (notion-bridge-org--extract-page-id-from-url href)))
                        (format "[[%s][%s]]"
                                (if (and page-id notion-bridge-org--page-map
                                         (gethash page-id notion-bridge-org--page-map))
                                    (concat "file:" (gethash page-id notion-bridge-org--page-map))
                                  (notion-bridge-org--link-target href))
                                (notion-bridge-org--link-desc wrapped)))
                    wrapped))))))
        (let ((emphasis (and (notion-bridge-org--annotated-p ann)
                             (not (string-prefix-p "[[" piece))
                             (not (equal rt-type "mention")))))
          (when (and (not (string-empty-p out)) (not (string-empty-p piece))
                     (or (and emphasis
                              (memq (aref piece 0) notion-bridge-org--markers)
                              (not (seq-contains-p notion-bridge-org--pre-chars
                                                   (aref out (1- (length out))))))
                         (and prev-emphasis
                              (not (seq-contains-p notion-bridge-org--post-chars
                                                   (aref piece 0))))))
            (setq out (concat out notion-bridge-org--zwsp)))
          (unless (string-empty-p piece)
            (setq prev-emphasis
                  (and emphasis
                       (memq (aref piece (1- (length piece)))
                             notion-bridge-org--markers)))))
        (setq out (concat out piece))))
    (mapconcat #'identity (split-string out "\n")
               (concat "\\\\\n" (or cont-indent "")))))

(defun notion-bridge-org--annotated-p (ann)
  (cl-some (lambda (k) (eq (plist-get ann k) t))
           '(:bold :italic :strikethrough :underline :code)))

(defun notion-bridge-org--wrap-annotations (text ann)
  "Wrap TEXT in emphasis markers for ANN, keeping edge whitespace outside."
  (if (or (string-blank-p text) (not (notion-bridge-org--annotated-p ann)))
      text
    (string-match "\\`\\([ \t\n]*\\)\\(\\(?:.\\|\n\\)*?\\)\\([ \t\n]*\\)\\'" text)
    (let ((lead (match-string 1 text))
          (trail (match-string 3 text))
          (r (match-string 2 text)))
      (when (eq (plist-get ann :code) t) (setq r (concat "~" r "~")))
      (when (eq (plist-get ann :strikethrough) t) (setq r (concat "+" r "+")))
      (when (eq (plist-get ann :underline) t) (setq r (concat "_" r "_")))
      (when (eq (plist-get ann :italic) t) (setq r (concat "/" r "/")))
      (when (eq (plist-get ann :bold) t) (setq r (concat "*" r "*")))
      (concat lead r trail))))

(defun notion-bridge-org--link-desc (s)
  (replace-regexp-in-string "\\]" "}" (replace-regexp-in-string "\\[" "{" s)))

(defun notion-bridge-org--link-target (href)
  (replace-regexp-in-string "\\]" "%5D" (replace-regexp-in-string "\\[" "%5B" href)))

(defun notion-bridge-org--extract-page-id-from-url (url)
  "Extract a Notion page ID from URL if it is a Notion page link."
  (when (and url (string-match
                  "notion\\.\\(?:so\\|site\\)/\\(?:[^/]+/\\)*\\(?:[^/?#]*-\\)?\\([0-9a-f]\\{32\\}\\)\\b"
                  url))
    (let ((hex (match-string 1 url)))
      (format "%s-%s-%s-%s-%s"
              (substring hex 0 8) (substring hex 8 12) (substring hex 12 16)
              (substring hex 16 20) (substring hex 20)))))

(defun notion-bridge-org--plain-text (rich-text)
  (mapconcat (lambda (rt) (or (plist-get rt :plain_text)
                              (plist-get (plist-get rt :text) :content) ""))
             rich-text ""))

(defun notion-bridge-org--escape-text-lines (text indent)
  "Escape structural line starts in TEXT whose lines continue at INDENT."
  (let ((lines (split-string text (concat "\\\\\\\\\n" (regexp-quote indent)))))
    (mapconcat #'notion-bridge-org--escape-line-start lines
               (concat "\\\\\n" indent))))

(defun notion-bridge-org--escape-heading (text)
  "Keep org from reading TODO keywords, priorities or tags in heading TEXT."
  (let* ((case-fold-search nil)
         (text (if (string-match-p "\\`\\(?:[A-Z]+ \\|\\[#\\|COMMENT\\b\\)" text)
                  (concat notion-bridge-org--zwsp text)
                text)))
    (if (string-match-p "[ \t]:[[:alnum:]_@#%:]+:[ \t]*\\'" text)
        (concat text notion-bridge-org--zwsp)
      text)))

(defun notion-bridge-org--placeholder (block depth)
  "Render BLOCK as a read-only placeholder at DEPTH."
  (let* ((indent (make-string (* 2 depth) ?\s))
         (type (plist-get block :type))
         (data (notion-bridge-org--type-data block))
         (summary
          (pcase type
            ("child_page" (or (plist-get data :title) "Untitled"))
            ("child_database" (or (plist-get data :title) "Untitled"))
            ((or "image" "file" "pdf" "video" "audio")
             (or (notion-bridge-org--plain-text (plist-get data :caption))
                 ""))
            ((or "bookmark" "embed" "link_preview") (or (plist-get data :url) ""))
            ("equation" (or (plist-get data :expression) ""))
            ("link_to_page" (or (plist-get data :page_id) (plist-get data :database_id) ""))
            (_ (notion-bridge-org--plain-text (plist-get data :rich_text)))))
         (summary (car (split-string (or summary "") "\n")))
         (preview (notion-bridge-org--preview-lines block)))
    (concat indent "#+NOTION_BLOCK: " (plist-get block :id) " " type
            (if (string-empty-p summary) "" (concat " " summary))
            (mapconcat (lambda (l) (concat "\n" indent "# " l)) preview ""))))

(defun notion-bridge-org--preview-lines (block)
  "Read-only preview lines for BLOCK's content."
  (let ((type (plist-get block :type))
        (data (notion-bridge-org--type-data block)))
    (append
     (pcase type
       ((or "image" "file" "pdf" "video" "audio")
        (let ((url (plist-get (or (plist-get data :external) (plist-get data :file)) :url)))
          (and url (list url))))
       ((or "callout" "quote" "paragraph" "heading_1" "heading_2" "heading_3"
            "toggle" "bulleted_list_item" "numbered_list_item" "to_do")
        (cdr (split-string (notion-bridge-org--plain-text (plist-get data :rich_text)) "\n")))
       ("child_page"
        (list (format "[[%s][open subpage]]"
                      (let ((local (and notion-bridge-org--page-map
                                        (gethash (plist-get block :id)
                                                 notion-bridge-org--page-map))))
                        (if local (concat "file:" local)
                          (concat "notion:" (plist-get block :id))))))))
     (cl-loop for child in (plist-get block :children)
              append (let ((cdata (notion-bridge-org--type-data child)))
                       (if (equal (plist-get child :type) "table_row")
                           (list (concat "| "
                                         (mapconcat #'notion-bridge-org--plain-text
                                                    (plist-get cdata :cells) " | ")
                                         " |"))
                         (mapcar (lambda (l) (concat "  " l))
                                 (cons (notion-bridge-org--plain-text
                                        (plist-get cdata :rich_text))
                                       (notion-bridge-org--preview-lines child)))))))))

(defun notion-bridge-org--table-to-org (block indent)
  (let* ((data (notion-bridge-org--type-data block))
         (header (eq (plist-get data :has_column_header) t))
         (rows (plist-get block :children))
         (width (or (plist-get data :table_width) 1))
         (lines nil))
    (cl-loop for row in rows
             for i from 0
             do (let ((cells (plist-get (notion-bridge-org--type-data row) :cells)))
                  (push (concat indent "| "
                                (mapconcat
                                 (lambda (j)
                                   (let ((cell (and (< j (length cells)) (elt cells j))))
                                     (replace-regexp-in-string
                                      "|" "\\vert{}"
                                      (replace-regexp-in-string
                                       "\\\\\\\\\n" " "
                                       (notion-bridge-org--rich-text-to-org cell))
                                      t t)))
                                 (number-sequence 0 (1- width)) " | ")
                                " |")
                        lines)
                  (when (and header (= i 0))
                    (push (concat indent "|"
                                  (mapconcat (lambda (_) "---") (number-sequence 1 width) "+")
                                  "|")
                          lines))))
    (string-join (nreverse lines) "\n")))

(defun notion-bridge-org--block-self-to-org (block depth)
  "Render BLOCK itself, without children, at DEPTH."
  (let* ((type (plist-get block :type))
         (indent (make-string (* 2 depth) ?\s))
         (data (notion-bridge-org--type-data block))
         (rich (plist-get data :rich_text)))
    (if (not (notion-bridge-org--editable-p block depth))
        (notion-bridge-org--placeholder block depth)
      (pcase type
        ("paragraph"
         (let ((text (notion-bridge-org--rich-text-to-org rich indent)))
           (if (string-empty-p text)
               (concat indent notion-bridge-org--zwsp)
             (concat indent (notion-bridge-org--escape-text-lines text indent)))))
        ((or "heading_1" "heading_2" "heading_3")
         (concat (pcase type ("heading_1" "* ") ("heading_2" "** ") (_ "*** "))
                 (notion-bridge-org--escape-heading
                  (replace-regexp-in-string
                   "\\\\\\\\\n" " " (notion-bridge-org--rich-text-to-org rich)))))
        ((or "bulleted_list_item" "numbered_list_item" "to_do")
         (let* ((bullet (if (equal type "numbered_list_item") "1. " "- "))
                (cont (concat indent (make-string (length bullet) ?\s)))
                (checkbox (when (equal type "to_do")
                            (if (eq (plist-get data :checked) t) "[X] " "[ ] ")))
                (text (notion-bridge-org--rich-text-to-org rich cont)))
           (concat indent bullet (or checkbox "")
                   (notion-bridge-org--escape-text-lines text cont))))
        ("code"
         (concat indent "#+begin_src "
                 (notion-bridge-org--code-language-to-org (plist-get data :language))
                 "\n"
                 (let ((code (org-escape-code-in-string
                              (notion-bridge-org--plain-text rich))))
                   (mapconcat (lambda (l) (if (string-empty-p l) l (concat indent l)))
                              (split-string code "\n") "\n"))
                 "\n" indent "#+end_src"))
        ("quote"
         (concat indent "#+begin_quote\n" indent
                 (notion-bridge-org--escape-text-lines
                  (notion-bridge-org--rich-text-to-org rich indent) indent)
                 "\n" indent "#+end_quote"))
        ("divider" (concat indent "-----"))
        ("toggle"
         (concat indent "#+begin_details "
                 (replace-regexp-in-string
                  "\\\\\\\\\n" " " (notion-bridge-org--rich-text-to-org rich))))
        ("table" (notion-bridge-org--table-to-org block indent))))))

(defun notion-bridge-org--separator (prev cur)
  "Text between sibling blocks PREV and CUR."
  (let ((pt (plist-get prev :type)) (ct (plist-get cur :type)))
    (if (and (member pt notion-bridge-org--list-types)
             (member ct notion-bridge-org--list-types))
        "\n"
      "\n\n")))

(defun notion-bridge-org--block-to-org (block &optional depth)
  "Convert a Notion BLOCK, with its children, to org text at DEPTH."
  (let* ((depth (or depth 0))
         (self (notion-bridge-org--block-self-to-org block depth))
         (type (plist-get block :type))
         (children (plist-get block :children)))
    (cond
     ((not (notion-bridge-org--editable-p block depth)) self)
     ((equal type "toggle")
      (concat self "\n"
              (notion-bridge-org--blocks-to-org children (1+ depth))
              (if children "\n" "")
              (make-string (* 2 depth) ?\s) "#+end_details"))
     ((and children (member type notion-bridge-org--list-types))
      (concat self
              (if (member (plist-get (car children) :type)
                          notion-bridge-org--list-types)
                  "\n" "\n\n")
              (notion-bridge-org--blocks-to-org children (1+ depth))))
     (t self))))

(defun notion-bridge-org--blocks-to-org (blocks &optional depth)
  "Convert a list of Notion BLOCKS to an org string at DEPTH."
  (let ((result "") (prev nil))
    (dolist (block blocks)
      (setq result (concat result
                           (if prev (notion-bridge-org--separator prev block) "")
                           (notion-bridge-org--block-to-org block depth)))
      (setq prev block))
    result))

(defun notion-bridge-org--block-key (block &optional depth)
  "Comparison key for BLOCK at DEPTH: its own org rendering, or its ID.
Read-only blocks are keyed by ID.  Whitespace at line edges is
ignored, since org does not preserve it."
  (if (or (equal (plist-get block :type) "notion_keep")
          (not (notion-bridge-org--editable-p block (or depth 0))))
      (concat "keep:" (notion-bridge-org--norm-id
                       (or (plist-get block :keep-id) (plist-get block :id))))
    (concat (plist-get block :type) "\n"
            (replace-regexp-in-string
             "^[ \t]+\\|[ \t]+$" ""
             (notion-bridge-org--block-self-to-org block 0)))))

(defun notion-bridge-org--norm-id (id)
  (and id (downcase (replace-regexp-in-string "-" "" id))))

;;; ============================================================
;;; Org -> Notion
;;; ============================================================

(defun notion-bridge-org--page-id-for-file (file)
  (when notion-bridge-org--page-map
    (let ((target (file-name-nondirectory file)) found)
      (maphash (lambda (id f) (when (equal f target) (setq found id)))
               notion-bridge-org--page-map)
      found)))

(defun notion-bridge-org--chunk (content)
  "Split CONTENT into Notion's 2000-character text limit."
  (if (<= (length content) 2000)
      (list content)
    (cons (substring content 0 2000)
          (notion-bridge-org--chunk (substring content 2000)))))

(defun notion-bridge-org--make-rich-text (content &optional annotations)
  "Notion rich_text elements for CONTENT with ANNOTATIONS plist."
  (let ((content (string-replace notion-bridge-org--zwsp "" content))
        (href (plist-get annotations :href)))
    (mapcar
     (lambda (piece)
       (let ((rt (list :type "text"
                       :text (list :content piece)
                       :annotations
                       (list :bold (if (plist-get annotations :bold) t :json-false)
                             :italic (if (plist-get annotations :italic) t :json-false)
                             :strikethrough (if (plist-get annotations :strikethrough) t :json-false)
                             :underline (if (plist-get annotations :underline) t :json-false)
                             :code (if (plist-get annotations :code) t :json-false)
                             :color "default"))))
         (when href
           (setq rt (plist-put rt :text (list :content piece :link (list :url href)))))
         rt))
     (if (string-empty-p content) nil (notion-bridge-org--chunk content)))))

(defun notion-bridge-org--make-mention (page-id text annotations)
  (list (list :type "mention"
              :mention (list :type "page" :page (list :id page-id))
              :plain_text text
              :annotations
              (list :bold (if (plist-get annotations :bold) t :json-false)
                    :italic (if (plist-get annotations :italic) t :json-false)
                    :strikethrough (if (plist-get annotations :strikethrough) t :json-false)
                    :underline (if (plist-get annotations :underline) t :json-false)
                    :code (if (plist-get annotations :code) t :json-false)
                    :color "default"))))

(defun notion-bridge-org--link-parts (href desc ann)
  "Rich text for an org link to HREF described by DESC."
  (let ((desc (or desc href)))
    (cond
     ((string-prefix-p "notion:" href)
      (notion-bridge-org--make-mention (substring href 7) desc ann))
     ((string-prefix-p "file:" href)
      (let ((id (notion-bridge-org--page-id-for-file (substring href 5))))
        (if id
            (notion-bridge-org--make-mention id desc ann)
          (notion-bridge-org--parse-inline desc ann))))
     ((string-match-p "\\`\\(https?\\|mailto\\):" href)
      (let ((href (replace-regexp-in-string
                   "%5D" "]" (replace-regexp-in-string "%5B" "[" href))))
        (mapcar (lambda (rt)
                  (if (equal (plist-get rt :type) "text")
                      (plist-put rt :text
                                 (list :content (plist-get (plist-get rt :text) :content)
                                       :link (list :url href)))
                    rt))
                (notion-bridge-org--parse-inline desc ann))))
     (t (notion-bridge-org--parse-inline desc ann)))))

(defun notion-bridge-org--opener-at-p (text pos len)
  (and (memq (aref text pos) notion-bridge-org--markers)
       (or (= pos 0) (seq-contains-p notion-bridge-org--pre-chars (aref text (1- pos))))
       (< (1+ pos) len)
       (not (memq (aref text (1+ pos)) '(?\s ?\t ?\n ?​)))))

(defun notion-bridge-org--find-closer (text marker start len)
  "Position of a valid closing MARKER in TEXT at or after START."
  (let ((i start) found)
    (while (and (not found) (< i len))
      (when (and (eq (aref text i) marker)
                 (> i start)
                 (not (memq (aref text (1- i)) '(?\s ?\t ?\n)))
                 (or (= (1+ i) len)
                     (seq-contains-p notion-bridge-org--post-chars (aref text (1+ i)))))
        (setq found i))
      (when (eq (aref text i) ?\n) (setq i len))
      (cl-incf i))
    found))

(defun notion-bridge-org--parse-inline (text &optional ann)
  "Parse org inline markup in TEXT into a list of rich_text elements.
ANN is inherited annotations.  Emphasis needs org's word boundaries."
  (let ((parts nil) (pos 0) (len (length (or text ""))) (plain ""))
    (cl-flet ((flush ()
                (unless (string-empty-p plain)
                  (setq parts (append parts (notion-bridge-org--make-rich-text plain ann)))
                  (setq plain ""))))
      (while (< pos len)
        (cond
         ((and (eq (aref text pos) ?\[) (< (1+ pos) len) (eq (aref text (1+ pos)) ?\[)
               (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]" text pos)
               (= (match-beginning 0) pos))
          (let ((href (match-string 1 text)) (desc (match-string 2 text))
                (end (match-end 0)))
            (flush)
            (setq parts (append parts (notion-bridge-org--link-parts href desc ann)))
            (setq pos end)))
         ((notion-bridge-org--opener-at-p text pos len)
          (let* ((m (aref text pos))
                 (close (notion-bridge-org--find-closer text m (1+ pos) len)))
            (if (not close)
                (setq plain (concat plain (string m)) pos (1+ pos))
              (let ((inner (substring text (1+ pos) close))
                    (key (pcase m (?* :bold) (?/ :italic) (?+ :strikethrough)
                                (?_ :underline) (_ :code))))
                (flush)
                (setq parts
                      (append parts
                              (if (eq key :code)
                                  (notion-bridge-org--make-rich-text
                                   inner (plist-put (copy-sequence ann) :code t))
                                (notion-bridge-org--parse-inline
                                 inner (plist-put (copy-sequence ann) key t)))))
                (setq pos (1+ close))))))
         (t (setq plain (concat plain (string (aref text pos))) pos (1+ pos)))))
      (flush))
    parts))

(defun notion-bridge-org--parse-inline-markup (text)
  "Parse org inline markup in TEXT and return a Notion rich_text vector."
  (vconcat (notion-bridge-org--parse-inline (or text ""))))

(defun notion-bridge-org--org-to-rich-text (text)
  (notion-bridge-org--parse-inline-markup text))

(defun notion-bridge-org--join-lines (text)
  "Join a paragraph's lines: \\\\ line ends are hard breaks, others spaces."
  (let* ((lines (split-string text "\n"))
         (out (car lines)))
    (dolist (l (cdr lines))
      (if (string-suffix-p "\\\\" (string-trim-right out))
          (setq out (concat (string-remove-suffix "\\\\" (string-trim-right out))
                            "\n" (string-trim-left l)))
        (setq out (concat out " " (string-trim-left l)))))
    (string-remove-suffix "\\\\" (string-trim-right out))))

(defun notion-bridge-org--make-block (type type-data &optional children)
  "Create a Notion block plist with TYPE and TYPE-DATA and CHILDREN."
  (let ((block (list :type type (intern (concat ":" type)) type-data)))
    (when children
      (setq block (plist-put block :children (append children nil))))
    block))

(defun notion-bridge-org--text-of (element)
  (let ((begin (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (if (and begin end)
        (string-trim (buffer-substring-no-properties begin end))
      "")))

(defun notion-bridge-org--paragraph-block (text)
  (notion-bridge-org--make-block
   "paragraph" (list :rich_text (notion-bridge-org--org-to-rich-text
                                 (notion-bridge-org--join-lines text)))))

(defun notion-bridge-org--code-value (element)
  "Source of a src or example ELEMENT with its own indentation removed."
  (let* ((value (or (org-element-property :value element) ""))
         (col (save-excursion
                (goto-char (org-element-property :post-affiliated element))
                (current-indentation)))
         (prefix (make-string col ?\s)))
    (setq value (string-remove-suffix "\n" value))
    (mapconcat (lambda (l) (string-remove-prefix prefix l))
               (split-string value "\n") "\n")))

(defun notion-bridge-org--table-block (element)
  (let* ((rows (cl-remove-if-not
                (lambda (r) (eq (org-element-property :type r) 'standard))
                (org-element-contents element)))
         (header (let ((first (car (org-element-contents element))))
                   (and (eq (org-element-property :type first) 'standard)
                        (eq (org-element-property
                             :type (cadr (org-element-contents element)))
                            'rule))))
         (cells (mapcar (lambda (r)
                          (mapcar (lambda (c)
                                    (replace-regexp-in-string
                                     "\\\\vert{}" "|" (notion-bridge-org--text-of c) t t))
                                  (org-element-contents r)))
                        rows))
         (width (max 1 (apply #'max 0 (mapcar #'length cells)))))
    (notion-bridge-org--make-block
     "table"
     (list :table_width width
           :has_column_header (if header t :json-false)
           :has_row_header :json-false)
     (mapcar (lambda (row)
               (notion-bridge-org--make-block
                "table_row"
                (list :cells (vconcat
                              (mapcar (lambda (j)
                                        (notion-bridge-org--org-to-rich-text
                                         (or (nth j row) "")))
                                      (number-sequence 0 (1- width)))))))
             cells))))

(defun notion-bridge-org--element-to-blocks (element)
  "Convert a single org ELEMENT to a list of Notion block plists."
  (pcase (org-element-type element)
    ('headline
     (unless (member "NOTION_COMMENTS" (org-element-property :tags element))
       (let ((level (org-element-property :level element))
             (title (or (org-element-property :raw-value element) "")))
         (cons (notion-bridge-org--make-block
                (pcase level (1 "heading_1") (2 "heading_2") (_ "heading_3"))
                (list :rich_text (notion-bridge-org--org-to-rich-text title)))
               (notion-bridge-org--elements-to-blocks (org-element-contents element))))))
    ('paragraph
     (list (notion-bridge-org--paragraph-block (notion-bridge-org--text-of element))))
    ('plain-list
     (cl-loop for item in (org-element-contents element)
              when (eq (org-element-type item) 'item)
              collect (notion-bridge-org--item-block
                       item (string-match-p "\\`[ \t]*[0-9]"
                                            (org-element-property :bullet item)))))
    ('src-block
     (list (notion-bridge-org--make-block
            "code"
            (list :rich_text (vconcat (notion-bridge-org--make-rich-text
                                       (notion-bridge-org--code-value element)))
                  :language (notion-bridge-org--code-language-to-notion
                             (org-element-property :language element))))))
    ('example-block
     (list (notion-bridge-org--make-block
            "code"
            (list :rich_text (vconcat (notion-bridge-org--make-rich-text
                                       (notion-bridge-org--code-value element)))
                  :language "plain text"))))
    ('fixed-width
     (list (notion-bridge-org--make-block
            "code"
            (list :rich_text (vconcat (notion-bridge-org--make-rich-text
                                       (string-remove-suffix
                                        "\n" (org-element-property :value element))))
                  :language "plain text"))))
    ('quote-block
     (let ((paras (cl-loop for el in (org-element-contents element)
                           when (eq (org-element-type el) 'paragraph)
                           collect (notion-bridge-org--join-lines
                                    (notion-bridge-org--text-of el)))))
       (list (notion-bridge-org--make-block
              "quote" (list :rich_text (notion-bridge-org--org-to-rich-text
                                        (string-join paras "\n")))))))
    ('special-block
     (when (equal (org-element-property :type element) "details")
       (list (notion-bridge-org--make-block
              "toggle"
              (list :rich_text (notion-bridge-org--org-to-rich-text
                                (or (org-element-property :parameters element) "")))
              (notion-bridge-org--elements-to-blocks (org-element-contents element))))))
    ('horizontal-rule
     (list (notion-bridge-org--make-block "divider" (list))))
    ('table
     (when (eq (org-element-property :type element) 'org)
       (list (notion-bridge-org--table-block element))))
    ('keyword
     (when (equal (org-element-property :key element) "NOTION_BLOCK")
       (let ((id (car (split-string (org-element-property :value element)))))
         (list (list :type "notion_keep" :keep-id id)))))
    ('verse-block
     (list (notion-bridge-org--make-block
            "paragraph"
            (list :rich_text (notion-bridge-org--org-to-rich-text
                              (string-trim (notion-bridge-org--text-of element)))))))
    ((or 'section 'org-data 'center-block)
     (notion-bridge-org--elements-to-blocks (org-element-contents element)))
    (_ nil)))

(defun notion-bridge-org--item-block (item ordered)
  (let* ((checkbox (org-element-property :checkbox item))
         (tag (org-element-property :tag item))
         (contents (org-element-contents item))
         (first (and (eq (org-element-type (car contents)) 'paragraph) (car contents)))
         (text (concat (if tag
                           (concat (string-trim
                                    (buffer-substring-no-properties
                                     (org-element-property :begin (car tag))
                                     (org-element-property :end (car (last tag)))))
                                   " :: ")
                         "")
                       (if first
                           (notion-bridge-org--join-lines
                            (notion-bridge-org--text-of first))
                         "")))
         (children (notion-bridge-org--elements-to-blocks
                    (if first (cdr contents) contents)))
         (rich (notion-bridge-org--org-to-rich-text text)))
    (cond
     (checkbox
      (notion-bridge-org--make-block
       "to_do" (list :rich_text rich :checked (if (eq checkbox 'on) t :json-false))
       children))
     (ordered
      (notion-bridge-org--make-block "numbered_list_item" (list :rich_text rich) children))
     (t
      (notion-bridge-org--make-block "bulleted_list_item" (list :rich_text rich) children)))))

(defun notion-bridge-org--elements-to-blocks (elements)
  "Convert a list of org ELEMENTS to Notion blocks."
  (cl-loop for el in (if (listp elements) elements nil)
           append (notion-bridge-org--element-to-blocks el)))

(defun notion-bridge-org--org-to-blocks (org-string)
  "Parse ORG-STRING and convert it to a list of Notion block plists."
  (with-temp-buffer
    (insert org-string)
    (let ((org-mode-hook nil))
      (org-mode))
    (notion-bridge-org--elements-to-blocks
     (org-element-contents (org-element-parse-buffer)))))

(defun notion-bridge-org--wire (block)
  "BLOCK as sent to the API: no local-only keys, nested children inline."
  (let* ((type (plist-get block :type))
         (key (intern (concat ":" type)))
         (data (copy-sequence (plist-get block key))))
    (when (plist-get data :rich_text)
      (setq data (plist-put data :rich_text
                            (vconcat (mapcar #'notion-bridge-org--wire-rich-text
                                             (plist-get data :rich_text))))))
    (when (plist-get data :cells)
      (setq data (plist-put data :cells
                            (vconcat (mapcar (lambda (cell)
                                               (vconcat (mapcar #'notion-bridge-org--wire-rich-text cell)))
                                             (plist-get data :cells))))))
    (when (and (equal type "table") (plist-get block :children))
      (setq data (plist-put data :children
                            (vconcat (mapcar #'notion-bridge-org--wire
                                             (plist-get block :children))))))
    (when (and (equal type "divider") (null data))
      (setq data (make-hash-table)))
    (list :type type key data)))

(defun notion-bridge-org--wire-rich-text (rt)
  (let ((r (copy-sequence rt)))
    (cl-remf r :plain_text)
    (cl-remf r :href)
    r))

(provide 'notion-bridge-org)

;;; notion-bridge-org.el ends here
