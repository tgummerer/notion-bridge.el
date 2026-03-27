;;; notion-bridge-org.el --- Bidirectional org-mode/Notion conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: notion-bridge
;; Keywords: org, notion

;;; Commentary:

;; Converts between Notion block JSON (plists) and org-mode text.

;;; Code:

(require 'cl-lib)
(require 'org-element)

(defvar notion-bridge-org--page-map nil
  "Hash table mapping Notion page IDs to local org filenames.
Set by `notion-bridge-sync-pull-all' before conversion so that
page mentions and child_page blocks become file links.")

;;; ============================================================
;;; Notion -> Org
;;; ============================================================

(defun notion-bridge-org--rich-text-to-org (rich-text-array)
  "Convert Notion RICH-TEXT-ARRAY to org-mode formatted string.
Each element is a plist with :type, :text (:content, :link), :annotations."
  (mapconcat
   (lambda (rt)
     (let* ((rt-type (plist-get rt :type))
            result)
       (cond
        ;; Page mention -> org file link or notion URL
        ((and (equal rt-type "mention")
              (plist-get rt :mention))
         (let* ((mention (plist-get rt :mention))
                (mention-type (plist-get mention :type))
                (plain-text (or (plist-get rt :plain_text) "link")))
           (cond
            ((equal mention-type "page")
             (let* ((page-id (plist-get (plist-get mention :page) :id))
                    (local-file (and notion-bridge-org--page-map
                                     (gethash page-id notion-bridge-org--page-map))))
               (if local-file
                   (format "[[file:%s][%s]]" local-file plain-text)
                 (format "[[notion:%s][%s]]" page-id plain-text))))
            (t plain-text))))
        ;; Regular text with annotations
        (t
         (let* ((text-obj (plist-get rt :text))
                (content (or (plist-get text-obj :content) ""))
                (link (plist-get text-obj :link))
                (href (and link (plist-get link :url)))
                (annotations (plist-get rt :annotations))
                (bold (eq (plist-get annotations :bold) t))
                (italic (eq (plist-get annotations :italic) t))
                (strikethrough (eq (plist-get annotations :strikethrough) t))
                (code (eq (plist-get annotations :code) t))
                (underline (eq (plist-get annotations :underline) t)))
           (setq result content)
           ;; Apply annotations inside-out
           (when code
             (setq result (concat "~" result "~")))
           (when strikethrough
             (setq result (concat "+" result "+")))
           (when underline
             (setq result (concat "_" result "_")))
           (when italic
             (setq result (concat "/" result "/")))
           (when bold
             (setq result (concat "*" result "*")))
           ;; Convert Notion page URLs to local file links
           (when href
             (let ((page-id (notion-bridge-org--extract-page-id-from-url href)))
               (if (and page-id notion-bridge-org--page-map
                        (gethash page-id notion-bridge-org--page-map))
                   (setq result (format "[[file:%s][%s]]"
                                        (gethash page-id notion-bridge-org--page-map)
                                        result))
                 (setq result (format "[[%s][%s]]" href result)))))
           result)))))
   rich-text-array
   ""))

(defun notion-bridge-org--extract-page-id-from-url (url)
  "Extract a Notion page ID from URL if it is a Notion page link.
Returns the page ID string or nil."
  (when (and url (string-match
                  "notion\\.so/\\(?:[^/]+/\\)?\\([0-9a-f]\\{32\\}\\)\\b"
                  url))
    ;; Re-insert hyphens into the 32-char hex to form a UUID
    (let ((hex (match-string 1 url)))
      (format "%s-%s-%s-%s-%s"
              (substring hex 0 8)
              (substring hex 8 12)
              (substring hex 12 16)
              (substring hex 16 20)
              (substring hex 20)))))

(defun notion-bridge-org--block-to-org (block &optional depth)
  "Convert a single Notion BLOCK plist to org-mode text.
DEPTH controls indentation for nested items."
  (let* ((type (plist-get block :type))
         (depth (or depth 0))
         (indent (make-string (* 2 depth) ?\s))
         (type-data (plist-get block (intern (concat ":" type))))
         (rich-text (plist-get type-data :rich_text))
         (text (if rich-text (notion-bridge-org--rich-text-to-org rich-text) ""))
         (children (plist-get block :children))
         (children-str (if children
                           (notion-bridge-org--blocks-to-org children (1+ depth))
                         "")))
    (pcase type
      ("paragraph"
       (concat indent text (unless (string-empty-p children-str) (concat "\n" children-str))))
      ("heading_1"
       (concat "* " text))
      ("heading_2"
       (concat "** " text))
      ("heading_3"
       (concat "*** " text))
      ("bulleted_list_item"
       (concat indent "- " text
               (unless (string-empty-p children-str) (concat "\n" children-str))))
      ("numbered_list_item"
       (concat indent "1. " text
               (unless (string-empty-p children-str) (concat "\n" children-str))))
      ("to_do"
       (let ((checked (and (plist-get type-data :checked)
                           (not (eq (plist-get type-data :checked) :json-false)))))
         (concat indent "- [" (if checked "X" " ") "] " text
                 (unless (string-empty-p children-str) (concat "\n" children-str)))))
      ("code"
       (let ((language (or (plist-get type-data :language) "")))
         (concat "#+begin_src " language "\n"
                 text "\n"
                 "#+end_src")))
      ("quote"
       (concat "#+begin_quote\n" text "\n#+end_quote"))
      ("divider"
       "-----")
      ("toggle"
       (concat "#+begin_details " text "\n"
               (if (string-empty-p children-str) "" children-str)
               "\n#+end_details"))
      ("callout"
       (let ((icon (plist-get type-data :icon))
             (icon-str ""))
         (when icon
           (let ((emoji (plist-get icon :emoji)))
             (when emoji (setq icon-str (concat emoji " ")))))
         (concat "#+begin_quote\n" icon-str text "\n#+end_quote")))
      ("bookmark"
       (let ((url (plist-get type-data :url)))
         (if url (format "[[%s]]" url) "")))
      ("image"
       (let* ((image-data (or (plist-get type-data :file)
                              (plist-get type-data :external)))
              (url (and image-data (plist-get image-data :url))))
         (if url (format "[[%s]]" url) "")))
      ("file"
       (let* ((file-data (or (plist-get type-data :file)
                             (plist-get type-data :external)))
              (url (and file-data (plist-get file-data :url))))
         (if url (format "[[%s]]" url) "")))
      ("equation"
       (let ((expression (plist-get type-data :expression)))
         (if expression (format "\\(%s\\)" expression) "")))
      ("child_page"
       (let* ((title (plist-get type-data :title))
              (page-id (plist-get block :id))
              (local-file (and notion-bridge-org--page-map
                               (gethash page-id notion-bridge-org--page-map))))
         (if local-file
             (format "- [[file:%s][%s]]" local-file (or title "Untitled"))
           (format "- [[notion:%s][%s]]" page-id (or title "Untitled")))))
      ("child_database"
       (let ((title (plist-get type-data :title)))
         (format "- /Database:/ %s" (or title "Untitled"))))
      ("link_to_page"
       (let* ((link-type (plist-get type-data :type))
              (page-id (when (equal link-type "page_id")
                         (plist-get type-data :page_id)))
              (local-file (and page-id notion-bridge-org--page-map
                               (gethash page-id notion-bridge-org--page-map))))
         (cond
          (local-file (format "[[file:%s]]" local-file))
          (page-id (format "[[notion:%s]]" page-id))
          (t ""))))
      (_
       ;; Unknown block type - render rich text if available
       text))))

(defun notion-bridge-org--blocks-to-org (blocks &optional depth)
  "Convert a list of Notion BLOCKS to org-mode string.
DEPTH controls nesting level.  Inserts blank lines between
paragraphs and other non-list blocks for proper org formatting."
  (let ((depth (or depth 0))
        (result "")
        (prev-type nil))
    (dolist (block blocks)
      (let* ((type (plist-get block :type))
             (text (notion-bridge-org--block-to-org block depth))
             (list-types '("bulleted_list_item" "numbered_list_item" "to_do"))
             (prev-is-list (member prev-type list-types))
             (curr-is-list (member type list-types))
             ;; Use double newline between paragraphs and between
             ;; different block types, single newline within list runs
             (sep (cond
                   ((string-empty-p result) "")
                   ((and prev-is-list curr-is-list) "\n")
                   (t "\n\n"))))
        (setq result (concat result sep text))
        (setq prev-type type)))
    result))

;;; ============================================================
;;; Org -> Notion
;;; ============================================================

(defun notion-bridge-org--make-rich-text (content &optional annotations)
  "Create a single Notion rich_text element with CONTENT and ANNOTATIONS plist."
  (let ((ann (list :bold (if (plist-get annotations :bold) t :json-false)
                   :italic (if (plist-get annotations :italic) t :json-false)
                   :strikethrough (if (plist-get annotations :strikethrough) t :json-false)
                   :underline (if (plist-get annotations :underline) t :json-false)
                   :code (if (plist-get annotations :code) t :json-false))))
    (let ((rt (list :type "text"
                    :text (list :content content)
                    :annotations ann)))
      (when (plist-get annotations :href)
        (plist-put (plist-get rt :text)
                   :link (list :url (plist-get annotations :href))))
      rt)))

(defun notion-bridge-org--parse-inline-markup (text)
  "Parse org inline markup in TEXT and return a Notion rich_text array.
Handles *bold*, /italic/, +strikethrough+, ~code~, _underline_, and [[links]]."
  (if (or (null text) (string-empty-p text))
      (vector)
    (let ((parts '())
          (pos 0)
          (len (length text)))
      (while (< pos len)
        (let ((found nil))
          ;; Try to match org link [[href][text]] or [[href]]
          (when (and (not found)
                     (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]" text pos))
            (when (= (match-beginning 0) pos)
              (let ((href (match-string 1 text))
                    (link-text (or (match-string 2 text) (match-string 1 text))))
                (push (notion-bridge-org--make-rich-text link-text (list :href href)) parts)
                (setq pos (match-end 0))
                (setq found t))))
          ;; Try markup patterns: *bold*, /italic/, +strike+, ~code~, _underline_
          (dolist (spec '(("\\*\\([^*\n]+?\\)\\*" :bold)
                          ("/\\([^/\n]+?\\)/" :italic)
                          ("\\+\\([^+\n]+?\\)\\+" :strikethrough)
                          ("~\\([^~\n]+?\\)~" :code)
                          ("_\\([^_\n]+?\\)_" :underline))
                   )
            (unless found
              (let ((re (car spec))
                    (ann-key (cadr spec)))
                (when (string-match re text pos)
                  (when (= (match-beginning 0) pos)
                    (let ((inner (match-string 1 text)))
                      (push (notion-bridge-org--make-rich-text inner (list ann-key t)) parts)
                      (setq pos (match-end 0))
                      (setq found t)))))))
          ;; No markup matched at current position - consume plain text
          (unless found
            (let ((next pos))
              ;; Advance until we hit a potential markup character or end
              (setq next (1+ pos))
              (while (and (< next len)
                          (not (memq (aref text next) '(?* ?/ ?+ ?~ ?_ ?\[))))
                (setq next (1+ next)))
              (push (notion-bridge-org--make-rich-text (substring text pos next) nil) parts)
              (setq pos next)))))
      (vconcat (nreverse parts)))))

(defun notion-bridge-org--org-to-rich-text (text)
  "Convert org TEXT with inline markup to Notion rich_text array."
  (notion-bridge-org--parse-inline-markup text))

(defun notion-bridge-org--make-block (type type-data &optional children)
  "Create a Notion block plist with TYPE and TYPE-DATA.
Optionally attach CHILDREN."
  (let ((block (list :type type
                     (intern (concat ":" type)) type-data)))
    (when children
      (plist-put block :children children))
    block))

(defun notion-bridge-org--element-to-blocks (element)
  "Convert a single org ELEMENT to a list of Notion block plists.
Returns a list since some elements expand to multiple blocks."
  (let ((type (org-element-type element)))
    (pcase type
      ('headline
       (let* ((level (org-element-property :level element))
              (title (or (org-element-property :raw-value element) ""))
              (notion-type (pcase level
                             (1 "heading_1")
                             (2 "heading_2")
                             (_ "heading_3")))
              (rich-text (notion-bridge-org--org-to-rich-text title))
              (heading-block (notion-bridge-org--make-block
                              notion-type
                              (list :rich_text rich-text)))
              ;; Recurse into headline contents (sections, sub-headlines)
              (child-blocks (notion-bridge-org--elements-to-blocks
                             (org-element-contents element))))
         (cons heading-block child-blocks)))
      ('paragraph
       (let* ((contents (notion-bridge-org--element-contents-string element))
              ;; Split on newlines — each line becomes a separate Notion paragraph
              (lines (split-string contents "\n" t "\\s-*")))
         (mapcar (lambda (line)
                   (notion-bridge-org--make-block
                    "paragraph"
                    (list :rich_text (notion-bridge-org--org-to-rich-text line))))
                 lines)))
      ('plain-list
       (let* ((list-type (org-element-property :type element))
              (items (org-element-contents element))
              (blocks '()))
         (dolist (item (if (listp items) items nil))
           (when (eq (org-element-type item) 'item)
             (let* ((checkbox (org-element-property :checkbox item))
                    (contents (notion-bridge-org--item-text-only item))
                    (rich-text (notion-bridge-org--org-to-rich-text contents))
                    ;; Find nested sub-lists inside this item
                    (sub-blocks (notion-bridge-org--item-sub-blocks item))
                    (block
                     (cond
                      (checkbox
                       (notion-bridge-org--make-block
                        "to_do"
                        (list :rich_text rich-text
                              :checked (if (eq checkbox 'on) t :json-false))
                        (when sub-blocks (vconcat sub-blocks))))
                      ((eq list-type 'ordered)
                       (notion-bridge-org--make-block
                        "numbered_list_item"
                        (list :rich_text rich-text)
                        (when sub-blocks (vconcat sub-blocks))))
                      (t
                       (notion-bridge-org--make-block
                        "bulleted_list_item"
                        (list :rich_text rich-text)
                        (when sub-blocks (vconcat sub-blocks)))))))
               (push block blocks))))
         (nreverse blocks)))
      ('src-block
       (let ((language (or (org-element-property :language element) ""))
             (value (or (org-element-property :value element) "")))
         ;; Remove trailing newline from value
         (when (string-suffix-p "\n" value)
           (setq value (substring value 0 -1)))
         (list (notion-bridge-org--make-block
                "code"
                (list :rich_text (vector (notion-bridge-org--make-rich-text value nil))
                      :language language)))))
      ('quote-block
       (let* ((contents (notion-bridge-org--block-contents-string element))
              (rich-text (notion-bridge-org--org-to-rich-text contents)))
         (list (notion-bridge-org--make-block
                "quote"
                (list :rich_text rich-text)))))
      ('special-block
       (let ((block-type (org-element-property :type element)))
         (when (equal block-type "details")
           ;; #+begin_details TITLE\nCHILDREN\n#+end_details -> toggle block
           (let* ((params (or (org-element-property :parameters element) ""))
                  (title-rich-text (notion-bridge-org--org-to-rich-text params))
                  (child-contents (notion-bridge-org--block-contents-string element))
                  (child-blocks (when (and child-contents
                                          (not (string-empty-p child-contents)))
                                  (notion-bridge-org--org-to-blocks child-contents))))
             (list (notion-bridge-org--make-block
                    "toggle"
                    (list :rich_text title-rich-text)
                    (when child-blocks (vconcat child-blocks))))))))
      ('horizontal-rule
       (list (notion-bridge-org--make-block "divider" nil)))
      ('section
       ;; Sections contain the body elements under headlines; recurse into them
       (notion-bridge-org--elements-to-blocks (org-element-contents element)))
      ('org-data
       ;; Top-level document element; recurse into contents
       (notion-bridge-org--elements-to-blocks (org-element-contents element)))
      (_
       nil))))

(defun notion-bridge-org--element-contents-string (element)
  "Extract plain text contents of a paragraph ELEMENT."
  (let ((begin (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (if (and begin end)
        (string-trim (buffer-substring-no-properties begin end))
      "")))

(defun notion-bridge-org--item-text-only (item)
  "Extract just the text of ITEM, excluding sub-lists.
Looks for the first paragraph child and returns its text."
  (let ((result ""))
    (dolist (child (org-element-contents item))
      (when (eq (org-element-type child) 'paragraph)
        (let ((begin (org-element-property :contents-begin child))
              (end (org-element-property :contents-end child)))
          (when (and begin end)
            (setq result (string-trim (buffer-substring-no-properties begin end)))
            ;; Take only first line (rest could be continuation)
            (setq result (car (split-string result "\n" t)))))))
    (or result "")))

(defun notion-bridge-org--item-sub-blocks (item)
  "Extract nested sub-list blocks from ITEM.
Returns a list of Notion block plists for any sub-lists."
  (let ((sub-blocks nil))
    (dolist (child (org-element-contents item))
      (when (eq (org-element-type child) 'plain-list)
        (setq sub-blocks (append sub-blocks
                                 (notion-bridge-org--element-to-blocks child)))))
    sub-blocks))

(defun notion-bridge-org--block-contents-string (element)
  "Extract text content from a block ELEMENT (quote, etc.)."
  (let ((begin (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (if (and begin end)
        (string-trim (buffer-substring-no-properties begin end))
      "")))

(defun notion-bridge-org--elements-to-blocks (elements)
  "Convert a list of org ELEMENTS to Notion blocks."
  (let ((blocks '()))
    (dolist (el (if (listp elements) elements nil))
      (let ((converted (notion-bridge-org--element-to-blocks el)))
        (when converted
          (setq blocks (append blocks converted)))))
    blocks))

(defun notion-bridge-org--org-to-blocks (org-string)
  "Parse ORG-STRING and convert to a list of Notion block plists."
  (with-temp-buffer
    (insert org-string)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (top-elements (org-element-contents tree)))
      (notion-bridge-org--elements-to-blocks top-elements))))

(provide 'notion-bridge-org)

;;; notion-bridge-org.el ends here
