;;; notion-bridge-cache.el --- Metadata cache for notion-bridge sync -*- lexical-binding: t; -*-

;; Store sync metadata as readable elisp sexps in sidecar files.
;; Cache directory: ~/.emacs.d/.cache/notion-bridge/

(require 'cl-lib)

(defconst notion-bridge-cache-directory
  (expand-file-name ".cache/notion-bridge/" user-emacs-directory)
  "Directory for notion-bridge cache files.")

(defun notion-bridge-cache--file-path (org-file)
  "Derive cache file path from ORG-FILE path.
Uses md5 of the absolute path as the filename."
  (let ((hash (md5 (expand-file-name org-file))))
    (expand-file-name (concat hash ".el") notion-bridge-cache-directory)))

(defun notion-bridge-cache-load (org-file)
  "Load and return cache plist for ORG-FILE.
Return nil if missing, corrupt, or not a valid plist."
  (let ((cache-file (notion-bridge-cache--file-path org-file)))
    (when (file-exists-p cache-file)
      (condition-case nil
          (let ((data (with-temp-buffer
                        (insert-file-contents cache-file)
                        (read (current-buffer)))))
            (if (listp data)
                data
              nil))
        (error nil)))))

(defun notion-bridge-cache-save (org-file cache)
  "Persist CACHE plist for ORG-FILE, creating directories as needed."
  (let ((cache-file (notion-bridge-cache--file-path org-file)))
    (make-directory (file-name-directory cache-file) t)
    (with-temp-file cache-file
      (let ((print-level nil)
            (print-length nil))
        (prin1 cache (current-buffer))
        (insert "\n")))))

(defun notion-bridge-cache-get-block-id (cache index)
  "Lookup block ID by position INDEX in CACHE.
Return the notion-id string or nil."
  (let ((blocks (plist-get cache :blocks)))
    (when (and blocks (< index (length blocks)))
      (plist-get (nth index blocks) :notion-id))))

(defun notion-bridge-cache-update-block (cache index block-id hash type)
  "Update block mapping at INDEX in CACHE.
Set BLOCK-ID, HASH, and TYPE.  Return the modified cache."
  (let* ((blocks (plist-get cache :blocks))
         (block (list :org-hash hash :notion-id block-id :type type :position index)))
    ;; Extend blocks list if needed
    (while (<= (length blocks) index)
      (setq blocks (append blocks (list nil))))
    (setcar (nthcdr index blocks) block)
    (plist-put cache :blocks blocks)))

(defun notion-bridge-cache-set-page-info (cache page-id last-edited)
  "Update page metadata in CACHE.
Set PAGE-ID and LAST-EDITED timestamp.  Return the modified cache."
  (setq cache (plist-put cache :page-id page-id))
  (setq cache (plist-put cache :last-edited last-edited))
  (plist-put cache :last-synced (format-time-string "%FT%T%z")))

(defun notion-bridge-cache-compute-hash (text)
  "Compute sha256 hash of TEXT."
  (secure-hash 'sha256 text))

(defun notion-bridge-cache-set-comments (cache comments)
  "Store COMMENTS in CACHE.  Return modified cache.
COMMENTS is a list of plists, each with :block-id, :discussion-id,
:author, :date, :text, and :parent-type."
  (plist-put cache :comments comments))

(defun notion-bridge-cache-get-comments (cache)
  "Get stored comments from CACHE."
  (plist-get cache :comments))

(provide 'notion-bridge-cache)
;;; notion-bridge-cache.el ends here
