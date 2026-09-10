;;; org-writer.el --- Utilities for using org-mode as a word processor, esp. for prose & novels
;;; Code:

(defgroup org-writer nil
  "Utility functions for writing prose with org-mode"
  :prefix "org-writer-"
  :group 'org)

(setq org-export-allow-bind-keywords t) ;;TODO: prompt the user about this 

(defcustom org-writer-headline-cutoff 2 "Max level of headlines to keep when exporting prose. Headlines at a greater depth will be treated as scene headers and replaced with a blank line in export")

;; usage: use #+BIND: org-export-filter-parse-tree-functions (org-writer-ignore-headlines-below)
;; and set org-writer-headline-cutoff

;; TODO: look into converting the hidden headlines into bookmarks instead!

(defun org-writer-ignore-headlines-below (data backend info)
  "Remove below a certain level retaining contents.
Each headline will be removed retaining its
contents and promoting them to the level of the
parent."
  (let ((max-level org-writer-headline-cutoff))
	(when max-level
      (org-element-map data 'headline
		(lambda (object)
		  (when (< max-level (org-element-property :level object))
			;; insert blank line
			(org-element-insert-before (headline-to-linebreak object backend) object)
			(mapc (lambda (el)
					;; insert children into parse tree above deleted headline
					(org-element-insert-before el object))
				  (org-element-contents object))
			(org-element-extract-element object)))
		info nil)))
  data)

(defun org-writer-odt-linebreak (&optional bookmark-name)
  (if bookmark-name
      (format "\n@@odt:<text:bookmark text:name=\"%s\"/>@@" bookmark-name)
    ""))

(defun org-writer-odt-bookmark-linebreak (bookmark-name backend begin end post-blank)
  "Create empty bookmark to insert in place of headline"
  (let ((value (format "\n<text:bookmark text:name=\"%s\"/>" bookmark-name)))
    (org-element-create
   'export-snippet
   (list :back-end "odt"
	 :value value
	 :begin begin
	 :end end
	 :post-blank post-blank))))

(defun headline-to-linebreak (obj &optional backend)
  (let* ((b (org-element-property :begin obj))
         (e (org-element-property :end obj))
         (cb (org-element-property :contents-begin obj))
         (ce (org-element-property :contents-end obj))
         (pb (org-element-property :post-blank obj))
         (pa (org-element-property :post-affiliated obj))
         break
         para)
    (setq break (cond ((org-export-derived-backend-p backend 'latex)
					   `(latex-fragment (:value "\\bigskip" :begin ,b :end ,e :post-blank ,pb)))
                      ((org-export-derived-backend-p backend 'odt)
                       (let ((title (org-element-property :title obj)))
			 (org-writer-odt-bookmark-linebreak title backend b e pb)))
		      ((org-export-derived-backend-p backend 'html)
		       " ")
                      (t
                       `(line-break (:begin ,b :end ,e :post-blank 0)))))
	
    (setq para `(paragraph (:begin ,b :end ,e :contents-begin ,cb :contents-end ,ce   :post-blank ,pb :post-affiliated ,pa)))
    (org-element-adopt-elements para "\n" break "\n")
    para))

;; note: replacing the headline with \nbsp produces the correct behavior wrt whitespace
;; However, removing the headline in the buffer gets rid of :noexport: functionality, which we want
;; \nbsp version (not working with latex): (setq break `(entity (:name nbsp :latex "~" :latex-math-p nil :html "&nbsp;" :ascii " " :latin1 " " :utf-8 " " :use-brackets-p nil :begin ,b :end ,e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing new projects and files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst org-writer-package-dir (file-name-directory load-file-name))
(defconst org-writer-setupfile-name "org-writer-setupfile.org")

(defconst org-writer-styles
  '(("novel" "org-writer-style-novel.ott" 2)
    ("novel-chapter" "org-writer-style-novel-chapter.ott" 1)))
;; eventually add short story, essay

(defun org-writer-get-style (style-name)
  (assoc style-name org-writer-styles))

(defun org-writer-style-name (style)
  (car style))

(defun org-writer-style-filename (style)
  (car (cdr style)))

(defun org-writer-style-destination-path (style project-dir)
  (file-name-concat project-dir (org-writer-style-filename style)))

(defun org-writer-style-ignore-headlines-max-level (style)
  (caddr style))
			       
(defun org-writer-choose-style-name ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-completion-help
    (completing-read "Choose a style template:" org-writer-styles nil t)))

(defun org-writer-current-dir-setupfile-path ()
  (file-name-concat default-directory org-writer-setupfile-name))

(defun org-writer-copy-setupfile (dest-dir)
  "Copy default setupfile to project-dir/org-writer-setupfile.org if not exists; else do nothing"
  (let ((setup-file-source (file-name-concat org-writer-package-dir "org-writer-setupfile.org"))
	(setup-file-dest (file-name-concat dest-dir org-writer-setupfile-name)))
    (unless (file-exists-p setup-file-dest)
      (copy-file setup-file-source setup-file-dest))))

(defun org-writer-copy-style-file (style-file-name dest-dir)
  "Copy default style file to project-dir/style-file-name if not exists; else do nothing"
  (let ((style-file-source (file-name-concat org-writer-package-dir style-file-name))
	(style-file-dest (file-name-concat dest-dir style-file-name)))
    (unless (file-exists-p style-file-dest)
      (copy-file style-file-source style-file-dest))))
  
(defun org-writer-initialize-project (project-dir)
  "Initialize new writing project in PROJECT-DIR; create setupfile"
  (interactive
   (let ((d (read-directory-name "Project directory:")))
     (list d)))
  (unless (file-exists-p project-dir)
    (make-directory project-dir nil))
  (org-writer-copy-setupfile project-dir))

(defun org-writer-initialize-file (file style-name)
  "Setup currently open file for writing export; write SETUPFILE setting to beginning"
  ;; todo: check for currently existing setupfile header, handle gracefully
  (interactive
   ;; get file name to edit
   (let* ((buffer-file (buffer-file-name))
	  (f (read-file-name "File path:" buffer-file buffer-file))
	  (project-dir (file-name-directory f))
	  (setupfile (file-name-concat project-dir org-writer-setupfile-name))
	  (style-name (org-writer-choose-style-name))
	  (style (org-writer-get-style style-name))
	  (style-file-name (org-writer-style-filename style))
	  (style-dest-path (org-writer-style-destination-path style project-dir)))
     ;; check if setupfile exists, offer to create
     (unless (file-exists-p setupfile)
       (let ((prompt (format "Setup file %s not found; create it?" setupfile)))
	 (when (y-or-n-p prompt)
	   (org-writer-copy-setupfile project-dir))))
     ;; check if style file exists, offer to create
     (unless (file-exists-p style-dest-path)
       (let ((prompt (format "Style file %s not found; create it?" style-dest-path)))
	 (when (y-or-n-p prompt)
	   (org-writer-copy-style-file style-file-name project-dir))))
     (list f style-name)))
  
  (find-file file)
  ;; assumption: we are now in a buffer editing the file, which may or may not be new
  (let* ((project-dir (file-name-directory file))
	 (setupfile (file-name-concat project-dir org-writer-setupfile-name))
	 (style (org-writer-get-style style-name))
	 (style-dest-path (org-writer-style-destination-path style project-dir))
	 (h (org-writer-style-ignore-headlines-max-level style))
	 (header-string (format "#+SETUPFILE: %s\n#+ODT_STYLES_FILE: %s\n#+BIND: org-writer-headline-cutoff %d\n" setupfile style-dest-path h)))
    ;; possible issue: other behaviors triggered by opening new file?
    (save-excursion
      (beginning-of-buffer)
      (insert header-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special blocks for ODT styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note: also tried advising the org-odt-special-block function
;; didn't work well because the contents of the block are already parsed; would need to
;; replace the style-name in all child paragraphs
;; Instead, this code overrides the processing of paragraphs for the odt backend
;; All it really changes is that it adds special blocks to the list of containers that change the style

(defun org-writer-special-block-style (special-block)
  (let ((type (org-element-property :type special-block)))
    (if (or (string= type "annotation")
	    (string= type "textbox"))
	'default
      type)))

(defun org-writer-paragraph-style (paragraph)
  "Get style based on environments the paragraph is in, including special blocks"
  (let* ((nearest-styling-ancestor
	  (org-element-lineage
             paragraph
             '(special-block center-block quote-block section))))
  (cl-case (org-element-type nearest-styling-ancestor)
    (center-block 'center)
    (quote-block 'quoted)
    (special-block (org-writer-special-block-style nearest-styling-ancestor)))))

(defun org-writer-format-paragraph (paragraph contents info default center quote)
  (let ((style (org-writer-paragraph-style paragraph)))
    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	    (cond
	     ((eq style 'quoted) quote)
	     ((eq style 'centered) center)
	     ((stringp style) style)
	     (t default))
	    ;; If PARAGRAPH is a leading paragraph in an item that has
	    ;; a checkbox, splice checkbox and paragraph contents
	    ;; together.
	    (concat (let ((parent (org-element-parent paragraph)))
		      (and (org-element-type-p parent 'item)
			   (not (org-export-get-previous-element paragraph info))
			   (org-odt--checkbox parent)))
		    contents))))

(advice-add 'org-odt--format-paragraph :override #'org-writer-format-paragraph)

(provide 'org-writer)
