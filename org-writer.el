(defcustom org-writer-ignore-headlines-max-level 2 "Max level of headlines to keep when exporting prose.")

;; usage: use #+BIND: org-export-filter-parse-tree-functions (org-writer-ignore-headlines-below)
;; and set org-writer-ignore-headlines-max-level

(defun org-writer-ignore-headlines-below (data backend info)
  "Remove below a certain level retaining contents.
Each headline will be removed retaining its
contents and promoting them to the level of the
parent."
  (message "data: %s" data)
  (let ((max-level org-writer-ignore-headlines-max-level))
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
                       "")
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

;; Initializing new projects and files

(defconst org-writer-package-dir (file-name-directory load-file-name))
(defconst org-writer-setupfile-name "org-writer-setupfile.org")
(defconst org-writer-style-dest-file-name "org-writer-style.ott")

(defun org-writer-current-dir-setupfile-path ()
  (file-name-concat default-directory org-writer-setupfile-name))

(defun org-writer-copy-setupfile (dest-dir)
  "Copy default setupfile to project-dir/org-writer-setupfile.org if not exists; else do nothing"
  (let ((setup-file-source (file-name-concat org-writer-package-dir "org-writer-setupfile.org"))
	(setup-file-dest (file-name-concat dest-dir org-writer-setupfile-name)))
    (message "Copying setupfile from %s to %s" setup-file-source setup-file-dest)
    (unless (file-exists-p setup-file-dest)
      (copy-file setup-file-source setup-file-dest))))

(defun org-writer-copy-style-file (dest-dir)
  "Copy default style file to project-dir/org-writer-style-dest-file-name if not exists; else do nothing"
  (let ((style-file-source (file-name-concat org-writer-package-dir "org-writer-style-novel.ott"))
	(style-file-dest (file-name-concat dest-dir org-writer-style-dest-file-name)))
    (message "Copying style file from %s to %s" style-file-source style-file-dest)
    (unless (file-exists-p style-file-dest)
      (copy-file style-file-source style-file-dest))))
  
(defun org-writer-initialize-project (project-dir)
  "Initialize new writing project in PROJECT-DIR; create setupfile and odt style template"
  ;; TODO: add support for multiple style files for, e.g., novels vs short stories
  (interactive
   (let ((d (read-directory-name "Project directory:")))
     (list d)))
  (unless (file-exists-p project-dir)
    (make-directory project-dir nil))
  (org-writer-copy-setupfile project-dir)
  (org-writer-copy-style-file project-dir))

(defun org-writer-initialize-file (file)
  "Setup currently open file for writing export; write SETUPFILE setting to beginning"
  ;; todo: check for currently existing setupfile header, handle gracefully
  (interactive
   ;; get file name to edit
   (let* ((buffer-file (buffer-file-name))
	  (f (read-file-name "File path:" buffer-file buffer-file))
	  (project-dir (file-name-directory f))
	  (setupfile (file-name-concat project-dir org-writer-setupfile-name))
	  (style-file (file-name-concat project-dir org-writer-style-dest-file-name)))
     ;; check if setupfile exists, offer to create
     (unless (file-exists-p setupfile)
       (let ((prompt (format "Setup file %s not found; create it?" setupfile)))
	 (when (y-or-n-p prompt)
	   (org-writer-copy-setupfile project-dir))))
     ;; check if style file exists, offer to create
     (unless (file-exists-p style-file)
       (let ((prompt (format "Style file %s not found; create it?" style-file)))
	 (when (y-or-n-p prompt)
	   (org-writer-copy-style-file project-dir)))
     (list f)))
  
  (find-file file)
  ;; assumption: we are now in a buffer editing the file, which may or may not be new
  (let* ((project-dir (file-name-directory file))
	(setupfile (file-name-concat project-dir org-writer-setupfile-name))
	(header-string (format "#+SETUPFILE: %s\n" setupfile)))
    ;; possible issue: other behaviors triggered by opening new file?
    (save-excursion
      (beginning-of-buffer)
      (insert header-string))))
