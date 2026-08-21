(defcustom org-writer-ignore-headlines-max-level 1 "Max level of headlines to keep when exporting prose.")

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
