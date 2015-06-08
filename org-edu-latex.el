;; Org-edu-LaTeX - LaTeX-based exporter for educational materials

(require 'ox-latex)
(require 'org-one-to-many)
(require 'subr-x)

(org-export-define-derived-backend 'edu-latex 'latex
  :translate-alist '((template . org-edu-latex-template)
		     (plain-list . org-edu-latex-plain-list)
		     (item . org-edu-latex-item)
		     (special-block . org-edu-latex-special-block)
		     (underline . org-edu-latex-underline))
  :options-alist '((:teacher-version nil "teacher" nil t)
		   (:checkbox-empty "CHECKBOX_EMPTY" nil "\\emptymct ")
		   (:checkbox-no "CHECKBOX_NO" nil "\\wrongmct ")
		   (:checkbox-yes "CHECKBOX_YES" nil "\\rightmct ")
		   (:radio-empty "RADIO_EMPTY" nil "\\emptysct ")
		   (:radio-no "RADIO_NO" nil "\\wringsct ")
		   (:radio-yes "RADIO_YES" nil "\\rightsct "))
  :menu-entry '(?e "Export to Edu-LaTeX"
		   ((?E "As LaTeX buffer" org-edu-latex-export-as-edu-latex)
		    (?e "As LaTeX file" org-edu-latex-export-to-edu-latex)
		    (?o "As LaTeX file and open"
			(lambda (a s v b)
			  (if a (org-edu-latex-export-to-edu-latex t s v b)
			    (org-open-file (org-edu-latex-export-to-edu-latex nil s v b))))))))

(defun org-edu-latex-export-as-edu-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as Edu-LaTeX.  The function is modeled
after org-latex-export-as-latex."
  (interactive)
  (org-export-to-buffer 'edu-latex "*Org Edu-LaTeX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (latex-mode))))

(defun org-edu-latex-export-to-edu-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Edu-LaTeX file.  The function is
modeled after org-latex-export-as-latex."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'edu-latex outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-edu-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((org-latex-packages-alist
	 (append org-latex-packages-alist '(("teacher" "org-edu" t)))))
    (org-latex-template contents info)))

(defun org-edu-latex-plain-list (plain-list contents info)
  (cond
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "mct")
    (org-edu-latex-mct-test plain-list contents info))
   ((member (org-export-read-attribute :attr_edu plain-list :test) '("sct" "select"))
    (org-edu-latex-mct-test plain-list contents info t))
   (t (org-latex-plain-list plain-list contents info))))

(defun org-edu-latex-mct-test (plain-list contents info &optional sct)
  (let* ((type (org-element-property :type plain-list))
	 (attr (org-export-read-attribute :attr_latex plain-list))
	 (latex-type (let ((env (plist-get attr :environment)))
		       (cond (env (format "%s" env))
			     ((eq type 'ordered) "enumerate")
			     ((eq type 'descriptive) "description")
			     (t "itemize"))))
	 (box (if sct (plist-get info :radio-empty)
		(plist-get info :checkbox-empty))))
    (org-latex--wrap-label
     plain-list
     (format "\\begin{itemize}[label={%s},%s]\n%s\\end{itemize}"
	     box
	     (or (plist-get attr :options) "")
	     contents)
     info)))

(defun org-edu-latex-item (item contents info)
  ;; (org-latex-item (item contents info)))
  (cond
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "mct")
    (org-edu-latex-mct-item item contents info))
   ((member
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) '("sct" "select"))
    (org-edu-latex-mct-item item contents info t))
   (t (org-latex-item item contents info))))

(defun org-edu-latex-mct-item (item contents info &optional sct)
  (let* ((state (org-element-property :checkbox item)))
    (format
     "\\item%s %s\n"
     (if (and (eql state 'on)
	      (plist-get info :teacher-version))
	 (if sct (concat "[" (plist-get info :radio-yes) "]")
	   (concat "[" (plist-get info :checkbox-yes) "]"))
       "")
     contents)))

;; ;; TODO
;; (defun org-edu-latex-select-item (item contents info)
;;   (let* ((state (org-element-property :checkbox item))
;; 	 (name (number-to-string (org-export-get-ordinal
;; 				  (org-element-property :parent item)
;; 				  info
;; 				  '(plain-list))))
;; 	 (id (concat name (format "%s" (org-export-get-ordinal item info)))))
;;     (format "<option value=\"%s\">%s</option>"
;; 	    (if (string= state "on") 1 0)
;; 	    (if (string-match "\n$" contents)
;; 		(substring contents 0 -1)
;; 	      contents))))

(defun org-edu-latex-check-for-block-type (element block-type)
  "Check whether ELEMENT is inside a BLOCK-TYPE (which is a
string) special block."
  (let ((parent (org-export-get-parent element)))
    (cond ((and (eq (car parent) 'special-block)
		(string= (org-element-property :type parent) block-type))
	   t)
	  (parent
	   (org-edu-latex-check-for-block-type parent block-type)))))

(defun org-edu-latex-underline (underline contents info)
  "This (ab)uses underline to make cloze tests.  If the underline
is anywhere within a CLOZE special block, it will be turned into
a cloze."
  (if (org-edu-latex-check-for-block-type underline "CLOZE")
      (org-edu-latex-cloze underline contents info)
    (org-latex-underline underline contents info)))

(defun org-edu-latex-cloze (cloze contents info)
  "Transcode a gap in a cloze test to LaTeX."
  (format "\\makegap{%s}" contents))

(defun org-edu-latex-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  Delete the underscores from the
name of the environment."
  (let ((type (downcase
	       (replace-regexp-in-string
		"_" "" (org-element-property :type special-block))))
	(opt (org-export-read-attribute :attr_latex special-block :options)))
    (concat (format "\\begin{%s}%s\n" type (or opt ""))
	    (org-latex--caption/label-string special-block info)
	    contents
	    (format "\\end{%s}" type))))

;; (defun org-edu-latex-special-block (special-block contents info)
;;   "Transcode a special block into a suitable environment."
;;   (if (string= (downcase (org-element-property :type special-block)) "hidden")
;;       (let* ((keywords-string (save-excursion
;; 				(buffer-substring-no-properties
;; 				 (progn (goto-char (org-element-property :post-affiliated special-block))
;; 					(let ((case-fold-search t)) (re-search-forward "#\\+begin_hidden *"))
;; 					(point))
;; 				 (progn (end-of-line)
;; 					(point)))))
;; 	     (keywords-alist (keywords-string-to-alist keywords-string))
;; 	     (contents (or contents ""))
;; 	     (attributes (org-export-read-attribute :attr_html special-block))
;; 	     (attributes (plist-put attributes :class "hidden"))
;; 	     (show-name (assoc "show" keywords-alist))
;; 	     (hide-name (assoc "hide" keywords-alist)))
;; 	(if show-name
;; 	    (setq attributes (plist-put attributes :data-show-text (cdr show-name))))
;; 	(if hide-name
;; 	    (setq attributes (plist-put attributes :data-hide-text (cdr hide-name))))
;; 	(setq attributes (org-html--make-attribute-string attributes))
;; 	(format "\\begin{hidden}\n%s\\end{hidden}" contents))
;;     (org-element-put-property special-block :type
;; 			      (replace-regexp-in-string "_" ""
;; 							(org-element-property :type special-block)))
;;     (org-latex-special-block special-block contents info)))

;; ;; TODO (probably not needed at all!)
;; (defun keywords-string-to-alist (keywords-string)
;;   "Convert a string with keywords (looking like in a plist) into an alist
;; \(with keys being strings, without leading colons).  For instance,
;; this: \":foo bar :baz qux \" gets converted into `((\"baz\"
;; . \"qux\") (\"foo\" . \"bar\"))'.  Notice that the trailing space
;; gets trimmed."
;;   (let ((start 0) keywords-alist)
;;     (while (string-match ":\\([a-z]+\\) \\([^:]*\\)\\( \\|$\\)" keywords-string start)
;;       (push (cons (match-string 1 keywords-string)
;; 		  (save-match-data
;; 		    (string-trim-right (match-string 2 keywords-string))))
;; 	    keywords-alist)
;;       (setq start (match-end 2)))
;;     keywords-alist))

(provide 'org-edu-latex)
