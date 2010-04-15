(in-package :cl-css)

;;;; begin casing tags
(defun name-to-class (tag)
  (upcase-after-dash (string-downcase (format nil "~A" tag))))

(defun upcase-word (word)
	   (apply #'concatenate 'string
		  (list (char-upcase (elt word 0)))
		  (loop for char in (rest (concatenate 'list word)) collect (list char))))

(defun upcase-after-dash (word)
  (let ((split (cl-ppcre:split "-" word)))
    (apply #'concatenate 'string (first split)
	   (loop for part in (rest split)
	      collect (upcase-word part)))))
;;; end casing tags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; definition of the expansions

(defmacro css (&rest property-values)
  `(print-css-key-vals ,@(loop for (p v) on property-values by #'cddr
			    append `((property css ,p)
				     (value ,@ v)))))

(defmacro css-block (&body blocks)
  "Each block-list in the body consists of a selector and a property block.  The former being a function or macro in the function space, the latter being something for css-prop."
  `(print-css-block ,@(loop for b in blocks
			 append `((selector ,@(first b))
				  (css ,@(rest b))))))

(defun print-css-block (&rest definitions)
  (format nil "~{~A { ~A~%~T}~^~%~}" definitions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; definition of values and spaces
(create-space selector)
(create-space property)
(create-space value)

(defun* (selector id) (id)
  (format nil "#~A" (name-to-class id)))
(defun* (selector class) (class)
  (format nil ".~A" (name-to-class class)))
(defun* (selector tag) (tag)
  (name-to-class tag))
(defun* (selector string) (string)
  string)

(defun print-css-key-vals (&rest key-vals)
  (format nil "~{~A: ~A~^;~&~}" key-vals))

(defun* (property css) (keyword)
  (string-downcase (symbol-name keyword)))

(defmacro* (value url) (url)
  `(value fun 'url ,url))
(defun* (value px) (px)
  (if (eql px 0)
      "0"
      (format nil "~Apx" px)))
(defun* (value em) (em)
  (if (eql em 0)
      "0"
      (format nil "~Aem" em)))
(defun* (value ex) (ex)
  (if (eql ex 0)
      "0"
      (format nil "~Aex" ex)))
(defun* (value %) (value)
  (if (eql value 0)
      "0"
      (format nil "~A%" value)))

(defmacro* (value fun) (name &rest args)
  `(format nil "~A(~{~A~^,~})" ,name ,args))
(defmacro* (value key) (keyword)
  `(string-downcase (symbol-name ',keyword)))
(defun* (value string) (string)
  (format nil "\"~A\"" string))
(defmacro* (value color) (keyword)
  `(format nil "#~A" ,keyword))

(defmacro* (value multi) (&rest args)
  `(format nil "~{~A~^ ~}" (list ,@(mapcar (lambda (arg) `(value ,@arg)) args))))
(defmacro* (value list) (&rest args)
  `(format nil "~{~A~^,~}" (list ,@(mapcar (lambda (arg) `(value ,@arg)) args))))
