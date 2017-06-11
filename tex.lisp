;;
;; Copyright (c) 2005, 2017, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defvar *tex-preamble* nil)
(defvar *tex-postamble* nil)

;;;; Render Markup data into TeX

(defun generate-tex (file)
  (with-output-to-file (out (ensure-directories-exist (tex-filename file)))
    (when *tex-preamble* (write-string *tex-preamble* out))
    (emit-tex-element out (markup (file-text file)))
    (when *tex-postamble* (write-string *tex-postamble* out))))

(defun tex-filename (file)
  (let ((*default-pathname-defaults* (parent-directory file)))
    (make-pathname :type "tex" :defaults (merge-pathnames "tex/" file))))

(defmacro nyi (form)
  `(error "Not implemented yet: ~s" ',form))

(defun emit-tex-element (stream element)
  (let ((tag (first element)))
    (when (consp tag)
      (when (rest tag) (warn "Ignoring attributes: ~a of tag ~a" (rest tag) (first tag)))
      (setf tag (first tag))
      (setf element (cons tag (rest element)))
      (when (consp tag) (warn "Tag still a cons: ~a" tag)))
    (cond
      ((eql tag :modeline))
      ((eql tag :body)
       (dolist (e (rest element)) (emit-tex-element stream e)))
      ((eql tag :h1))
      ((eql tag :h2)
       (emit-tagged-tex-group stream `(:chapter ,@(rest element))))
      #+(or)((member tag '(:h1 :h2 :h3 :h4 :h5 :h6 :h7 :h8 :h9))
       (emit-tex-header stream element))
      ((member tag '(:bullets :numbered))
       (nyi (emit-tex-list stream element)))
      ((eql tag :math)
       (emit-math-mode stream (rest element)))
      ((member tag '(:p :blockquote))
       (emit-tex-paragraph stream element))
      ((member tag '(:note :comment))
       (nyi (emit-tex-sub-document stream element)))
      ((member tag '(:i :b :code))
       (emit-tex-span stream element))
      ((eql tag :blank)
       (format stream "~&\\vskip 1em~&\\noindent"))
      (t
       (error "Don't know how to render tag ~s" tag)))))

(defun emit-tex-header (stream element)
  (let ((level (parse-integer (symbol-name (first element)) :start 1)))
    (format stream "~&{\\h~r " level)
    (dolist (e (rest element)) (emit-tex-text stream e))
    (format stream "}~2%")))

(defun emit-tex-paragraph (stream paragraph)
  (case (first paragraph)
    (:p (emit-implicit-tex-paragraph stream paragraph))
    (t  (emit-regular-tex-paragraph stream paragraph))))

(defun emit-implicit-tex-paragraph (stream paragraph)
  (fresh-line stream)
  (dolist (e (rest paragraph)) (emit-tex-text stream e))
  (fresh-line stream)
  (terpri stream))

(defun emit-regular-tex-paragraph (stream paragraph)
  (fresh-line stream)
  (emit-tagged-tex-group stream paragraph)
  (fresh-line stream)
  (terpri stream))

#+(or)(defun emit-tex-text (stream text &optional (prefix ""))
  (cond
    ((stringp text) (format stream "~a" (fix-tex-characters text)))
    ((numberp text) (format stream "~a" text))
    ((and (characterp text) (char= #\Newline text)) (format stream "~%~a" prefix))
    ((and (characterp text) (member text '(#\\ #\{ #\}) :test #'char=)) (format stream "\\~c" text))
    (t (emit-tex-element stream text))))

(defun emit-tex-text (stream text)
  (etypecase text
    (string (format stream "~a" (fix-tex-characters text)))
    (cons (emit-tex-element stream text))))

(defvar *use-tex-characters* t)

(defun fix-tex-characters (text)
  (flet ((fix (regexp replacement)
           (setf text (cl-ppcre:regex-replace-all regexp text replacement))))
    (let ((ldq (cl-ppcre:create-scanner (code-char 8220)))
          (rdq (cl-ppcre:create-scanner (code-char 8221)))
          (lsq (cl-ppcre:create-scanner (code-char 8216)))
          (rsq (cl-ppcre:create-scanner (code-char 8217)))
          (mdash (cl-ppcre:create-scanner (format nil "\\s*~c\\s*" (code-char 8212))))
          (ndash (cl-ppcre:create-scanner (code-char 8211)))
          (underbar (cl-ppcre:create-scanner #\_))
          (dollar (cl-ppcre:create-scanner #\$))
          (pound (cl-ppcre:create-scanner #\#))
          (percent (cl-ppcre:create-scanner #\%))
          (ampersand (cl-ppcre:create-scanner #\&))
          (minus-minus (cl-ppcre:create-scanner "--")))

      (fix minus-minus "-{-}") ; want a literal -- not an ndash
      (when *use-tex-characters*
        (fix ldq "``")
        (fix rdq "''")
        (fix lsq "`")
        (fix rsq "'")
        (fix mdash "---")
        (fix ndash "--"))
      (fix underbar "\\_")
      (fix dollar "\\$")
      (fix pound "\\#")
      (fix percent "\\%")
      (fix ampersand "\\\\&")
      text)))

(defun emit-tex-span (stream span)
  (emit-tagged-tex-group stream span))

(defun emit-tagged-tex-group (stream group)
  (destructuring-bind (tag &body body) group
    (when (consp tag)
      (when (rest tag) (warn "Ignoring attributes: ~a of tag ~a" (rest tag) (first tag)))
      (setf tag (first tag))
      (when (consp tag) (warn "Tag still a cons: ~a" tag)))
    (format stream "\\~(~a~){"  tag)
    (dolist (e body) (emit-tex-text stream e))
    (format stream "}")))

(defun emit-math-mode (stream body)
  (format stream "$")
  (dolist (e body) (emit-tex-text stream e))
  (format stream "$"))
