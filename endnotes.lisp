;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun endnotes (doc)
  "Rewrite a doc with :note elements in it so they became endnotes."
  (funcall
   (>>>
    (rewriter :note (numberer))
    (lambda (x) `(,@x (:notes ,@(extract :note x))))
    (rewriter :notes (rewriter :note (>>> #'endnote-backlinker #'divver)))
    (rewriter :notes #'divver)
    (rewriter :body (rewriter :note #'endnote-marker)))
   doc))

;;; Helper functions

(defun >>> (&rest fn) (apply #'compose (reverse fn)))

(defun rewriter (tag fn)
  "A function that given a tree finds every instance of TAG'd elements and
replaces them with teh result of FN."
  (labels ((walk (tree)
             (if (consp tree)
                 (mapcar #'walk (if (eql (car tree) tag) (funcall fn tree) tree))
                 tree)))
    #'walk))

(defun numberer ()
  "Insert an element with a number that increases each time we are called."
  (let ((n 0))
    #'(lambda (tree)
        (destructuring-bind (tag &rest body) tree
          `(,tag ,(incf n) ,@body)))))

(defun divver (tree)
  "Replace tree with a :DIV with a class attribute from the original tag."
  `((:div :class ,(string-downcase (first tree))) ,@(rest tree)))

(defun fragment (x)
  "Make a framgment HREF value."
  (format nil "#~a" x))

(defun note-id (n)
  "The id value for the actual footnote number N."
  (format nil "note_~d" n))

(defun marker-id (n)
  "The id value for the marker for footnote number N."
  (format nil "marker_~d" n))

(defun endnote-marker (note)
  "Make a :NOTE element into its endnote marker, linking to the note and with an
ID to allow linking back."
  (let ((n (second note)))
    `((:a
       :id ,(marker-id n)
       :href ,(fragment (note-id n))
       :class "marker")
      ,n)))

(defun endnote-backlinker (note)
  "Convert the number in a :NOTE element into the target for the endnote marker
and a link back to the marker."
  (destructuring-bind (tag n (e1 &rest e1-body) &rest body) note
    `(,tag
      (,e1
       ((:a
         :id ,(note-id n)
         :href ,(fragment (marker-id n))
         :class "marker")
        ,n) " "
       ,@e1-body)
      ,@body)))

(defun extract (tag sexp)
  "Extract all the elements with a given TAG in depth-first order."
  (when (consp sexp)
    (if (eql (car sexp) tag)
        (list sexp)
        (mapcan #'(lambda (x) (extract tag x)) sexp))))
