;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

;; Functions for manipulating trees prior to rendering.

(in-package :com.gigamonkeys.yamp)

(defun >>> (&rest fn)
  "Reverse order version of COMPOSE."
  (apply #'compose (reverse fn)))

(defun extract (tag sexp)
  "Extract all the elements with a given TAG in depth-first order."
  (when (consp sexp)
    (if (eql (car sexp) tag)
        (list sexp)
        (mapcan #'(lambda (x) (extract tag x)) sexp))))

(defun has (tag sexp)
  "Are there any TAG elements in SEXP?"
  (when (consp sexp)
    (or (eql (car sexp) tag)
        (has tag (first sexp))
        (has tag (rest sexp)))))

(defun rewriter (tag fn)
  "A function that given a tree finds every instance of TAG'd elements and
replaces them with the result of FN."
  (labels ((walk (tree)
             (if (consp tree)
                 (mapcar #'walk (if (eql (car tree) tag) (funcall fn tree) tree))
                 tree)))
    #'walk))

(defun rewriter-if (p fn)
  (labels ((walk (tree)
             (if (consp tree)
                 (mapcar #'walk (if (funcall p tree) (funcall fn tree) tree))
                 tree)))
    #'walk))

(defun retagger (old new)
  "Retag all trees tagged with OLD with NEW"
  (labels ((walk (tree)
             (if (consp tree)
                 (if (eql (car tree) old)
                     `(,new ,@(mapcar #'walk (cdr tree)))
                     (mapcar #'walk tree))
                 tree)))
    #'walk))

(defun deleter (tag)
  "A function that deletes all subtrees tagged with TAG."
  (labels ((walk (tree)
             (if (consp tree)
                 (unless (eql (car tree) tag)
                   (list (mapcan #'walk tree)))
                 (list tree))))
    (compose #'car #'walk)))

(defun appending (contents)
  "A function that appends CONTENTS at the end of an element."
  #'(lambda (e) `(,@e ,@contents)))

(defun replacing-with (contents)
  #'(lambda (e) (declare (ignore e)) contents))

(defun numberer ()
  "Insert an element with a number that increases each time we are called."
  (let ((n 0))
    #'(lambda (tree)
        (destructuring-bind (tag &rest body) tree
          `(,tag ,(incf n) ,@body)))))

(defun divver (tree)
  "Replace tree with a :DIV with a class attribute from the original tag."
  `((:div :class ,(string-downcase (first tree))) ,@(rest tree)))

(defun spanner (tree)
  "Replace tree with a :span with a class attribute from the original tag."
  `((:span :class ,(string-downcase (first tree))) ,@(rest tree)))
