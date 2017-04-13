;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun markup-html (doc)
  (funcall
   (>>>
    #'links
    #'endnotes
    #'htmlize
    #'entitle
    (stylize "style.css"))
   doc))

(defun htmlize (doc)
  "Wrap the :body we get from the markup parser in a proper HTML5 document with
a doctype and everything."
  `(:progn
    (:noescape "<!DOCTYPE html>")
    (:html
      (:head
       (:meta :charset "UTF-8"))
      ,doc)))

(defun entitle (doc)
  "Add a :TITLE element to :HEAD based on the contents of the first :H1"
  (let ((h1 (first (extract :h1 doc))))
    (when h1
      (funcall (rewriter :head (appending `((:title ,(just-text (cdr h1)))))) doc))))

(defun stylize (style &key inline)
  "Add an :LINK or :STYLE to :HEAD for an external or inline style element."
  (rewriter
   :head
   (appending (if inline
                  `((:style (:noescape ,style)))
                  `((:link :href ,style :rel "stylesheet"))))))
