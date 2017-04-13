;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun links (doc)
  "Rewrite the doc so :link elements are expanded into :a tags with the appropriate link."
  (let ((linkdefs (get-linkdefs doc)))
    (funcall
     (>>>
      (rewriter :link (linker linkdefs))
      (deleter :link_def))
     doc)))


;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun deleter (tag)
  (labels ((walk (tree)
             (if (consp tree)
                 (unless (eql (car tree) tag)
                   (list (mapcan #'walk tree)))
                 (list tree))))
    (compose #'car #'walk)))

(defun get-linkdefs (doc)
  "Extract the link names and urls."
  (loop with h = (make-hash-table :test 'equalp)
     for (nil (nil link) (nil url)) in (extract :link_def doc)
     do (setf (gethash link h) url)
     finally (return h)))

(defun linker (links)
  "Rewrite a link tag into anchor."
  #'(lambda (x)
      `((:a :href ,(gethash (link-key x) links)) ,@(link-contents x))))

(defun link-key (link)
  "The key extracted from a :link, either the explicit :key value or
the text of the :link stripped of any markup."
  (just-text (or (find-if #'(lambda (x) (and (consp x) (eql (car x) :key))) link) link)))

(defun link-contents (link)
  "Contents of the link with any :key removed."
  (rest (funcall (deleter :key) link)))

(defun just-text (sexp)
  "Textual content of the element as a single string with all markup
removed."
  (with-output-to-string (s)
    (labels ((walk (x)
               (typecase x
                 (string (write-string x s))
                 (cons (mapcar #'walk x)))))
      (walk sexp))))
