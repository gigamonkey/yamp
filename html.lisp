;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun generate-html (file)
  "Generate HTML for the markup file in the same location except with an .html
extension."
  (with-output-to-file (out (make-pathname :type "html" :defaults file))
    (monkeylib-text-output:with-text-output (out)
      (monkeylib-html:emit-html
       (markup-html
        (markup (file-text file))))))
  t)

(defun markup-html (doc &key title (style "style.css") script)
  (funcall
   (>>>
    #'links
    #'endnotes
    (rewriter :img #'image)
    (rewriter-if #'formatted-code-section #'formatted-code)
    #'htmlize
    (entitle title)
    (stylize style)
    (enscript script)
    (rewriter :section-marker #'divver)
    (rewriter :section #'section-to-div)
    )
   doc))

(defun links (doc)
  "Rewrite the doc so :link elements are expanded into :a tags with the
appropriate link."
  (let ((linkdefs (get-linkdefs doc)))
    (funcall
     (>>> (deleter :link_def) (rewriter :link (linker linkdefs)))
     doc)))

(defun endnotes (doc)
  "Rewrite a doc with :note elements converted to endnotes."
  (if (extract :note doc)
      (funcall
       (>>>
        (rewriter :note (numberer))
        (lambda (x) `(,@x (:notes ,@(extract :note x))))
        (rewriter :notes (rewriter :note (>>> #'endnote-backlinker #'divver)))
        (rewriter :notes #'divver)
        (rewriter :body (rewriter :note #'endnote-marker)))
       doc)
      doc))

(defun image (img)
  `(:img :src ,@(rest img)))

(defun htmlize (doc)
  "Wrap the :body we get from the markup parser in a proper HTML5 document with
a doctype and proper charset."
  `(:progn
    (:noescape "<!DOCTYPE html>")
    (:html
      (:head
       (:meta :charset "UTF-8"))
      (:body ((:div :id "container") ,@(rest doc))))))

(defun entitle (title)
  "Add a :TITLE element to :HEAD based on the contents of the first :H1"
  #'(lambda (doc)
      (if title
          (funcall (rewriter :head (appending `((:title ,title)))) doc)
          (let ((h1 (first (extract :h1 doc))))
            (when h1
              (funcall (rewriter :head (appending `((:title ,(just-text (cdr h1)))))) doc))))))

(defun stylize (style &key inline)
  "Add an :LINK or :STYLE to :HEAD for an external or inline style element."
  (rewriter
   :head
   (appending (if inline
                  `((:style (:noescape ,style)))
                  `((:link :href ,style :rel "stylesheet"))))))

(defun enscript (script)
  "Add an SCRIPT tag if script is provided."
  (if script
      (rewriter :body (appending `((:script :src ,script :type "text/javascript"))))
      #'identity))

;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         :class "backlink")
        ,n) " "
       ,@e1-body)
      ,@body)))

(defun fragment (x)
  "Make a framgment HREF value."
  (format nil "#~a" x))

(defun note-id (n)
  "The id value for the actual footnote number N."
  (format nil "note_~d" n))

(defun marker-id (n)
  "The id value for the marker for footnote number N."
  (format nil "marker_~d" n))

(defun get-linkdefs (doc)
  "Extract the link names and urls."
  (loop with h = (make-hash-table :test 'equalp)
     for (nil (nil link) (nil url)) in (extract :link_def doc)
     do (setf (gethash link h) url)
     finally (return h)))

(defun linker (links)
  "Rewrite a link tag into anchor."
  #'(lambda (x) `((:a :href ,(get-url (link-key x) links)) ,@(link-contents x))))

(defun get-url (link h)
  "Lookup the URLy for a link. Warn if none found."
  (or (gethash link h) (progn (warn "No link found for ~a" link) "nowhere.html")))

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



(defun formatted-code-section (expr)
  (and
   (consp expr)
   (eql (first expr) :section)
   (eql (first (second expr)) :formattedcode)))

(defun formatted-code (expr)
  (destructuring-bind (section (formattedcode (pre text))) expr
    (declare (ignore section formattedcode pre))
    `(:pre ,@(nth-value 1 (markup-lite (cons text 0))))))
