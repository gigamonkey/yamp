;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun generate-html (file)
  "Generate HTML for the markup file in the same location except with an .html
extension."
  (multiple-value-bind (output-file config) (html-filename file)
    (with-output-to-file (out (ensure-directories-exist output-file))
      (with-text-output (out)
        (monkeylib-html:emit-html
         (markup-html (markup (file-text file)) config)))))
  ;; Return t here since we invoke this from slime which chokes on the pathname.
  t)

(defun html-filename (file)
  "Translate filenae of Markup file to the HTML file to be generated and load
the corresponding config file."
  (let* ((*default-pathname-defaults* (parent-directory file))
         (config (load-config file))
         (output (first (config :output config))))
    (values (make-pathname :type "html" :defaults (if output (merge-pathnames output file) file)) config)))

(defun markup-html (doc config)
  (let ((has-tweets (extract :tweet doc))
        (dateline (or (first (extract :dateline doc)) (config :dateline config))))
    (destructuring-bind (&key year &allow-other-keys) (parse-iso-8601 (just-text dateline))
      (let ((config (cons `(:year ,year) config)))
        (funcall
         (>>>
          ;; Massaging the Markup structure into HTML.
          #'links
          #'endnotes
          (rewriter :img #'(lambda (img) `(:img :src ,@(rest img))))

          ;; Special sections specified in config file.
          (section-rewriter (config :sections config))

          ;; Other sections get turned into divs.
          (rewriter :section #'(lambda (s) (divver (second s))))

          ;; Section markers
          (rewriter :§ (replacing-with (first (config :section-marker config))))

          ;; Spans.
          (spans-rewriter (config :spans config))

          ;; Turn into HTML
          (htmlizer config)

          #'(lambda (d)
              (if (eql (config :title config) :auto) (entitle d) d))

          (twitter-widget has-tweets))
         doc)))))

(defun spans-rewriter (spans)
  (flet ((apply-spanner (d s) (funcall (rewriter s #'spanner) d)))
    #'(lambda (doc)
        (reduce #'apply-spanner spans :initial-value doc))))

(defun section-rewriter (sections)
  (labels ((apply-section (d s)
             (destructuring-bind (tag sym) s
               (funcall (rewriter :section (if-section-p tag (symbol-function sym))) d)))
           (if-section-p (tag fn)
             #'(lambda (tree)
                 (if (eql (car (second tree)) tag) (funcall fn (second tree)) tree))))
    #'(lambda (doc)
        (reduce #'apply-section sections :initial-value doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defun config (key config) (cdr (assoc key config)))

(defun load-config (filename)
  (let ((eof '#:eof))
    (labels ((read-file (f)
             (with-open-file (in f :if-does-not-exist nil)
               (when in
                 (loop for o = (read in nil eof) until (eql o eof) collecting o))))
             (combine (acc file) (merge-alists acc (read-file file)))
             (config-location (filename)
               (if (file-exists-p "./config/")
                   (merge-pathnames "config/" filename)
                   filename)))
      (let ((files (list
                    (make-pathname :name "defaults" :type "config" :defaults (config-location filename))
                    (make-pathname :type "config" :defaults (config-location filename)))))
        (let ((from-files (reduce #'combine files :initial-value ())))
          (if (config :output from-files)
              from-files
              (cons `(:output ,(default-output-config filename)) from-files)))))))

(defun merge-alists (a1 a2)
  (flet ((combine (acc next)
           (destructuring-bind (k . v) next
             (let ((in-a1 (assoc k acc)))
               (if in-a1
                   (cons `(,k ,@(cdr in-a1) ,@v) (remove k acc :key #'car))
                   (cons `(,k ,@v) acc))))))
    (nreverse (reduce #'combine a2 :initial-value a1))))

(defun default-output-config (filename)
  (if (file-exists-p "./web/")
      (make-pathname :directory (list :relative "web" (pathname-name filename)) :name "index")
      filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link rewriting

(defun links (doc)
  "Rewrite the doc so :link elements are expanded into :a tags with the
appropriate link."
  (let ((linkdefs (get-linkdefs doc)))
    (funcall
     (>>> (deleter :link_def) (rewriter :link (linker linkdefs)))
     doc)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Endnote rewriting

(defun endnotes (doc)
  "Rewrite a doc with :note elements converted to endnotes."
  (if (extract :note doc)
      (funcall
       (>>>
        (rewriter :note (numberer))
        #'(lambda (x) `(,@x (:notes ,@(extract :note x))))
        (rewriter :notes (rewriter :note (>>> #'endnote-backlinker #'divver)))
        (rewriter :notes #'divver)
        (rewriter :body (rewriter :note #'endnote-marker)))
       doc)
      doc))

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

(defun endnote-marker (note)
  "Make a :NOTE element into its endnote marker, linking to the note and with an
ID to allow linking back."
  (let ((n (second note)))
    `((:a
       :id ,(marker-id n)
       :href ,(fragment (note-id n))
       :class "marker")
      ,n)))

(defun fragment (x)
  "Make a framgment HREF value."
  (format nil "#~a" x))

(defun note-id (n)
  "The id value for the actual footnote number N."
  (format nil "note_~d" n))

(defun marker-id (n)
  "The id value for the marker for footnote number N."
  (format nil "marker_~d" n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTMLization

(defun htmlizer (config)
  (let ((name (first (config :htmlizer config))))
    (if name
        #'(lambda (doc) (funcall (symbol-function name) doc config))
        #'htmlize)))

(defun htmlize (doc)
  "Default htmlizer used when one isn't specified in the config file. Wrap
the :body we get from the markup parser in a proper HTML5 document with a
doctype and proper charset."
  `(:progn
    (:noescape "<!DOCTYPE html>")
    (:html
      (:head
       (:meta :charset "UTF-8"))
      (:body ((:div :id "container") ,@(rest doc))))))

(defun entitle (doc)
  "Add a :TITLE element to :HEAD based on the contents of the first :H1"
  (let ((h1 (first (extract :h1 doc))))
    (if h1
        (funcall (rewriter :head (appending `((:title ,(just-text (cdr h1)))))) doc)
        doc)))

(defun twitter-widget (has-tweets)
  (if has-tweets
      (rewriter :body
                (appending
                 `((:script
                    :async "async"
                    :src "https://platform.twitter.com/widgets.js"
                    :type "text/javascript"
                    :charset "utf-8"))))
      #'identity))

(defun just-text (sexp)
  "Textual content of the element as a single string with all markup
removed."
  (with-output-to-string (s)
    (labels ((walk (x)
               (typecase x
                 (string (write-string x s))
                 (cons (mapcar #'walk x)))))
      (walk sexp))))

;; Used in config files
(defun formatted-code (expr)
  (destructuring-bind (pre text) expr
    (declare (ignore pre))
    `(:pre ,@(nth-value 1 (markup-lite (cons text 0))))))

;; Used in config files
(defun tweet-by-id (tree)
  (let ((filename (make-pathname :defaults (merge-pathnames "tweets/") :name (just-text tree))))
    `(:noescape ,(file-text filename))))

;; Used in config files
(defun chart (tree)
  `(:div :class "chart" :id (:format "chart~a" ,(just-text tree))))

;; Used in config files
(defun sourcecode (tree)
  (let* ((text (string-trim " " (just-text tree)))
         (spc (position #\Space text))
         (file (subseq text 0 spc))
         (name (if spc (subseq text (1+ spc)) nil))
         (contents (file-text file))
         (lines (split-sequence #\Newline contents)))
    (if (not name)
        `(:pre (:code ,(format nil "~{~a~%~}" (remove-if #'(lambda (line) (search "8<---" line)) lines))))
        (flet ((start (line) (search (format nil "8<--- ~a" name) line))
               (end (line) (search "8<----" line)))
          (let* ((start (cdr (member-if #'start lines)))
                 (end (member-if #'end start)))
            (if end
                `(:pre (:code ,(format nil "~{~&~a~}" (ldiff start end))))
                (error "Coludn't find include section ~a in ~a" name file)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This doesn't really belong here. This is specific to how I generate HTML for
;; my website and is only used specified in a config file.

(defun gigamonkeys-html (doc config)
  (let ((styles (config :styles config))
        (scripts (config :scripts config)))
    `(:progn
       (:noescape "<!doctype html>")
       ((:html :lang "en")
        (:head
         (:meta :http-equiv "content-type" :content "text/html; charset=UTF-8")
         (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")
         ,@(loop for s in styles collecting s)
         (:noescape "<!--[if lt IE 9]>")(:script :src "//html5shim.googlecode.com/svn/trunk/html5.js")(:noescape "<![endif]-->"))

        (:body
         ((:div :class "wrap")
          (:header
           (:figure (:img :src "../img/monkey.jpg" (:figcaption "Original image by Luc Viatour / " ((:a :href "http://www.Lucnix.be") "www.Lucnix.be")))))
          ((:div :class "contents") ,@(rest doc))
          (:footer
           (:p "Copyright " ,@(config :year config) " Peter Seibel")
           (:p ((:a :href "mailto:peter@gigamonkeys.com") "peter@gigamonkeys.com") " · "
               ((:a :href "http://twitter.com/peterseibel") "@peterseibel") " · "
               ((:a :href "http://github.com/gigamonkey") "gigamonkey"))
           (:p "Monkey image Luc Viatour / " ((:a :href "http://www.Lucnix.be") "www.Lucnix.be"))))
         ,@(loop for s in scripts collecting s))))))

(defvar *months* #("UNUSED" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(defun format-dateline (tree)
  (destructuring-bind (&key year (month 0) day) (parse-iso-8601 (just-text tree))
    `((:div :class "dateline") ,(format nil "~d ~a ~d" day (elt *months* month) year))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Amazon Kindle Direct Publishing

(defun kdp-html (doc config)
  (let ((styles (config :styles config)))
    `(:progn
       (:noescape "<!doctype html>")
       ((:html :lang "en")
        (:head ,@(loop for s in styles collecting s))
        (:body ,@(rest (funcall
                        (>>>
                         (deleter :h1)
                         #'style-kdp-paragraphs
                         (retagger :h2 :h1))
                        (anchor-chapters (latin-1-safe doc)))))))))

(defun style-kdp-paragraphs (doc)
  (labels ((non-paragraph-element (x)
             (and (consp x) (not (eql (car x) :p))))

           (paragraph-element (x)
             (and (consp x) (eql (car x) :p)))

           (walk (tree)
             (cond
               ((and (consp tree)
                     (non-paragraph-element (first tree))
                     (paragraph-element (second tree)))
                `(,(walk (first tree))
                   ,(walk `((:p :class "noindent") ,@(rest (second tree))))
                   ,@(walk (rest (rest tree)))))
               ((consp tree)
                `(,(walk (first tree)) ,@(walk (rest tree))))
               (t tree))))
    (walk doc)))

(defun generate-kdp-toc (file)
  (let ((chapters (extract :h2 (markup (file-text file))))
        (output-file (make-pathname :name "toc" :defaults (html-filename file))))
    (with-output-to-file (out output-file)
      (with-text-output (out)
        (monkeylib-html:emit-xhtml
         `(:progn
            (:? "xml" :version 1.0 :encoding "utf-8")
            (:noescape "<!DOCTYPE html>")
            ((:html :xmlns "http://www.w3.org/1999/xhtml"
                    :xmlns\:epub "http://www.idpf.org/2007/ops")
             (:body
              ((:nav :epub\:type "toc")
               (:ol
                ,@(loop for i from 1 for c in chapters collecting
                       `(:li (:a :href ,(format nil "grid.html#chapter_~d" i) ,(just-text c))))))
              ((:nav :epub\:type "landmarks" :class "hidden-tag" :hidden "hidden")
               ((:ol :class "none" :epub\:type "list")
                (:li (:a :epub\:type "toc" :href "toc.html" "Table of Contents"))))))))))))

(defun generate-kdp-titlepage (file)
  (multiple-value-bind (html-output config) (html-filename file)
    (let ((title (first (extract :h1 (markup (file-text file)))))
          (output-file (make-pathname :name "title" :defaults html-output))
          (author (first (config :author config)))
          (copyright (first (config :copyright-year config))))
      (with-output-to-file (out output-file)
        (with-text-output (out)
          (monkeylib-html:emit-html
           `(:html
              (:head
               (:link :rel "stylesheet" :type "text/css" :href "style.css"))
              (:body
               ((:p :class "title") ,(just-text title))
               ((:p :class "author") ,author)
               ((:p :class "copyright") (:noescape "&#x00a9;") ,(format nil " ~d ~a" copyright author))))))))))

(defun anchor-chapters (doc)
  (let ((c 0))
    (flet ((add-anchor (tree) `(:h2 :id ,(format nil "chapter_~d" (incf c)) ,@(rest tree))))
      (funcall (rewriter :h2 #'add-anchor) doc))))

(defun latin-1-safe (doc)
  (labels ((walk (tree)
             (typecase tree
               (string (entify tree 0))
               (cons (list (mapcan #'walk tree)))
               (t (list tree)))))
    (first (walk doc))))

(defun entify (text pos)
  (when (< pos (length text))
    (let ((nextpos (position-if #'(lambda (c) (> (char-code c) 255)) text :start pos)))
      (cond
        ((null nextpos)
         (list (subseq text pos)))
        ((= nextpos pos)
         `((:noescape ,(format nil "&#x~x;" (char-code (char text nextpos)))) ,@(entify text (1+ nextpos))))
        (t
         `(,(subseq text pos nextpos) ,@(entify text nextpos)))))))
