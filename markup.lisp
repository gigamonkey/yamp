;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun markup (text subdocs)
  (%markup (detab text) 0 subdocs))

(defparser %markup (subdocs &state (current 0) (so-far 0) (subdoc-level 0))

  (document
   (optional modeline)
   (many (try eol))
   (-> (many element) paragraphs)
   eod
   `(:body ,@paragraphs))

  (element
   indentation
   (or header
       section
       verbatim
       ordered-list
       unordered-list
       ;;dlist
       blockquote
       (try linkdef)
       ;;section-divider
       paragraph))

  (header
   (-> (many1 "*") stars)
   whitespace
   (-> paragraph-text txt)
   `(,(keywordize (format nil "H~d" (length stars))) ,@txt))

  (section
   "## " (-> name name) (many1 (try eol))
   (-> (many section-body) paragraphs)
   "##." blank
   `(,(keywordize name) ,@paragraphs))

  (section-body
   (not-followed-by "##.")
   element)

  (verbatim
   (-> (indented 3 verbatim-text) txt)
   `(:pre ,txt))

  (verbatim-text
   (-> (many1 (or verbatim-blank-line verbatim-line)) lines)
   (format nil "~{~&~a~}" (drop-trailing-blanks lines)))

  (verbatim-blank-line (try eol))

  (verbatim-line
   indentation
   (-> (text (many1 (progn (not-followed-by eol) any-char))) txt)
   (or eol (try eod))
   txt)

  (ordered-list (try (listy :ol "#")))

  (unordered-list (listy :ul "-"))

  (dlist
   (-> (indented 2 (progn (look-ahead term) (many1 (or term definition)))) ds)
   `(:dl ,@ds))

  (term
   (try (progn indentation "% "))
   (-> (many1 (or (text-until (tagged-or " %")) tagged-text)) term)
   " %"
   eol
   `(:term ,term))

  (definition
   (-> (many1 definition-paragraph) paragraphs)
   `(:definition ,@paragraphs))

  (definition-paragraph
   (try (progn indentation (not-followed-by "% ")))
   paragraph)

  (blockquote
   (-> (indented 2 (many1 blockquote-element)) es)
   `(:blockquote ,@es))

  (blockquote-element
   (try (not-followed-by (progn (counted 3 #\Space) (none-of #\Space))))
   element)

  (linkdef
   (-> (between "[" "]" (text-until "]")) name) " "
   (-> (between "<" ">" (text-until ">")) link) blank
   `(:link_def (:link ,name) (:url ,link)))

  (section-divider whitespace "§" blank `(:section-divider))

  (paragraph
   (-> paragraph-text txt)
   `(:p ,@txt))

  (modeline
   (try "-*-")
   (many (progn (not-followed-by blank) any-char))
   blank
   nil)

  (paragraph-text
   (-> (many1 (or (text-until (tagged-or (or "[" blank))) tagged-text linkref)) txt)
   blank
   txt)

  (linkref
   "["
   (-> link-contents contents)
   (-> (optional link-key) key)
   "]"
   `(:link ,@contents ,@(if key (list `(:key ,key)))))

  (link-contents
   (many1 (or (text-until (tagged-or (or "|" "]"))) tagged-text)))

  (link-key
   "|" (text (many1 (none-of "]"))))

  (escaped-char
   (try (progn "\\" (or #\\ #\{ #\} #\* #\# #\- #\[ #\] #\% #\| #\<))))

  (newline
   (not-followed-by blank)
   newline-char
   indentation
   (value #\Space))

  (plain-char
   (in-subdoc (! "}") any-char))

  (tag-open
   (not-followed-by escaped-char) "\\"
   (-> name n)
   "{"
   (keywordize n))

  (tagged-text
   (-> tag-open n)
   (-> (if (member n subdocs) subdoc-contents simple-contents) contents)
   "}"
   `(,n ,@contents))

  (subdoc-contents
   (incf subdoc-level)
   (-> (many1 element) elements)
   eod
   (decf subdoc-level)
   elements)

  (simple-contents
   (many1 (or (text-until (tagged-or "}")) tagged-text)))

  ((tagged-or p) (or tag-open (match p)))

  (name (text (many1 (char-if #'alpha-char-p))))

  ((listy name marker)
   (-> (indented 2 (many1
                    (progn
                      (try indentation)
                      (list-element marker)))) contents)
   `(,name ,@contents))

  ((list-element marker)
   (try (progn (match marker) " "))
   (ensure
    (progn
      (extra-indentation 2)
      (-> (many1 (progn indentation (or ordered-list unordered-list paragraph))) contents)
      `(:li ,@contents))
    (progn
      (dedent 2))))

  ;; Whitespace and indentation handling

  (whitespace (many (or #\Space #\Tab)))

  ((indent n) (incf current n))

  ((dedent n)
   (decf current n))

  ((extra-indentation n)
   (incf current n)
   (setf so-far current))

  (eol whitespace newline-char)

  (eod (in-subdoc end-of-subdoc eof))

  (end-of-subdoc (look-ahead "}"))

  (blank
   (or eol eod)
   (or (many1 (try eol)) eod))

  (indentation
   (try (counted (- current so-far) #\Space))
   (setf so-far current)
   t)

  ((indented n p)
   (try (look-ahead (counted n #\Space)))
   (ensure ;; Build this into the compiler so we don't have to wrap
           ;; the cleanup clause in a lambda
    (progn
      (incf current n)
      (match p))
    (lambda (txt pos)
      (declare (ignore txt pos))
      (decf current n))))

  (newline-char
   (setf so-far 0)
   #\Newline)

  ((text-until p)
   (-> (many1 (progn (not-followed-by p) (or escaped-char newline plain-char))) chars)
   (mush-text chars))

  ((in-subdoc p1 p2)
   (if (> subdoc-level 0) (match p1) (match p2)))

  ((between start end p)
   (match start)
   (-> (match p) r)
   (match end)
   r))

;;; Utility functions

(defun detab (s)
  (with-output-to-string (out)
    (loop for c across s
       do (if (eql c #\Tab)
              (dotimes (i 8) (write-char #\Space out))
              (write-char c out)))))

(defun mush-text (chars) (format nil "~{~a~}" chars))

(defun drop-trailing-blanks (lines)
  "Drop all the trailing blank lines and also the \n's on the end of the last line."
  (labels ((drop (x xs)
             (if (and (consp xs) (eql x (car xs)))
                 (drop x (rest xs))
                 xs)))
    (nreverse (drop #\Newline (nreverse lines)))))
