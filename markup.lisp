;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defparser markup (subdocs &state (current 0) (so-far 0) (subdoc-level 0))

  (document
   (optional modeline)
   (many (try eol))
   (-> (many element) paragraphs)
   eod
   `(:body ,@paragraphs))

  (element
   indentation
   (or header
       ;;section
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
   `(:section ,name ,@paragraphs))

  (section-body
   (not-followed-by "##.")
   element)

  (verbatim
   (-> (indented 3 verbatim-text) txt)
   `(:pre ,txt))

  (verbatim-text
   (-> (many1 (or (tracer verbatim-blank-line 'verbatim-blank-line)
                  (tracer verbatim-line 'verbatim-line))) lines)
   (format nil "~{~&~a~}" (drop-trailing-blanks lines)))

  (verbatim-blank-line (try eol))

  (verbatim-line
   (tracer indentation 'indentation-in-verbatim-line)
   (-> (text (many1 (progn (not-followed-by eol) any-char))) txt)
   (or eol (try eod))
   txt)

  (ordered-list
   (show-state)
   (tracer (try (listy :ol "#")) 'listy))

  (unordered-list (show-state) (listy :ul "-"))

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
   (try (not-followed-by (progn (counted 3 " ") (none-of " "))))
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
                      (tracer (try indentation) 'try-indentation)
                      (tracer (list-element marker) 'list-element)))) contents)
   `(,name ,@contents))

  ((list-element marker)
   (ensure
    (progn
      (show-state "Start list-element")
      (extra-indentation 2)
      (show-state "After extra-indentation")
      (tracer (try (progn (match marker) " ")) 'list-marker)
      (-> (many1 (progn indentation (or ordered-list unordered-list paragraph))) contents)
      `(:li ,@contents))
    (progn
      (show-state "End list-element")
      (dedent 2))))

  ;; Whitespace and indentation handling

  (whitespace (many (or #\Space #\Tab)))

  ((indent n) (incf current n))

  ((dedent n)
   (decf current n)
   (setf so-far 0)) ;; FIXME: this is a hack. Need a systematic way to
  ;; reset state when productions fail.

  ((extra-indentation n)
   (incf current n)
   (incf so-far n))

  (eol whitespace newline-char)

  (eod (in-subdoc end-of-subdoc eof))

  (end-of-subdoc (look-ahead "}"))

  (blank
   (or eol eod)
   (or (many1 (try eol)) eod))

  (indentation
   (show-state "Beginning of indentation")
   (tracer
    (try
     (tracer
      (counted (- current so-far) " ")
      'counted))
     'try)
   (setf so-far current)
   (show-state "End of indentation")
   t)

  ((indented n p)
   (ensure ;; Build this into the compiler so we don't have to wrap
           ;; the cleanup clause in a lambda
    (progn
      (incf current n)
      (try (look-ahead (counted n " ")))
      (match p))
    (lambda (txt pos) (declare (ignore txt pos)) (decf current n))))

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

(defun mush-text (chars) (format nil "~{~a~}" chars))


#+(or)(defparser markup (&state (current 0) (so-far 0) (subdoc-level 0))

  (document
   (-> (optional modeline) m)
   (many (try eol))
   (-> (many element) paragraphs)
   eod
   `(:document (:modeline ,m) ,@paragraphs))

  (modeline
   (-> (text
        (progn
          (try "-*-")
          (many (progn (not-followed-by eol) any-char))))
       ml)
   `(:modeline ,ml))

  (element
   indentation
   (or header paragraph))

  (header
   (-> (many1 "*") stars)
   whitespace
   (-> paragraph-text txt)
   `(,(keywordize (format nil "H~d" (length stars))) ,txt))

  (paragraph
   (-> paragraph-text txt)
   `(:paragraph ,txt))

  (paragraph-text
   (-> (many1 (or (text-until (tagged-or (or "[" blank))) tagged-text linkref)) txt)
   blank
   txt)

  (tag-open
   (not-followed-by escaped-char) "\\"
   (-> name n)
   "{"
   n)

  (name (text (many1 (char-if #'alpha-char-p))))

  (tagged-text
   (-> tag-open n)
   (-> (text-until "}") element)
   "}"
   `(,(keywordize n) ,element))

  ((tagged-or p) (or tag-open (match p)) nil)

  (linkref
   "[" (-> (text-until "]") contents) "]"
   `(:link ,contents))

  (indentation
   (try (counted (- current so-far) " "))
   (setf so-far current))

  (eol whitespace #\Newline)

  (eod (in-subdoc end-of-subdoc eof))

  (end-of-subdoc (look-ahead "}"))

  (blank
   (or eol eod)
   (or (many1 (try eol)) eod))

  (whitespace (many (or #\Space #\Tab)))

  ((in-subdoc p1 p2)
   (if (> subdoc-level 0) (match p1) (match p2)))

  ((text-until p)
   (-> (many1 (progn (not-followed-by p) (or escaped-char newline plain-char))) chars)
   (format nil "~{~a~}" chars))

  (escaped-char
   (try (progn "\\" (or #\\ #\{ #\} #\* #\# #\- #\[ #\] #\% #\| #\<))))

  (newline
   (not-followed-by blank)
   (-> newline-char nl)
   indentation
   nl)

  (newline-char
   (setf so-far 0)
   #\Newline)

  (plain-char any-char)
  )

;;; Utility functions

(defun drop-trailing-blanks (lines)
  "Drop all the trailing blank lines and also the \n's on the end of the last line."
  ;; TODO: implement
  lines)
