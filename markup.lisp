;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun markup (text)
  "Parse a string containing markup. If the markup uses subdoc tags,
they should be provided via the SUBDOCS keyword arg. Or they can be specified in
the modeline as a comma-delimited list in the value of the 'subdoc' file
variable."
  (multiple-value-bind (ok r) (markup-lang (cons (detab text) 0))
    (and ok r)))

(defparser markup-lang (&state subdocs (indent 0) (so-far 0) (subdoc-level 0))

  (document
   (optional modeline)
   (many eol)
   (=> (many element) `(:body ,@_)))

  (element
   indentation
   (or extra-blank
       header
       section-one-line
       section
       section-divider
       verbatim
       ordered-list
       unordered-list
       definition-list
       blockquote
       linkdef
       paragraph))

  (extra-blank eol `(:blank))

  (header
   (-> (many1 "*") stars)
   whitespace
   (=> paragraph-text `(,(keywordize (format nil "H~d" (length stars))) ,@_)))

  (section-one-line
   "## " (-> name name) " "
   (=> one-line-text `(:section (,name ,@_)))
   " ##." (or blank eol))

  (one-line-text
   (=> (many1 (or (text-until (or " ##." tag-open "[")) tagged-text linkref))))

  (section
   "## " (-> name name) blank
   (=> (many (and (! end-section) element)) `(:section (,name ,@_)))
   end-section)

  (end-section indentation "##." blank)

  (verbatim (=> (indented 3 verbatim-text) `(:pre ,_)))

  (verbatim-text (=> (many1 (or eol verbatim-line)) (combine-verbatim _)))

  (verbatim-line
   indentation
   (=> (text (many1 (and (! (or eol section-divider)) any-char))))
   (or eol eod))

  (ordered-list (=> (indented 2 (many1 (list-item "#"))) `(:ol ,@_)))

  (unordered-list (=> (indented 2 (many1 (list-item "-"))) `(:ul ,@_)))

  ((list-item marker)
   indentation (match marker) " "
   (extra-indentation 2)
   (=> (many1 (and indentation (or ordered-list unordered-list paragraph))) `(:li ,@_))
   (decf indent 2))

  (definition-list
   (=> (indented 2 (and (peek term) (many1 (or term definition)))) `(:dl ,@_)))

  (term
   indentation "% "
   (=> (many1 (or (text-until (or tag-open " %")) tagged-text)) `(:dt ,@_))
   " %" (or blank eol))

  (definition
   (=> (many1 (and indentation (! "% ") paragraph)) `(:dd ,@_)))

  (blockquote
   (=> (indented 2 (many1 blockquote-element)) `(:blockquote ,@_)))

  (blockquote-element
   (! (and (counted 3 #\Space) (not-char #\Space)))
   indentation element)

  (linkdef
   "[" (-> (text-until "]") name) "]" whitespace "<" (-> (text-until ">") url) ">"
   (or newline blank)
    `(:link_def (:link ,name) (:url ,url)))

  (section-divider whitespace "§" blank `(:§ "§"))

  (paragraph (=> paragraph-text `(:p ,@_)))

  (modeline
   "-*-" (-> (many modeline-variable) vars) "-*-"
   blank
   (when (assoc :subdocs vars)
     (setf subdocs (extract-subdocs vars))))

  (modeline-variable
   whitespace
   (-> name n) ": " (=> (text (many (not-char #\;))) (cons n _)) ";"
   whitespace)

  (paragraph-text
   (=> (many1 (or (text-until (or tag-open "[" blank)) tagged-text linkref)))
   blank)

  (linkref
   "["
   (-> link-contents contents)
   (=> (optional link-key) `(:link ,@contents ,@(if _ (list `(:key ,_)))))
   "]")

  (link-contents (many1 (or (text-until (or tag-open "|" "]")) tagged-text)))

  (link-key "|" (text-until "]"))

  (escaped-char (and "\\" (or #\\ #\{ #\} #\* #\# #\- #\[ #\] #\% #\| #\<)))

  ((unescaped p) (! escaped-char) (match p))

  (newline-char (setf so-far 0) #\Newline)

  (eol whitespace newline-char)

  (newline (! blank) eol indentation '#\Space)

  (blank
   (or eod (and eol (or eol eod))))

  (plain-char (in-subdoc (not-char "}") any-char))

  (tag-open (unescaped "\\") (=> name) "{")

  (tagged-text
   (-> tag-open tag)
   (=> (if (member tag subdocs) subdoc-contents simple-contents) `(,tag ,@_))
   "}")

  (subdoc-contents
   (incf subdoc-level)
   (=> (many element)) eod
   (decf subdoc-level))

  (simple-contents (many (or (text-until (or tag-open "[" "}")) tagged-text linkref)))

  (name (=> (text (many1 (? any-char #'alpha-char-p))) (keywordize _)))

  (whitespace (many (or #\Space #\Tab)))

  ((extra-indentation n)
   (incf indent n)
   (setf so-far indent))

  (eod (in-subdoc end-of-subdoc eof))

  (end-of-subdoc (peek "}"))

  (indentation
   (counted (- indent so-far) #\Space)
   (setf so-far indent)
   t)

  ((indented n p)
   (peek (counted n #\Space))
   (incf indent n)
   (=> (match p))
   (decf indent n))

  ((text-until p)
   (=> (many1 (and (! p) (or escaped-char newline plain-char))) (to-string _)))

  ((not-char p) (and (! p) any-char))

  ((in-subdoc p1 p2) (if (> subdoc-level 0) (match p1) (match p2))))


;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun detab (s)
  "Convert tab characters to eight spaces."
  (with-output-to-string (out)
    (loop for c across s
       do (if (eql c #\Tab)
              (dotimes (i 8) (write-char #\Space out))
              (write-char c out)))))

(defun to-string (chars) (concatenate 'string chars))

(defun combine-verbatim (lines)
  "Combine the lines from parsing a verbatim section into a single string."
  (format nil "~{~&~a~}" (drop-trailing-blanks lines)))

(defun drop-trailing-blanks (lines)
  "Drop all the trailing blank lines."
  (labels ((drop (x xs)
             (if (and (consp xs) (eql x (car xs)))
                 (drop x (rest xs))
                 xs)))
    (nreverse (drop #\Newline (nreverse lines)))))

(defun extract-subdocs (vars)
  "Extract the list of subdoc tags from the value in the modeline."
  (mapcar #'keywordize (split-sequence #\, (cdr (assoc :subdocs vars)))))
