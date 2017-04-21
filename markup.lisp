;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun markup (text &key subdocs)
  "Parse a string containing markup. If the markup uses subdoc tags,
they should be provided via the SUBDOCS keyword arg. Or they can be specified in
the modeline as a comma-delimited list in the value of the 'subdoc' file
variable."
  (let ((text (detab text))) ;; This actually only currently works for string input
    (multiple-value-bind (ok r) (%markup subdocs (cons text 0))
      (and ok r))))

(defparser %markup (subdocs &state (indent 0) (so-far 0) (subdoc-level 0))

  (document
   (optional modeline)
   (many eol)
   (=> (many element) `(:body ,@_))
   eod)

  (element
   indentation
   (or header
       section
       verbatim
       ordered-list
       unordered-list
       definition-list
       blockquote
       linkdef
       section-divider
       paragraph))

  (header
   (-> (many1 "*") stars)
   whitespace
   (=> paragraph-text `(,(keywordize (format nil "H~d" (length stars))) ,@_)))

  (section
   "## " (-> name name) (many1 eol)
   (=> (many (and (! "##.") element)) `(,name ,@_))
   "##." blank)

  (verbatim (=> (indented 3 verbatim-text) `(:pre ,_)))

  (verbatim-text (=> (many1 (or eol verbatim-line)) (combine-verbatim _)))

  (verbatim-line
   indentation
   (=> (text (many1 (and (! eol) any-char))))
   (or eol eod))

  (ordered-list (=> (indented 2 (many1 (list-item "#"))) `(:ol ,@_)))

  (unordered-list (=> (indented 2 (many1 (list-item "-"))) `(:ul ,@_)))

  ((list-item marker)
   indentation (match marker) " "
   (extra-indentation 2)
   (=> (many1 (and indentation (or ordered-list unordered-list paragraph))) `(:li ,@_))
   (decf indent 2))

  (definition-list
   (=> (indented 2 (and (look-ahead term) (many1 (or term definition)))) `(:dl ,@_)))

  (term
   indentation "% "
   (=> (many1 (or (text-until (or tag-open " %")) tagged-text)) `(:dt ,@_))
   " %" eol)

  (definition
   (=> (many1 definition-paragraph) `(:dd ,@_)))

  (definition-paragraph
    (and indentation (! "% "))
    paragraph)

  (blockquote
   (=> (indented 2 (many1 blockquote-element)) `(:blockquote ,@_)))

  (blockquote-element
   (! (and (counted 3 #\Space) (not-char #\Space)))
   element)

  (linkdef
   (-> (between "[" "]" (text-until "]")) name)
   " "
   (=> (between "<" ">" (text-until ">")) `(:link_def (:link ,name) (:url ,_)))
   blank)

  (section-divider whitespace "§" blank `(:section "§"))

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

  (link-contents
   (many1 (or (text-until (or tag-open "|" "]")) tagged-text)))

  (link-key
   "|" (text (many1 (not-char "]"))))

  (escaped-char
   (and "\\" (or #\\ #\{ #\} #\* #\# #\- #\[ #\] #\% #\| #\<)))

  ((unescaped p) (! escaped-char) (match p))

  (newline
   (! blank)
   newline-char
   indentation
   (value #\Space))

  (newline-char (setf so-far 0) #\Newline)

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

  (simple-contents
   (many1 (or (text-until (or tag-open "}")) tagged-text)))

  (name (=> (text (many1 (? any-char #'alpha-char-p))) (keywordize _)))

  (whitespace (many (or #\Space #\Tab)))

  ((extra-indentation n)
   (incf indent n)
   (setf so-far indent))

  (eol whitespace newline-char)

  (eod (in-subdoc end-of-subdoc eof))

  (end-of-subdoc (look-ahead "}"))

  (blank
   (or eol eod)
   (or (many1 eol) eod))

  (indentation
   (counted (- indent so-far) #\Space)
   (setf so-far indent)
   t)

  ((indented n p)
   (look-ahead (counted n #\Space))
   (incf indent n)
   (=> (match p))
   (decf indent n))

  ((text-until p)
   (=> (many1
        (and
          (! p)
          (or escaped-char newline plain-char)))
       (format nil "~{~a~}" _)))

  ((in-subdoc p1 p2)
   (if (> subdoc-level 0) (match p1) (match p2)))

  ((between start end p)
   (match start)
   (=> (match p))
   (match end)))


;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric detab (s))

(defmethod detab ((s string))
  "Convert tab characters to eight spaces."
  (with-output-to-string (out)
    (loop for c across s
       do (if (eql c #\Tab)
              (dotimes (i 8) (write-char #\Space out))
              (write-char c out)))))

(defmethod detab ((s stream)) s) ;; FIXME: this obviously needs to do something

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
