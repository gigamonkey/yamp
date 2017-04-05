;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun parse-file ())

(defun parse-text ())

(defparser document ()
  (optional modeline)
  (many (try eol))
  (-> (many element) paragraphs)
  eod
  `(:document ,@paragraphs))

(defparser element ()
  indentation
  (-> (or header
          section
          verbatim
          ordered-list
          unordeded-list
          definition-list
          blockquote
          (try linkdef)
          section-divider
          paragraph)))

(defparser header ()
  (-> header-marker level)
  (-> paragraph-text text)
  `(:header ,level ,text))

(defparser header-marker ()
  (-> (many1 "*") stars)
  whitespace
  (length stars))

(defparser section ()
  "## " (-> name name) (many1 (try eol))
  (-> (many section-body) paragraphs)
  "##." blank
  `(:section ,name ,@paragraphs))

(defparser verbatim ()
  (indented 3 (-> verbatim-text text))
  `(:verbatim ,text)

(defparser verbatim-text ()
  (-> (many1 (or verbatim-blank-line verbatim-line)) lines)
  (format nil "~{~&~a~}" (drop-trailing-blanks lines)))

(defparser verbatim-blank-line () (try eol) #\Newline)

(defparser verbatim-line ()
  indentation
  (-> (many1 (p (not-followed-by eol) any-char)) text)
  (or eol (try eod))
  (apply #'concatenate 'string text))

(defparser ordered-list () (listy :ol "#"))

(defparser unordered-list () (listy :ul "-"))

(defparser definition-list ()
  (-> (indented 2
                (-> (p (look-ahead term) (-> (many1 (or term definition)) ds)) ds)) ds)
  `(:dl ,@ds))

(defparser term ()
  (try (p indentation "% "))
  (-> (many1 (text-until (or (tagged-or " %") tagged-text))) term)
  " %"
  eol
  `(:term ,term))

(defparser definition ()
  (-> (many1 definition-paragraph) paragraphs)
  `(:definition ,@paragraphs))

(defparser definition-paragraph ()
  (try (p indentation (not-followed-by "% ")))
  (-> paragraph))

(defparser blockquote ()
  (indented 2)
  (-> (many1 blockquote-element) es)
  `(:blockquote ,@es))

(defparser blockquote-element ()
  (try (not-followed-by (p (count 3 " ") (none-of " "))))
  (-> element))

(defparser linkdef ()
  (-> (between "[" "]" (chars-until "]")) name) " "
  (-> (between "<" ">" (many (none-of ">"))) link) blank
  `(:link ,name ,link))

(defparser section-divider ()
  whitespace "§" blank `(:section-divider))

(defparser paragraph ()
  (-> paragraph-text text)
  `(:paragraph ,text))

(defparser modeline ()
  (try "-*-")
  (many (p (not-followed-by blank) any-char))
  blank)

(defparser paragraph-text ()
  (-> (many1 (or (text-until (or (tagged-or "[") blank)) tagged-text linkref)) text)
  blank
  text)

(defparser linkref ()
  "["
  (-> link-contents contents)
  (-> (option-maybe link-key) maybe-key)
  "]"
  `(:link ,contents ,maybe-key))

(defparser link-contents ()
  (-> (many1 (text-until (or (tagged-or (one-of "|]")) tagged-text)))))

(defparser link-key ()
  "|" (-> (many1 (none-of "]")) r) r)

(defparser escaped-char ()
  (try (p "\\" (one-of "\\{}*#-[]%|<"))))

(defparser newline-char ()
  (not-followed-by blank)
  newline
  indentation
  #\Newline)

(defparser plain-char ()
  (-> (in-subdoc (none-of "}") any-char) c)
  c)

(defparser tag-open ()
  (not-followed-by escaped-char) "\\"
  (-> name name)
  "{"
  name)

(defparser tagged-text ()
  (-> tag-open name)
  (-> (if (member name (state subdocs))
          (subdoc-contents name)
          (simple-contents name))
      element)
  "}"
  element)

(defparser subdoc-contents (name)
  (with-state ((subdoc-level (+ 1 subdoc-level)))
    (-> (many1 element) elements)
    eod
    `(,name ,@elements)))

(defparser simple-contents (name)
  (-> (many1 (text-until (or (tagged-or "}") tagged-text))) contents)
  `(,name ,@contents))

(defparser tagged-or (p) (or tag-open p) nil)

(defparser name () (many1 letter))

(defparser listy (name marker)
  (->
   (indented 2 (-> (many1 (try (p indentation (-> (list-element marker))))) c))
   contents)
  `(,name ,@contents))

(defparser list-element (marker)
  (try (p (char marker) " "))
  (with-state ((current (+ current 2)))
    (-> (many1 (p indentation (-> (or ordered-list unordered-list paragraph)))) contents)
    `(:li ,@contents)))

;;; Whitespace and indentation handling

(defparser whitespace () (many (one-of " \t")))

(defparser eol () whitespace newline)

(defparser eod () (in-subdoc end-of-subdoc eof))

(defparser end-of-subdoc () (look-ahead "}"))

(defparser blank ()
  (or eol eod)
  (or (many1 (try eol)) eod))

(defparser indentation ()
  ;; I'm confused why this is a TRY. -Peter
  (try (count (- (state current) (state so-far)) " "))
  (setf (state so-far) (state current)))

(defparser indented (n p)
  (with-state ((current (+ current n)))
    (try (look-ahead (count n " ")))
    (-> p)))

(defparser newline ()
  (setf (state so-far) 0)
  #\Newline)

;;; Combinators

(defparser text-until (p) (chars-until p))

(defparser chars-until (p)
  (many1 (p (not-followed-by p) (-> (or escaped-char newline-char plain-char)))))

(defparser in-subdoc (p1 p2)
  (if (> (state subdoc-level) 0) p1 p2))

;;; Standard combinators



;;; Utility functions

(defun drop-trailing-blanks (lines)
  "Drop all the trailing blank lines and also the \n's on the end of the last line."
  ;; TODO: implement
  lines)
