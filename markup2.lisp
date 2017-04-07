;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

;; Arguments to a parser get added to the inital state.

(defparameter *p*
  '(defparser markup (subdocs &state (current 0) (so-far 0) (subdoc-levels 0))

    (document
     (optional modeline)
     (many (try eol))
     (-> (many element) paragraphs)
     eod
     `(:document ,@paragraphs))

    (element
     indentation
     (or header
      section
      verbatim
      ordered-list
      unordered-list
      dlist
      blockquote
      (try linkdef)
      section-divider
      paragraph))

    (header
     (-> (many1 "*") stars)
     whitespace
     (-> paragraph-text text)
     `(:header ,(length stars) ,text))

    (section
     "## " (-> name name) (many1 (try eol))
     (-> (many section-body) paragraphs)
     "##." blank
     `(:section ,name ,@paragraphs))

    (verbatim
     (indented 3 (-> verbatim-text text))
     `(:verbatim ,text))

    (verbatim-text
     (-> (many1 (or verbatim-blank-line verbatim-line)) lines)
     (format nil "~{~&~a~}" (drop-trailing-blanks lines)))

    (verbatim-blank-line (try eol) #\Newline)

    (verbatim-line
     indentation
     (-> (many1 (progn (not-followed-by eol) any-char)) text)
     (or eol (try eod))
     (apply #'concatenate 'string text))

    (ordered-list (listy :ol "#"))

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
     (try (not-followed-by (progn (counted 3 " ") (none-of " "))))
     element)

    (linkdef
     (-> (between "[" "]" (text-until "]")) name) " "
     (-> (between "<" ">" (many (none-of ">"))) link) blank
     `(:link ,name ,link))

    (section-divider whitespace "§" blank `(:section-divider))

    (paragraph
     (-> paragraph-text text)
     `(:paragraph ,text))

    (modeline
     (try "-*-")
     (many (progn (not-followed-by blank) any-char))
     blank
     nil)

    (paragraph-text
     (-> (many1 (or (text-until (tagged-or (or "[" blank))) tagged-text linkref)) text)
     blank
     text)

    (linkref
     "["
     (-> link-contents contents)
     (-> (option-maybe link-key) maybe-key)
     "]"
     `(:link ,contents ,maybe-key))

    (link-contents
     (many1 (or (text-until (tagged-or (one-of "|]"))) tagged-text)))

    (link-key
     "|" (-> (many1 (none-of "]")) r) r)

    (escaped-char
     (try (progn "\\" (one-of "\\{}*#-[]%|<"))))

    (newline
     (not-followed-by blank)
     (-> newline-char nl)
     indentation
     nl)

    (plain-char
     (in-subdoc (none-of "}") any-char))

    (tag-open
     (not-followed-by escaped-char) "\\"
     (-> name name)
     "{"
     name)

    (tagged-text
     (-> tag-open n)
     (-> (if (member n subdocs)
             (subdoc-contents n)
             (simple-contents n))
      element)
     "}"
     element)

    ((subdoc-contents n)
     (incf subdoc-level)
     (-> (many1 element) elements)
     eod
     (decf subdoc-level)
     `(,n ,@elements))

    ((simple-contents name)
     (-> (many1 (or (text-until (tagged-or "}")) tagged-text)) contents)
     `(,name ,@contents))

    ((tagged-or p) (or tag-open p) nil)

    (name (many1 letter))

    ((listy name marker)
     (-> (indented 2 (many1 (try (progn indentation (list-element marker))))) contents)
     `(,name ,@contents))

    ((list-element marker)
     (try (progn (char marker) " "))
     (incf current 2)
     (-> (many1 (progn indentation (-> (or ordered-list unordered-list paragraph)))) contents)
     (decf current 2)
     `(:li ,@contents))

    ;; Whitespace and indentation handling

    (whitespace (many (one-of " \t")))

    (eol whitespace newline-char)

    (eod (in-subdoc end-of-subdoc eof))

    (end-of-subdoc (look-ahead "}"))

    (blank
     (or eol eod)
     (or (many1 (try eol)) eod))

    (indentation
     (try (counted (- current so-far) " "))
     (setf so-far current))

    ((indented n p)
     (incf current n)
     (try (look-ahead (counted n " ")))
     (-> p r)
     (decf current n))
     r)

    (newline-char
     (setf so-far 0)
     #\Newline)

    ((text-until p) (many1 (progn (not-followed-by p) (or escaped-char newline plain-char))))

    ((in-subdoc p1 p2)
     (if (> subdoc-level 0) (match p1) (match p2)))

    ((between start end p)
     (match start)
     (-> (match p) r)
     (match end)
     r)))
