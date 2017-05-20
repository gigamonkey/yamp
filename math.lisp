(in-package :com.gigamonkeys.yamp)

(defparser math ()

  (expression
   (-> term first)
   (-> (optional (and (token "+") expression)) rest)
   (combine '+ first rest))

  (term
   (-> factor first)
   (-> (optional (and (token "*") term)) rest)
   (combine '* first rest))

  (factor (or parenthesized number))

  (number (=> (text (many1 digit)) (parse-integer _)))

  (parenthesized (token "(") (=> expression) (token ")"))

  (digit (? any-char #'digit-char-p))

  ((token p) whitespace (match p) whitespace)

  (whitespace (many (or #\Space #\Tab #\Newline))))

(defun combine (op first rest)
  (cond
    ((null rest) first)
    ((and (consp rest) (eql (car rest) op)) `(,op ,first ,@(cdr rest)))
    (t `(,op ,first ,rest))))
