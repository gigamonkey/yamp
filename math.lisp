(in-package :com.gigamonkeys.yamp)

(defparser math ()

  (math (=> expression (simplify _)))

  (expression
   (-> term first)
   (-> (or plus-more epsilon) rest)
   (combine '+ first rest))

  (term
   (-> factor first)
   (-> (or times-more epsilon) rest)
   (combine '* first rest))

  (plus-more plus expression)

  (times-more times term)

  (factor (or parenthesized number))

  (number (=> (text (many1 digit)) (parse-integer _)))

  (parenthesized lparen (=> expression) rparen)

  (lparen whitespace "(" whitespace)

  (rparen whitespace ")" whitespace)

  (plus whitespace "+" whitespace)

  (times whitespace "*" whitespace)

  (digit (char-if #'digit-char-p))

  (whitespace (many (or #\Space #\Tab #\Newline)))

  (epsilon nil))

(defun combine (op first rest)
  (if rest (list op first rest) first))

(defun simplify (expr)
  (if (consp expr)
      (let ((expr `(,(car expr) ,@(mapcar #'simplify (cdr expr)))))
        (if (and (consp (caddr expr))
                 (eql (car expr) (caaddr expr)))
            `(,(car expr) ,(cadr expr) ,@(cdaddr expr))
            expr))
      expr))
