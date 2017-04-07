(in-package :com.gigamonkeys.yamp)

(defparser math ()

  (math (-> expression e) (simplify e))

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

  (number
   (-> (text (many1 digit)) digits)
   (parse-integer digits))

  (parenthesized
   whitespace "(" (-> expression e) ")" whitespace e)

  (plus whitespace "+" whitespace)

  (times whitespace "*" whitespace)

  (digit (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

  (whitespace (many (or #\Space #\Tab #\Newline))))

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
