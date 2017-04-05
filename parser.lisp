;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

#|

* Evaluation rules

SYMBOL naming a production or external parser

  => `(lambda (text state position) (,SYMBOL text state position))

(SYMBOL &rest args) where SYMBOL names a production or an external parser

  => `(lambda (text state position) (,SYMBOL ,@args text state position))

SYMBOL not naming a production or external parser. This is an error at
the top-level of a production except as the last form.

  => SYMBOL

(SYMBOL &rest args) where SYMBOL does not name a production or an
external parser. This is an error at the top-level of a production
except as the last form.

  => (,SYMBOL ,@args)

(MATCH "STRING")

  => (string-matcher "STRING")

(MATCH #\Character)

  => (character-matcher #\Character)

(OR ...)

  =>


(PROGN p), p a parser function

  => (lambda (text state position)
       (funcall p text state position))

(PROGN x), x not a parser function

  => (lambda (text state position)
       (declare (ignore text))
       (values t x state position))


(PROGN p1 p2 ... x) where X evaluates to a production or parser

  => (lambda (text state position)
       (multiple-value-bind (ok r s pos) (funcall ,p1 text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall ,p2 text s p)
               ...
               (if ok
                   (multiple-value-bind (ok r s pos) (funcall ,x text s p)
                     (if ok
                         (values t r s p)
                         (values nil nil state position))))))))


(PROGN p1 p2 ... x) where X does not evaluate to a production or parser

  => (lambda (text state position)
       (multiple-value-bind (ok r s pos) (funcall ,p1 text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall ,p2 text s p)
               (if ok
                   (values t ,x s p)
                   (values nil nil state position))))))


(-> p SYMBOL)

  => (multiple-value-bind (ok SYMBOL s pos) (funcall p text state position)
       (if ok
           <continuation>
            (t (values nil nil state position))))))


(STATE SYMBOL)

  => (cdr (assoc symbol state))

(SETF (STATE SYMBOL) VALUE)

  => (setf state (acons symbol value state))

Examples:

(many (try eol))

  => (lambda (text state position)
       (many (lambda (text state position)
               (try
                (lambda (text state position) (eol text state position))))
             text
             state
             position))

(many (p foo (-> bar res) baz res))

  => (lambda (text state position)
        (many (lambda (text state position)
                (multiple-value-bind (ok r s pos) (funcall foo text state position)
                  (if ok
                      (multiple-value-bind (ok res s pos) (funcall bar text state position)
                        (if ok
                            (multiple-value-bind (ok r s pos) (funcall baz text state position)
                              (if ok
                                  (values t res s pos)
                                  (values nil nil state pos)))
                            (value nil nil state pos)))
                      (values nil nil state pos))))
              text state position))

(count 3 " ")

  => (lambda (text state position)
       (count 3 (lambda (text state position) (match-string " " text state position))))

(progn foo bar)

  => (lambda (text state position)
       (multiple-value-bind (ok x s pos) (funcall foo text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall bar text state position)
               (if ok
                   (values t r s pos)
                   (values nil nil state position)))
             (values nil nil state position))))


(progn (-> foo x) `(:whatever x))

  => (lambda (text state position)
       (multiple-value-bind (ok x s pos) (funcall foo text state position)
         (if ok
             (values t `(:whatever ,x) s pos)
             (values nil nil state position))))

(or foo bar)

  => (multiple-value-bind (ok r s pos)
         (foo text state position)
       (if ok
           (values t r s pos) ;; success
           (multiple-value-bind (ok r s pos)
               (bar text state position)
             (if ok
                 (values t r s pos) ;; success
                 (values nil nil state position)))))



(STATE :name) (except in SETF, et al. forms) access the named
value in the current (i.e. passed in) state.

(SETF (STATE :name) ...) modify the named value in the new state.
(INCF (STATE :name) ...) modify the named value in the new state.
(DECF (STATE :name) ...) modify the named value in the new state.

|#

#+(or)(defmacro defparser (name (&rest args) &body body)
  ;; Should check for duplicate names and error.
  `(defun ,name (,@args)
     (let (,@(extract-bound-names body))
       ;; Mark starting point.

       ;; Evaluate each form but the last as a parser. If it matches,
       ;; evaluate next form with new starting point.

       ;; For binding forms (-> ...), assign the result of the parser to
       ;; the appropriate name.

       ;; Evaluate the last form
       ,last-form)))

#+(or)(defun foo (form)
  (destructuring-bind (defparser name (&rest args) &rest body) form
    (declare (ignore defparser name args))
    (let* ((symbols (uniq-names body))
           (names (defined-names body)))
      (format t "~{~&~(~a~)~}" (sort (remove-if #'fboundp (set-difference symbols names)) #'string<)))))


(defun compile-parser (p)
  (destructuring-bind (defparser name (&rest args) &rest body) p
    (declare (ignore defparser))
    (%compile-parser name args body)))

(defun %compile-parser (name args body)
  (let* ((productions (mapcar #'production body))
         (names (mapcar #'(lambda (x) (getf x :name)) productions)))
    `(defun ,name (text position ,@args)
       (flet (,@(loop for p in productions collect
                    (destructuring-bind (&key name args body) p
                      `(,name (,@args text state position)
                              ,(compile-progn body 'text 'state 'position names)))))
         (,(first names)
           text
           (list ,@(mapcar #'(lambda (x) `(cons ',x ,x)) args))
           position)))))

(defun production (p)
  "A parser is made up of productions."
  (let ((name nil)
        (extra-args nil))
    (destructuring-bind (x &rest body) p
        (typecase x
          (cons
           (setq name (car x))
           (setq extra-args (cdr x)))
          (symbol
           (setq name x))
          (t (error "Name must be SYMBOL or LIST.")))
      `(:name ,name :args ,extra-args :body ,body))))

(defun production-names (p)
  "The names of the productions defined in the parser."
  (mapcar #'(lambda (x) (getf x :name)) (mapcar #'production (cdddr p))))

(defun grammar-p (name names)
  (or (member name names) (get name 'parser-function)))

(defun compile-production (p names)
  (destructuring-bind (&key name args body) p
    `(:name ,name :function (lambda (,@args text state position) ,(compile-progn body 'text 'state 'position names)))))

(defun compile-progn (body txt st pos names)
  (unless (null body)
    (with-gensyms (s p)
      (compile-form-in-progn
       (first body)
       txt st pos s p names
       (compile-progn (rest body) txt s p names)))))

(defun compile-form-in-progn (form text orig-state orig-position s p names continuation)
  (with-gensyms (ok)
      (let ((form (rewrite-form form names))
            (failure `(values nil nil ,orig-state ,orig-position)))
        (flet ((comp (form result)
                 `(multiple-value-bind (,ok ,result ,s ,p) ,(compile-parser-function-invocation form names text orig-state orig-position)
                    (if ,ok
                        ,(or continuation `(values t ,result ,s ,p))
                        ,failure))))
          (cond
            ((parser-function-invocation-p form names)  (comp form (gensym "R")))
            ((binding-form-p form) (comp (rewrite-form (cadr form) names) (caddr form)))
            (t form))))))

(defun compile-or (body txt st pos names)
  (unless (null body)
    (with-gensyms (s p)
      (compile-form-in-or
       (first body)
       txt st pos s p names
       (compile-or (rest body) txt s p names)))))

(defun compile-form-in-or (form text orig-state orig-position s p names continuation)
  (with-gensyms (ok)
    (let ((form (rewrite-form form names))
          (failure `(values nil nil ,orig-state ,orig-position)))
      (flet ((comp (form result)
                 `(multiple-value-bind (,ok ,result ,s ,p) ,(compile-parser-function-invocation form names text orig-state orig-position)
                    (if ,ok
                        (value t ,result ,s ,p)
                        ,(or continuation failure)))))
        (cond
          ((parser-function-invocation-p form names)  (comp form (gensym "R")))
          ((binding-form-p form) (error "Binding forms not allowed in OR expressions."))
          (t (error "Expression ~a not allowed in OR expression" form)))))))

(defun compile-expression (exp names)
  (cond
    ((parser-function-invocation-p exp names)
     (compile-parser-function-expression (rewrite-form exp names) names))
    ((and (consp exp) (eql (car exp) 'progn))
     (with-gensyms (text state position)
       `(lambda (,text ,state ,position)
          ,(compile-progn (rest exp) text state position names))))
    ((and (consp exp) (eql (car exp) 'or))
     (with-gensyms (text state position)
       `(lambda (,text ,state ,position)
          ,(compile-or (rest exp) text state position names))))
    (t exp)))

(defun compile-parser-function-expression (exp names)
  (destructuring-bind (name &rest args) exp
    (if args
        (with-gensyms (txt s p)
          `(lambda (,txt ,s ,p) ,(compile-parser-function-invocation exp names txt s p)))
        `(function ,name))))

(defun compile-parser-function-invocation (exp names txt s p)
  (destructuring-bind (name &rest args) exp
    (flet ((comp (x) (compile-expression x names)))
      `(,name ,@(mapcar #'comp args) ,txt ,s ,p))))

(defun rewrite-form (form names)
  (cond
    ((parser-function-symbol-invocation-p form names) (list form))
    (t form)))

(defun parser-function-symbol-invocation-p (form names)
  (and (symbolp form) (grammar-p form names)))

(defun parser-function-invocation-p (form names)
  (and (consp form) (symbolp (car form)) (grammar-p (car form) names)))

(defun binding-form-p (form)
  (and (consp form) (eql (car form) '->)))



(defun uniq-names (form)
  (cond
    ((consp form)
     (sort (delete-duplicates
            (let ((s (car form))
                  (rest (mapcan #'uniq-names (remove-if (complement #'consp) (cdr form)))))
              (if (and (symbolp s) (not (keywordp s)))
                  (cons (car s) rest)
                  rest)))
           #'string<))
    (t nil)))


(defun call-parser (p text state position)
  (funcall p text state position))


(defmacro defparserfun (name (&rest args) &body body)
  `(progn
     (setf (get ',name 'parser-function) t)
     (defun ,name (,@args) ,@body)))


(defparserfun any-char (text state position)
  "Match a single character. Succeeds everywhere except at the EOF."
  (if (< position (length text))
      (values t (char text position) state (1+ position))
      (values nil nil state position)))

(defparserfun try (p text state position)
  "Attempt to parse using p, moving forward if it succeeds. However if
it fails, it does not consume any input."
  (multiple-value-bind (ok r ns np) (call-parser p text state position)
    (if ok
        (values t r ns np)
        (values nil nil state position))))

(defparserfun many (p text state position)
  "Match P as many times as possible. Always succeeds as zero is an
acceptable number of times to match."
  (loop with r = nil
     while t do
       (multiple-value-bind (ok result new-state new-position)
           (call-parser p text state position)
         (cond
           (ok
            (push r result)
            (setf state new-state)
            (setf position new-position))
           (t
            (return (values t (nreverse r) state position)))))))

(defparserfun many1 (p text state position)
  "Match P as many times as possible but at least once."
  (multiple-value-bind (ok r s p) (many p text state position)
    (if (and ok r)
        (values t r s p)
        (values nil nil state position))))

(defparserfun not-followed-by (p text state position)
  "Match only if not followed by P."
  (multiple-value-bind (ok r s pos)
      (call-parser p text state position)
    (declare (ignore r s pos))
    (values (not ok) text state position)))

(defparserfun optional (p text state position)
  "Match P if we can. Doesn't return anything. Only fails if P
consumes input before failing."
  (multiple-value-bind (ok r s pos)
      (call-parser p text state position)
    (declare (ignore ok r))
    (values (= pos position) t nil s pos)))

(defparserfun counted (n p text state position)
  "Match P N times."
  (if (zerop n)
      (values text state position)
      (multiple-value-bind (ok r s pos) (funcall p text state position)
        (declare (ignore r))
        (if ok
            (counted (1- n) p text s pos)
            (values nil nil state position)))))
