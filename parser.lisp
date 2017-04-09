;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)


;;; XXX - can implement backtracking state by compiling in
;;; save/restore of state that happens whenever a parse fails.

(defmacro defparser (name (&rest args) &body body)
  (compile-parser name args body))

(defun decompile (p)
  "Mostly for debugging. Essentially the same as macro expanding."
  (destructuring-bind (defparser name (&rest args) &rest body) p
    (declare (ignore defparser))
    (compile-parser name args body)))


(defun compile-parser (name arglist body)
  (let* ((productions (mapcar #'normalize-production body))
         (names (mapcar #'caar productions)))
    (multiple-value-bind (args state-vars) (extract-state-variables arglist)
      (let ((state (append args (mapcar #'just-name state-vars))))
        (with-gensyms (txt pos)
          `(defun ,name (,txt ,pos ,@args)
             (let (,@state-vars)
               (labels ((show-state (&optional label)
                          (tracemsg "~a: ~@{~(~a~): ~a~^; ~}"
                                    (or label "STATE")
                                    ,@(loop for n in state collect `(quote ,n) collect n)
                                    "pos" ,pos))
                        ,@(mapcar
                           #'(lambda (p) (compile-production p names state))
                           productions))
                 (,(first names) ,txt ,pos)))))))))

(defun compile-production (p names state)
  (with-gensyms (txt pos)
    (destructuring-bind ((name &rest args) &rest body) p
      `(,name (,@args ,txt ,pos)
              (declare (ignorable ,txt))
              ,(compile-progn body txt pos names (gensym "R") state)))))

(defun just-name (x)
  (typecase x
    (symbol x)
    (cons (first x))))

(defun extract-state-variables (args)
  (let ((state (member `&state args)))
    (if state
        (values (ldiff args state) (cdr state))
        (values args nil))))

(defun save-state-bindings (names)
  (mapcar #'(lambda (x) `(,(gensym (symbol-name x)) ,x)) names))

(defun restore-state (bindings)
  (mapcar #'(lambda (x) `(setf ,@(reverse x))) bindings))

(defun normalize-production (p)
  (destructuring-bind (x &rest body) p
    `(,(typecase x
         (cons x)
         (symbol (list x)))
       ,@body)))

(defun grammar-p (name names)
  (or (member name names) (get name 'parser-function)))

(defun good (result position) (values t result position))

(defun bad (position) (values nil nil position))


(defun compile-progn (body txt pos names result state)
  "Compile the forms in an PROGN so that the PROGN matches if each of
the elements matches in sequence."
  (unless (null body)
    (with-gensyms (p)
      (compile-wrapped-form
       #'progn-wrapper
       (first body)
       txt pos p names result state
       (compile-progn (rest body) txt p names (gensym "R") state)))))

(defun progn-wrapper (expr ok result p continuation failure state)
  (if (null continuation)
      expr
      (let ((bindings (save-state-bindings state)))
        `(let (,@bindings)
           (multiple-value-bind (,ok ,result ,p) ,expr
             (declare (ignorable ,result))
             (if ,ok
                 ,(or continuation `(good ,result ,p))
                 (progn
                   ,@(restore-state bindings)
                   ,failure)))))))

(defun compile-or (body txt pos names result state)
  "Compile the forms in an OR so that the OR matches if any of the
elements matches, returning the result from the first match."
  (unless (null body)
    (with-gensyms (p)
      (compile-wrapped-form
       #'or-wrapper
       (first body)
       txt pos p names result state
       (compile-or (rest body) txt pos names (gensym "R") state)))))

(defun or-wrapper (expr ok result p continuation failure state)
  (let ((bindings (save-state-bindings state)))
    `(let (,@bindings)
       (multiple-value-bind (,ok ,result ,p) ,expr
         (if ,ok
             (good ,result ,p)
             (progn
               ,@(restore-state bindings)
               ,(or continuation failure)))))))

(defun compile-wrapped-form (wrapper form text p-in p names r state continuation)
  (with-gensyms (ok)
    (let ((failure `(bad ,p-in)))
      (labels ((wrap (expr) (funcall wrapper expr ok r p continuation failure state))
               (self (expr) (compile-wrapped-form wrapper expr text p-in p names (gensym "R") state nil)))
          (cond

            ((parser-function-invocation-p form names)
             (wrap (compile-parser-call form names text p-in state)))

            ((form-p '-> form)
             (destructuring-bind (expr var) (cdr form)
               (funcall wrapper (self expr) ok var p continuation failure state)))

            ((form-p 'progn form)
             (wrap (compile-progn (rest form) text p-in names r state)))

            ((form-p 'or form)
             (wrap (compile-or (rest form) text p-in names r state)))

            ((form-p 'match form)
             (wrap (compile-match (cadr form) text p-in)))

            ((form-p 'if form)
             (destructuring-bind (test then else) (cdr form)
               (wrap `(if ,test ,(self then) ,(self else)))))

            ((stringp form)
             (wrap (compile-string form text p-in)))

            ((characterp form)
             (wrap (compile-character form text p-in)))

            (t (wrap `(good ,form ,p-in))))))))

(defun compile-expression (exp names state)
  (with-gensyms (text position)
    (labels ((thunk (exp)
               (if (and (consp exp) (grammar-p (car exp) names) (= (length exp) 3))
                   `(function ,(car exp))
                   `(lambda (,text ,position)
                      (declare (ignorable ,text))
                      ,exp))))
      (cond
        ((parser-function-invocation-p exp names)
         (thunk (compile-parser-call exp names text position state)))

        ((form-p 'progn exp)
         (thunk (compile-progn (rest exp) text position names (gensym "R") state)))

        ((form-p 'or exp)
         (thunk (compile-or (rest exp) text position names (gensym "R") state)))

        ((form-p 'if exp)
         (destructuring-bind (test then else) (cdr exp)
           `(if ,test
                ,(compile-expression then names state)
                ,(compile-expression else names state))))

        ((stringp exp)
         (thunk (compile-string exp text position)))

        ((characterp exp)
         (thunk (compile-character exp text position)))

        (t exp)))))

(defun compile-string (str text position)
  `(matching ,str ,(length str) ,text ,position))

(defun compile-character (char text position)
  `(matching-char ,char ,text ,position))

(defun matching (s len text position)
  (let* ((end (+ len position))
         (ok (if (<= end (length text)) (string= s text :start2 position :end2 end))))
    (if ok
        (good s end)
        (bad position))))

(defun matching-char (c text position)
  (if (< position (length text))
      (if (char= c (char text position))
          (good c (1+ position))
          (bad position))
      (bad position)))


(defun compile-match (parser txt p)
  `(funcall ,parser ,txt ,p))

(defun compile-parser-call (exp names txt p state)
  (destructuring-bind (name &rest args) (rewrite-form exp names)
    (flet ((comp (x) (compile-expression x names state)))
      `(,name ,@(mapcar #'comp args) ,txt ,p))))

(defun rewrite-form (form names)
  "Rewrite bare symbols naming parser productions or parser functions
into cannonical list from."
  (cond
    ((and (symbolp form) (grammar-p form names)) (list form))
    (t form)))

(defun parser-function-invocation-p (form names)
  (or
   (and (symbolp form) (grammar-p form names))
   (and (consp form) (symbolp (car form)) (grammar-p (car form) names))))

(defun form-p (what form)
  (and (consp form) (eql (car form) what)))

(defmacro defparserfun (name (&rest args) &body body)
  `(progn
     (setf (get ',name 'parser-function) t)
     (defun ,name (,@args) ,@body)))

(defparserfun any-char (text position)
  "Match a single character. Succeeds everywhere except at the EOF."
  (if (< position (length text))
      (good (char text position) (1+ position))
      (bad position)))

(defparserfun try (p text position)
  "Attempt to parse using p, moving forward if it succeeds. However if
it fails, it does not consume any input."
  (multiple-value-bind (ok r np) (funcall p text position)
    (if ok
        (good r np)
        (bad position))))

(defparserfun many (parser text position)
  "Match P as many times as possible. Always succeeds as zero is an
acceptable number of times to match."
  (loop with r = nil
     while t do
       (multiple-value-bind (ok result new-position) (funcall parser text position)
         (cond
           (ok
            (push result r)
            (setf position new-position))
           (t
            (return (good (nreverse r) position)))))))

(defparserfun many1 (parser text position)
  "Match P as many times as possible but at least once."
  (multiple-value-bind (ok r p) (funcall parser text position)
    (if ok
        (multiple-value-bind (ok r2 p) (many parser text p)
          (if ok
              (good (cons r r2) p)
              (bad position))))))

(defparserfun not-followed-by (p text position)
  "Match only if not followed by P."
  (values (not (funcall p text position)) nil position))

(defparserfun optional (p text position)
  "Match P if we can."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore ok))
    (values t r pos)))

(defparserfun counted (n p text position)
  "Match P N times."
  (if (zerop n)
      (good t position)
      (multiple-value-bind (ok r pos) (funcall p text position)
        (declare (ignore r))
        (if ok
            (counted (1- n) p text pos)
            (bad position)))))

(defparserfun look-ahead (p text position)
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore r pos))
    (if ok
        (good nil position)
        (bad position))))

(defparserfun eof (text position)
  (if (<= (length text) position)
      (good t position)
      (bad position)))

(defparserfun epsilon (text position)
  (declare (ignore text))
  (good nil position))

(defparserfun text (p text position)
  "Capture the text matched by P."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore r))
    (if ok
        (good (subseq text position pos) pos)
        (bad position))))

(defparserfun ! (p text position)
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore r pos))
    (if ok
        (values nil nil position)
        (values t (char text position) (1+ position)))))

(defparserfun none-of (p text position)
  "P really should match a single character."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore r pos))
    (if ok
        (values nil nil position)
        (values t (char text position) (1+ position)))))

(defparserfun char-if (pred text position)
  (let ((c (char text position)))
    (if (funcall pred c)
        (good c (1+ position))
        (bad position))))

(defparserfun ensure (p1 p2 text position)
  (multiple-value-bind (ok r pos) (funcall p1 text position)
    (funcall p2 text position)
    (values ok r pos)))

(defvar *trace* 0)
(defvar *trace-level* 0)

(defun trace-on () (incf *trace*))
(defun trace-off () (decf *trace*))

(defparserfun tracing (p text position)
  (trace-on)
  (unwind-protect (funcall p text position)
    (trace-off)))

(defparserfun tracer (p name text position)
  (let ((indent (make-string (* 2 *trace-level*) :initial-element #\Space))
        (*trace-level* (1+ *trace-level*)))
    (when (plusp *trace*)
      (format t "~&~aMatching ~a at ~d ..." indent name position))

    (multiple-value-bind (ok r p) (funcall p text position)
      (when (plusp *trace*)
        (if ok
            (format t "~&~a~a at ~d: MATCHED: ~s" indent name position r)
            (format t "~&~a~a at ~d: No match." indent name position)))

      (values ok r p))))

(defun tracemsg (fmt &rest args)
  (when (plusp *trace*)
    (let ((indent (make-string (* 2 *trace-level*) :initial-element #\Space)))
      (format t "~&~a" indent)
      (apply #'format t fmt args))))

(defun value (x) x)
