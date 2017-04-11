;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)


;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro defparser (name (&rest args) &body body)
  "Define a parser named NAME. The argument list can be used to specify
parameters the parser function will accept in addition to text and position
arguments. Any arguments after &state will be bound at the beginning of the
function and can be used by productions within the grammar to maintain state
which will backtrack when the parser does."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'parser-function) t))
     ,(compile-parser name args body)))


(defmacro defparserfun (name (&rest args) &body body)
  "Macro for defining hand-written functions that act as parsers. Used
internally to define things that can't be expressed as normal parsers."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'parser-function) t))
     (defun ,name (,@args) ,@body)))

(defun value (x)
  "Return X as a value rather than interpreting it as a parser. Mostly useful
for returning specific characters or strings."
  x)


;;; Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compile-parser (name arglist body)
  (let* ((productions (mapcar #'normalize-production body))
         (names (mapcar #'caar productions)))
    (multiple-value-bind (args initial-state) (extract-state-variables arglist)
      (let ((state (append args (mapcar #'car initial-state))))
        (with-gensyms (txt pos)
          (flet ((prod-compiler (p) (compile-production p names state)))
            `(defun ,name (,@args ,txt ,pos)
               (let (,@initial-state)
                 (labels ((show-state (&optional label)
                            (tracemsg "~a: ~@{~(~a~): ~a~^; ~}"
                                      (or label "STATE")
                                      ,@(loop for n in state collect `(quote ,n) collect n)
                                      "pos" ,pos))
                          ,@(mapcar #'prod-compiler productions))
                   (,(first names) ,txt ,pos))))))))))

(defun normalize-production (p)
  "Make sure a production is of the form ((foo ...) ...)"
  `(,(mklist (car p)) ,@(cdr p)))

(defun extract-state-variables (args)
  "Split the args from the initial state binding forms. Also normalize the
binding forms."
  (let ((state (member `&state args)))
    (if state
        (values
         (ldiff args state)
         (mapcar #'(lambda (x) (if (symbolp x) (list x nil) x)) (cdr state)))
        (values args nil))))

(defun compile-production (p names state)
  "Compile a production into a function definition appropriate for a LABELS
binding."
  (with-gensyms (txt pos)
    (destructuring-bind ((name &rest args) &rest body) p
      `(,name (,@args ,txt ,pos)
         (declare (ignorable ,txt))
         ,(compile-and body txt pos names (gensym "R") state)))))

(defun save-state-bindings (names)
  "Generate bindings to save the current value of the state variables."
  (mapcar #'(lambda (x) `(,(gensym (symbol-name x)) ,x)) names))

(defun restore-state (bindings)
  "Given bindings produced by SAVE-STATE-BINDINGS, generate a list of SETF forms
that will restore the saved values."
  (mapcar #'(lambda (x) `(setf ,@(reverse x))) bindings))

(defun grammar-p (name names)
  "If a symbol either names a production within the grammar or an external
parser function."
  (or (member name names) (get name 'parser-function)))

(defun compile-and (body txt pos names result state)
  "Compile the forms in an AND so that the AND matches if each of the elements
matches in sequence."
  (unless (null body)
    (let ((body (rewrite-returns body)))
      (with-gensyms (p)
        (compile-wrapped-form
         #'and-wrapper
         (first body)
         txt pos p names result state
         (compile-and (rest body) txt p names (gensym "R") state))))))

(defun rewrite-returns (and-body)
  "Rewrite the list of forms to be compiled into a AND so that a (=> ...) form
will have its result returned as the result of the AND. If the => form specifies
a value expression it can refer to the value matched by the parser via the
variable _. Otherwise the value of the parser is returned by the AND."
  (let (result)
    (labels ((return-form (x) (and (consp x) (eql (car x) '=>)))

             (translate (x r &optional s)
               (setq result r)
               `(-> ,(cadr x) ,(or s r)))

             (rewrite (x)
               (if (return-form x)
                   (cond
                     (result (error "Two => forms in body: ~s" and-body))
                     ((cddr x) (translate x (caddr x) '_))
                     (t (translate x (gensym "R"))))
                   x)))

      (let ((new-body (mapcar #'rewrite and-body)))
        (if result `(,@new-body ,result) and-body)))))

(defun and-wrapper (expr ok result p continuation failure state)
  "Wrap an expression to be part of a AND. Arranges to save the state, evaluate
the expression. If it succeeds, invoke the continuation of the AND, if there is
one, or succeed, returning the value and position returned by the expression.
Otherwise fail, rolling back the state. "
  (if (null continuation)
      expr
      (let ((state-bindings (save-state-bindings state)))
        `(let (,@state-bindings)
           (multiple-value-bind (,ok ,result ,p) ,expr
             (declare (ignorable ,result))
             (if ,ok
                 ,(or continuation `(good ,result ,p))
                 (progn
                   ,@(restore-state state-bindings)
                   ,failure)))))))

(defun compile-or (body txt pos names result state)
  "Compile the forms in an OR so that the OR matches if any of the elements
matches, returning the result from the first match."
  (unless (null body)
    (with-gensyms (p)
      (compile-wrapped-form
       #'or-wrapper
       (first body)
       txt pos p names result state
       (compile-or (rest body) txt pos names (gensym "R") state)))))

(defun or-wrapper (expr ok result p continuation failure state)
  "Wrap an expression to be part of an OR. Arranges to save the state,
evaluate the expression, and then either return success or invoke the
continuation of the OR, if there is one, or fail and roll back the state."
  (let ((state-bindings (save-state-bindings state)))
    `(let (,@state-bindings)
       (multiple-value-bind (,ok ,result ,p) ,expr
         (if ,ok
             (good ,result ,p)
             (progn
               ,@(restore-state state-bindings)
               ,(or continuation failure)))))))

(defun compile-wrapped-form (wrapper form text p-in p names r state continuation)
  "Used to compile forms that are part of ANDs and ORs."
  (with-gensyms (ok)
    (let ((failure nil))
      (labels ((wrap (expr) (funcall wrapper expr ok r p continuation failure state))
               (self (expr) (compile-wrapped-form wrapper expr text p-in p names (gensym "R") state nil)))
          (cond

            ((parser-function-invocation-p form names)
             (wrap (compile-parser-call form names text p-in state)))

            ((form-p 'and form)
             (wrap (compile-and (rest form) text p-in names r state)))

            ((form-p 'or form)
             (wrap (compile-or (rest form) text p-in names r state)))

            ((form-p 'if form)
             (destructuring-bind (test then else) (cdr form)
               (wrap `(if ,test ,(self then) ,(self else)))))

            ((form-p '-> form)
             (destructuring-bind (expr var) (cdr form)
               (funcall wrapper (self expr) ok var p continuation failure state)))

            ((form-p 'match form)
             (wrap (compile-match (cadr form) text p-in)))

            ((form-p 'trace form)
             (compile-wrapped-form wrapper `(tracer ,(cadr form) ',(cadr form)) text p-in p names (gensym "R") state continuation))

            ((stringp form)
             (wrap (compile-string form text p-in)))

            ((characterp form)
             (wrap (compile-character form text p-in)))

            (t (wrap `(good ,form ,p-in))))))))

(defun compile-parser-argument (exp names state)
  "Used to compile expressions in function calls."
  (with-gensyms (text position)
    (labels ((thunk (exp)
               (if (and (consp exp) (grammar-p (car exp) names) (= (length exp) 3))
                   `(function ,(car exp))
                   `(lambda (,text ,position)
                      (declare (ignorable ,text))
                      ,exp)))
             (self (exp) (compile-parser-argument exp names state)))
      (cond
        ((parser-function-invocation-p exp names)
         (thunk (compile-parser-call exp names text position state)))

        ((form-p 'and exp)
         (thunk (compile-and (rest exp) text position names (gensym "R") state)))

        ((form-p 'or exp)
         (thunk (compile-or (rest exp) text position names (gensym "R") state)))

        ((form-p 'if exp)
         (destructuring-bind (test then else) (cdr exp)
           `(if ,test ,(self then) ,(self else))))

        ((form-p 'trace exp)
         (self `(tracer ,(cadr exp) ',(cadr exp))))

        ((stringp exp)
         (thunk (compile-string exp text position)))

        ((characterp exp)
         (thunk (compile-character exp text position)))

        (t exp)))))

(defun compile-string (str text position)
  `(matching-string ,str ,(length str) ,text ,position))

(defun compile-character (char text position)
  `(matching-char ,char ,text ,position))

(defun compile-match (parser txt p)
  `(funcall ,parser ,txt ,p))

(defun compile-parser-call (exp names txt p state)
  (destructuring-bind (name &rest args) (rewrite-form exp names)
    (flet ((comp (x) (compile-parser-argument x names state)))
      `(,name ,@(mapcar #'comp args) ,txt ,p))))

(defun rewrite-form (form names)
  "Rewrite bare symbols naming parser productions or parser functions into
cannonical list from."
  (cond
    ((and (symbolp form) (grammar-p form names)) (list form))
    (t form)))

(defun parser-function-invocation-p (form names)
  (or
   (and (symbolp form) (grammar-p form names))
   (and (consp form) (symbolp (car form)) (grammar-p (car form) names))))

(defun form-p (what form)
  (and (consp form) (eql (car form) what)))


;;; Runtime functions used by compiled parsers ;;;;;;;;;;;;;;;;;;;;;;;

(defun good (result position)
  "Return values indicating a parser succeeded."
  (values t result position))

(defun matching-string (s len text position)
  (let ((end (+ position len)))
    (when (and (<= end (length text))
               (string= s text :start2 position :end2 end))
      (good s end))))

(defun matching-char (c text position)
  (when (and (< position (length text)) (char= c (char text position)))
    (good c (1+ position))))


;;; Parser primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparserfun any-char (text position)
  "Match a single character. Succeeds everywhere except at the EOF."
  (when (< position (length text))
    (good (char text position) (1+ position))))

(defparserfun eof (text position)
  "Succeed when we are at the end of the text."
  (when (<= (length text) position)
    (good t position)))


;;; Parser combinators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparserfun optional (p text position)
  "Match P if we can, returning what P did if it succeeded or nil if it failed."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (if ok (good r pos) (good nil position))))

(defparserfun try (p text position)
  "Attempt to parse using P, moving forward if it succeeds. If P fails, the try
consumes no input."
  (multiple-value-bind (ok r np) (funcall p text position)
    (when ok (good r np))))

(defparserfun many (p text position)
  "Match P as many times as possible. Always succeeds as zero is an acceptable
number of times to match. Returns a list of the values returned by P."
  (let ((result nil))
    (loop
       (multiple-value-bind (ok r pos) (funcall p text position)
         (cond
           (ok
            (push r result)
            (setf position pos))
           (t
            (return (good (nreverse result) position))))))))

(defparserfun many1 (p text position)
  "Match P as many times as possible but at least once. Returns a list of the
values returned by P."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (when ok
      (multiple-value-bind (ok r2 pos) (many p text pos)
        (declare (ignore ok)) ; many always succeeds.
        (good (cons r r2) pos)))))

(defparserfun not-char (p text position)
  "Succeed only if P does not match at the current position. Consumes and
returns one character when P does not match."
  (unless (funcall p text position)
    (good (char text position) (1+ position))))

(defparserfun look-ahead (p text position)
  "Succeed if P matches. Does not consume any input in either case."
  (when (funcall p text position)
    (good nil position)))

(defparserfun ! (p text position)
  "Succeed if P does not match. Does not consume any input in either case."
  (unless (funcall p text position)
    (good nil position)))

(defparserfun ? (p predicate text position)
  "Succeed if P succeeds and the result satisifes the given predicate."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (when (and ok (funcall predicate r))
      (good r pos))))

(defparserfun counted (n p text position)
  "Match P N times. Return a list of values matched by P."
  (if (zerop n)
      (good nil position)
      (multiple-value-bind (ok r pos) (funcall p text position)
        (when ok
          (multiple-value-bind (ok2 r2 pos2) (counted (1- n) p text pos)
            (when ok2
              (good (cons r r2) pos2)))))))

(defparserfun text (p text position)
  "Capture the text matched by P."
  (multiple-value-bind (ok r pos) (funcall p text position)
    (declare (ignore r))
    (when ok
      (good (subseq text position pos) pos))))


;;;; Tracing helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *trace* 0)
(defvar *trace-level* 0)

(defun trace-on () (incf *trace*))
(defun trace-off () (decf *trace*))

(defparserfun tracing (p text position)
  "Turn on tracing for the dynamic scope of the parser P."
  (trace-on)
  (unwind-protect (funcall p text position)
    (trace-off)))

(defparserfun tracer (p name text position)
  "Trace the execution of the parser P reporting it as NAME."
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
  "Output an arbitrary but properly indented trace message."
  (when (plusp *trace*)
    (let ((indent (make-string (* 2 *trace-level*) :initial-element #\Space)))
      (format t "~&~a" indent)
      (apply #'format t fmt args))))
