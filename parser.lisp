;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)


;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro defparser (name (&rest args) &body body)
  "Define a parser named NAME. The argument list can be used to specify
parameters the parser function will accept in addition to the input argument.
Any arguments after &state will be bound at the beginning of the function and
can be used by productions within the grammar to maintain state which will
backtrack when the parser does."
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
        (with-gensyms (input)
          (flet ((prod-compiler (p) (compile-production p names state)))
            `(defun ,name (,@args ,input)
               (let (,@initial-state)
                 (labels ((show-state (&optional label)
                            (tracemsg "~a: ~@{~(~a~): ~a~^; ~}"
                                      (or label "STATE")
                                      ,@(loop for n in state collect `(quote ,n) collect n)
                                      "pos" (input-position ,input)))
                          ,@(mapcar #'prod-compiler productions))
                   (,(first names) ,input))))))))))

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
  (with-gensyms (input)
    (destructuring-bind ((name &rest args) &rest body) p
      `(,name (,@args ,input)
         ,(compile-and body input names (gensym "R") state)))))

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
  (or (not (null (member name names))) (get name 'parser-function)))

(defun compile-and (body input names result state)
  "Compile the forms in an AND so that the AND matches if each of the elements
matches in sequence."
  (unless (null body)
    (let ((body (rewrite-returns body)))
      (with-gensyms (next-input)
        (compile-wrapped-form
         #'and-wrapper
         (first body)
         input next-input names result state
         (compile-and (rest body) next-input names (gensym "R") state))))))

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

(defun and-wrapper (expr ok result p continuation state)
  "Wrap an expression to be part of a AND. Arranges to save the state, evaluate
the expression. If it succeeds, invoke the continuation of the AND, if there is
one, or succeed, returning the value and next-input returned by the expression.
Otherwise fail, rolling back the state. "
  (if (null continuation)
      expr
      (let ((state-bindings (save-state-bindings state)))
        `(let (,@state-bindings)
           (multiple-value-bind (,ok ,result ,p) ,expr
             (declare (ignorable ,result))
             (if ,ok
                 ,(if continuation
                      `(multiple-value-bind (,ok ,result ,p) ,continuation
                         (if ,ok
                             (good ,result ,p)
                             (progn
                               ,@(restore-state state-bindings)
                               nil)))
                      `(good ,result ,p))
                 (progn
                   ,@(restore-state state-bindings)
                   nil)))))))

(defun compile-or (body input names result state)
  "Compile the forms in an OR so that the OR matches if any of the elements
matches, returning the result from the first match."
  (unless (null body)
    (with-gensyms (next-input)
      (compile-wrapped-form
       #'or-wrapper
       (first body)
       input next-input names result state
       (compile-or (rest body) input names (gensym "R") state)))))

(defun or-wrapper (expr ok result p continuation state)
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
               ,continuation))))))

(defun compile-wrapped-form (wrapper form input p names r state continuation)
  "Used to compile forms that are part of ANDs and ORs."
  (with-gensyms (ok)
    (labels ((wrap (expr) (funcall wrapper expr ok r p continuation state))
             (self (expr) (compile-wrapped-form wrapper expr input p names (gensym "R") state nil)))
      (cond

        ((parser-function-invocation-p form names)
         (wrap (compile-parser-call form names input state)))

        ((form-p 'and form)
         (wrap (compile-and (rest form) input names r state)))

        ((form-p 'or form)
         (wrap (compile-or (rest form) input names r state)))

        ((form-p 'if form)
         (destructuring-bind (test then else) (cdr form)
           (wrap `(if ,test ,(self then) ,(self else)))))

        ((form-p '-> form)
         (destructuring-bind (expr var) (cdr form)
           (funcall wrapper (self expr) ok var p continuation state)))

        ((form-p 'match form)
         (wrap (compile-match (cadr form) input)))

        ((form-p 'trace form)
         (compile-wrapped-form wrapper `(tracer ,(cadr form) ',(cadr form)) input p names (gensym "R") state continuation))

        ((stringp form)
         (wrap (compile-string form input)))

        ((characterp form)
         (wrap (compile-character form input)))

        (t (wrap `(good ,form ,input)))))))

(defun compile-parser-argument (exp names state)
  "Used to compile expressions in function calls."
  (with-gensyms (input)
    (labels ((thunk (exp)
               (if (input-is-only-arg-p exp names)
                   `(function ,(car exp))
                   `(lambda (,input)
                      (declare (ignorable ,input))
                      ,exp)))
             (self (exp) (compile-parser-argument exp names state)))
      (cond
        ((parser-function-invocation-p exp names)
         (thunk (compile-parser-call exp names input state)))

        ((form-p 'and exp)
         (thunk (compile-and (rest exp) input names (gensym "R") state)))

        ((form-p 'or exp)
         (thunk (compile-or (rest exp) input names (gensym "R") state)))

        ((form-p 'if exp)
         (destructuring-bind (test then else) (cdr exp)
           `(if ,test ,(self then) ,(self else))))

        ((form-p 'trace exp)
         (self `(tracer ,(cadr exp) ',(cadr exp))))

        ((stringp exp)
         (thunk (compile-string exp input)))

        ((characterp exp)
         (thunk (compile-character exp input)))

        (t exp)))))

(defun input-is-only-arg-p (exp names)
  "Expression is a call to a grammar function and the only arguments required
are the ones that will be passed by anyone invoking a grammar function, i.e. the
input. N.B. the length test depends, obviously, on the number of standard
arguments which used to be two (text and position) and is now one (combined
input object)."
  (and (consp exp) (grammar-p (car exp) names) (= (length exp) 2)))

(defun compile-string (str input)
  `(matching-string ,str ,(length str) ,input))

(defun compile-character (char input)
  `(matching-char ,char ,input))

(defun compile-match (parser input)
  `(funcall ,parser ,input))

(defun compile-parser-call (exp names input state)
  (destructuring-bind (name &rest args) (rewrite-form exp names)
    (flet ((comp (x) (compile-parser-argument x names state)))
      `(,name ,@(mapcar #'comp args) ,input))))

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

(defun good (result next-input)
  "Return values indicating a parser succeeded."
  (values t result next-input))


;;;;;; I/O protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matching-string (s len input)
  (:documentation "Does the string S (with length LEN) match at INPUT?"))

(defgeneric matching-char (c input)
  (:documentation "Does the char C match at INPUT?"))

(defgeneric end-of-text-p (input)
  (:documentation "Are we at the end of the text?"))

(defgeneric getc (input)
  (:documentation "Get the next character and move forward to the next input."))

(defgeneric gettext (input end)
  (:documentation "Get the text between START and END." ))

(defgeneric input-position (input)
  (:documentation "Current position within the input. This is only used for
  informational purposes in tracing output."))

;;;;;;;;; (cons text position) implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matching-string (s len (input cons))
  (destructuring-bind (text . position) input
    (let ((end (+ position len)))
      (when (and (<= end (length text))
                 (string= s text :start2 position :end2 end))
        (good s (cons text end))))))

(defmethod matching-char (c (input cons))
  (destructuring-bind (text . position) input
    (when (and (< position (length text)) (char= c (char text position)))
      (good c (cons text (1+ position))))))

(defmethod end-of-text-p ((input cons))
  (destructuring-bind (text . position) input
    (>= position (length text))))

(defmethod getc ((input cons))
  (destructuring-bind (text . position) input
    (values (char text position) (cons text (1+ position)))))

(defmethod gettext ((input cons) end)
  (destructuring-bind (text . position) input
    (subseq text position (cdr end))))

(defmethod input-position ((input cons)) (cdr input))

;;; Parser primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparserfun any-char (input)
  "Match a single character. Succeeds everywhere except at the EOF."
  (unless (end-of-text-p input)
    (multiple-value-bind (c p) (getc input)
      (good c p))))

(defparserfun eof (input)
  "Succeed when we are at the end of the text."
  (when (end-of-text-p input)
    (good t input)))


;;; Parser combinators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparserfun optional (p input)
  "Match P if we can, returning what P did if it succeeded or nil if it failed."
  (multiple-value-bind (ok r next-input) (funcall p input)
    (if ok (good r next-input) (good nil input))))

(defparserfun many (p input)
  "Match P as many times as possible. Always succeeds as zero is an acceptable
number of times to match. Returns a list of the values returned by P."
  (let ((result nil))
    (loop
       (multiple-value-bind (ok r next-input) (funcall p input)
         (cond
           (ok
            (push r result)
            (setf input next-input))
           (t
            (return (good (nreverse result) input))))))))

(defparserfun many1 (p input)
  "Match P as many times as possible but at least once. Returns a list of the
values returned by P."
  (multiple-value-bind (ok r input) (funcall p input)
    (when ok
      (multiple-value-bind (ok r2 input) (many p input)
        (declare (ignore ok)) ; many always succeeds.
        (good (cons r r2) input)))))

(defparserfun not-char (p input)
  "Succeed only if P does not match at the current input. Consumes and
returns one character when P does not match."
  (unless (funcall p input)
    (multiple-value-bind (c p) (getc input)
      (good c p))))

(defparserfun peek (p input)
  "Succeed if P matches. Does not consume any input in either case."
  (when (funcall p input)
    (good nil input)))

(defparserfun ! (p input)
  "Succeed if P does not match. Does not consume any input in either case."
  (unless (funcall p input)
    (good nil input)))

(defparserfun ? (p predicate input)
  "Succeed if P succeeds and the result satisifes the given predicate."
  (multiple-value-bind (ok r next-input) (funcall p input)
    (when (and ok (funcall predicate r))
      (good r next-input))))

(defparserfun counted (n p input)
  "Match P N times. Return a list of values matched by P."
  (if (zerop n)
      (good nil input)
      (multiple-value-bind (ok r next-input) (funcall p input)
        (when ok
          (multiple-value-bind (ok2 r2 next-input) (counted (1- n) p next-input)
            (when ok2
              (good (cons r r2) next-input)))))))

(defparserfun text (p input)
  "Capture the text matched by P."
  (multiple-value-bind (ok r next-input) (funcall p input)
    (declare (ignore r))
    (when ok
      (good (gettext input next-input) next-input))))

;;;; Tracing helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *trace* 0)
(defvar *trace-level* 0)

(defun trace-on () (incf *trace*))
(defun trace-off () (decf *trace*))

(defparserfun tracing (p input)
  "Turn on tracing for the dynamic scope of the parser P."
  (trace-on)
  (unwind-protect (funcall p input)
    (trace-off)))

(defparserfun tracer (p name input)
  "Trace the execution of the parser P reporting it as NAME."
  (let ((indent (make-string (* 2 *trace-level*) :initial-element #\Space))
        (*trace-level* (1+ *trace-level*)))
    (when (plusp *trace*)
      (format t "~&~aMatching ~a at ~d ..." indent name (input-position input)))

    (multiple-value-bind (ok r p) (funcall p input)
      (when (plusp *trace*)
        (if ok
            (format t "~&~a~a at ~d: MATCHED: ~s" indent name (input-position input) r)
            (format t "~&~a~a at ~d: No match." indent name (input-position input))))

      (values ok r p))))

(defun tracemsg (fmt &rest args)
  "Output an arbitrary but properly indented trace message."
  (when (plusp *trace*)
    (let ((indent (make-string (* 2 *trace-level*) :initial-element #\Space)))
      (format t "~&~a" indent)
      (apply #'format t fmt args))))
