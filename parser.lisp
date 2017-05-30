;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)


;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro defterm (name (&rest args) &body body)
  "Define a stand-alone parser function using the same language as DEFPARSER."
  (multiple-value-bind (docstring body) (extract-docstring body)
    (with-gensyms (input)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (get ',name 'parser-function) t))
         (defun ,name (,@args ,input)
           ,@(when docstring (list docstring))
           ,(compile-and body input (list name) nil))))))

;;; Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-parser (name arglist body)
  (multiple-value-bind (docstring body) (extract-docstring body)
    (let* ((productions (mapcar #'normalize-production body))
           (names (mapcar #'caar productions)))
      (multiple-value-bind (args initial-state) (extract-state-variables arglist)
        (let ((state (append args (mapcar #'car initial-state))))
          (flet ((comp (p) (compile-production p (cons name names) state)))
            (with-gensyms (input)
              `(defun ,name (,@args ,input)
                 ,@(when docstring (list docstring))
                 (let (,@initial-state)
                   (labels ((show-state (&optional label)
                              (tracemsg "~a: ~@{~(~a~): ~a~^; ~}"
                                        (or label "STATE")
                                        ,@(loop for n in state collect `(quote ,n) collect n)
                                        "pos" (input-position ,input)))
                            ,@(mapcar #'comp productions))
                     (,(first names) ,@args ,input)))))))))))

(defun extract-docstring (body)
  "Extract an optional docstring from the body of a macro form."
  (if (and (consp body) (stringp (first body)))
      (values (first body) (rest body))
      (values nil body)))

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

(defun compile-production (p names state)
  "Compile a production into a function definition appropriate for a LABELS
binding."
  (with-gensyms (input)
    (destructuring-bind ((name &rest args) &rest body) p
      `(,name (,@args ,input) ,(compile-and body input names state)))))

(defun compile-form (form input names state)
  "Compile a single parser form as it would appear in an AND or OR."
  (let ((state-bindings (save-state-bindings state)))
    (flet ((self (x) (compile-form x input names state)))
      (with-gensyms (ok result next-input)
        `(let (,@state-bindings)
           (multiple-value-bind (,ok ,result ,next-input)
               ,(cond
                  ((form-p 'and form)          (compile-and (rest form) input names state))
                  ((form-p 'or form)           (compile-or (rest form) input names state))
                  ((form-p 'if form)           (compile-if (cdr form) #'self))
                  ((form-p 'match form)        (compile-match (cadr form) input))
                  ((form-p 'trace form)        (compile-trace form #'self))
                  ((parser-call-p form names)  (compile-parser-call (mklist form) input names state))
                  ((stringp form)              (compile-string form input))
                  ((characterp form)           (compile-character form input))

                  ;; N.B. we copy-input here to ensure that lisp forms do not
                  ;; have their state rewound.
                  (t `(values t ,form (copy-input ,input))))
             (when (or (eql ,next-input ,input) (not ,ok))
               ,@(restore-state state-bindings))
             (when ,ok
               (values ,ok ,result ,next-input))))))))

(defun compile-and (body input names state)
  "Compile the forms in an AND so that the AND matches if each of the elements
matches in sequence."
  (with-gensyms (ok result)
    (labels ((inner (exprs)
               (if exprs
                   (multiple-value-bind (form var) (form-and-variable (first exprs))
                     `(multiple-value-bind (,ok ,(or var result) ,input)
                          ,(compile-form form input names state)
                        ,@(when (not var) `((declare (ignorable ,result))))
                        (when ,ok ,(inner (rest exprs)))))
                   `(values ,ok ,result ,input))))
      (inner (rewrite-and-body body)))))

(defun form-and-variable (exp)
  (if (form-p '-> exp) (values-list (rest exp)) (values exp nil)))

(defun rewrite-and-body (body)
  "Rewrite the list of forms to be compiled into a AND so that a (=> ...) form
will have its result returned as the result of the AND. If the => form specifies
a value expression it can refer to the value matched by the parser via the
variable _. Otherwise the value of the parser wrapped in the => is returned by
the AND. If there is no => form in the AND, then the value of the AND is the
value of the last expression."
  (let (variables result)
    (flet ((rewrite (x)
             (cond
               ((form-p '-> x)
                (push (third x) variables)
                x)
               ((form-p '=> x)
                (if result (error "Two => forms in body: ~s" body))
                (setq result (or (third x) '_))
                (push '_ variables)
                `(-> ,(second x) _))
               (t x))))
      (let ((new-body (mapcar #'rewrite body)))
        (values (if result `(,@new-body ,result) body) variables)))))

(defun compile-or (body input names state)
  (with-gensyms (ok result next-input)
    (if body
        `(multiple-value-bind (,ok ,result ,next-input)
             ,(compile-form (car body) input names state)
           (if ,ok
               (values ,ok ,result ,next-input)
               ,(compile-or (rest body) input names state)))
        nil)))

(defun compile-if (body comp)
  (destructuring-bind (test then else) body
    `(if ,test ,(funcall comp then) ,(funcall comp else))))

(defun compile-match (parser input)
  `(funcall ,parser ,input))

(defun compile-trace (form comp)
  (funcall comp `(tracer ,(cadr form) ',(cadr form))))

(defun compile-parser-call (exp input names state)
  "Compile a call to a parser function. Arguments are evaluated slightly
differently than forms appearing as grammar expressions: grammar expressions are
turned into a thunk so the parser function can invoke them as needed while IF
and TRACE forms have their parts compiled with this function. All other values
are passed through as plain Lisp values."
  (destructuring-bind (name &rest args) exp
    (labels ((compile-arg (form)
               (cond
                 ((or
                   (parser-call-p form names)
                   (form-p 'and form)
                   (form-p 'or form)
                   (stringp form)
                   (characterp form))
                  (with-gensyms (input)
                    (thunkify (compile-form form input names state) input names)))

                 ((form-p 'if form) (compile-if (cdr form) #'compile-arg))
                 ((form-p 'trace form) (compile-trace form #'compile-arg))
                 (t form))))

      `(,name ,@(mapcar #'compile-arg args) ,input))))

(defun thunkify (x input names)
  "Slight optimization for the case where we just have a parser function being
called with the input as its only argument."
  (if (and (consp x) (grammar-p (car x) names) (= (length x) 2))
      `(function ,(car x))
      `(lambda (,input) ,x)))

(defun compile-string (str input) `(matching-string ,str ,(length str) ,input))

(defun compile-character (char input) `(matching-char ,char ,input))

(defun parser-call-p (form names)
  (or
   (and (symbolp form) (grammar-p form names))
   (and (consp form) (symbolp (car form)) (grammar-p (car form) names))))

(defun form-p (what form)
  (and (consp form) (eql (car form) what)))

;;; I/O protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defgeneric copy-input (input)
  (:documentation "Make a copy of input that is not EQL to the current input."))

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

(defmethod copy-input (input) (copy-list input))

(defmethod input-position ((input cons)) (cdr input))


;;; Parser primitives and combinators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun good (result next-input)
  "Return values indicating a parser succeeded."
  (values t result next-input))

(defmacro defparserfun (name (&rest args) &body body)
  "Macro for defining hand-written functions that act as parser elements. Used
to define things that can't be expressed as normal parsers but which don't need
to be built into the compiler proper."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'parser-function) t))
     (defun ,name (,@args) ,@body)))

(defparserfun any-char (input)
  "Match a single character. Succeeds everywhere except at the EOF."
  (unless (end-of-text-p input)
    (multiple-value-bind (c p) (getc input)
      (good c p))))

(defparserfun eof (input)
  "Succeed when we are at the end of the text."
  (when (end-of-text-p input)
    (good t input)))

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

(defparserfun text (p input)
  "Capture the text matched by P."
  (multiple-value-bind (ok r next-input) (funcall p input)
    (declare (ignore r))
    (when ok
      (good (gettext input next-input) next-input))))

;;; Tracing helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sometimes it's useful to trace the behavior of a grammar. These functions
;;; allow you to annotate your grammer with (trace ...) and (tracing ...) forms
;;; to trace specific parts of the grammar and to turn tracing on in particular
;;; dynamic scopes.

(defvar *trace* 0)
(defvar *trace-level* 0)

(defun trace-on () (setf *trace* 1))
(defun trace-off () (setf *trace* 0))

(defparserfun tracing (p input)
  "Turn on tracing for the dynamic scope of the parser P."
  (incf *trace*)
  (unwind-protect (funcall p input)
    (decf *trace*)))

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
