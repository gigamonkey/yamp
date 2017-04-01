;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;



(in-package :com.gigamonkeys.yamp)

;;; Evaluation rule

;;; SYMBOL: variable whose value may be a parser.

;;; (SYMBOL ...) and not a special form: Value of SYMBOL must be a
;;; parser which is then invoked with the current text state and
;;; position. If the parser succeeds the next form in the body is
;;; evaluated. If not, the enclosing parser fails, leaving the
;;; position and state unchanged.

;;; (STATE :name) (except in SETF, et al. forms) access the named
;;; value in the current (i.e. passed in) state.

;;; (SETF (STATE :name) ...) modify the named value in the new state.
;;; (INCF (STATE :name) ...) modify the named value in the new state.
;;; (DECF (STATE :name) ...) modify the named value in the new state.


(defmacro defparser (name (&rest args) &body body)
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

(defun actual-parser-fuction (a1 a2 state text position)
  ;; returns:
  ;;  - parse succeeded: t or nil
  ;;  - result of parse for capture (nil if parse failed)
  ;;  - next state (which is old state if parse failed)
  ;;  - new position (old position if parse failed)
  )


(defun try (p text state position)
  "Attempt to parse using p, moving forward if it succeeds. TRY
however always succeeds."
  (multiple-value-bind (ok r ns np) (call-parser p text state position)
    (if ok
        (values t r ns np)
        (values t nil state position))))

(defun many (p text state position)
  "Match P as many times as possible. Always succeeds as zero is an
acceptable number of times to match."
  (loop with r = nil
     while t do
       (multiple-value-bind (ok result new-state new-position)
           (call-parser p text state position)
         (cond
           (ok
            (setf r result)
            (setf state new-state)
            (setf position new-position))

           (t
            (return (values t r state position)))))))
