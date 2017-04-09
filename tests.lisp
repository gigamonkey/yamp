;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun run-tests (dir &key (verbose t) (quiet t) (stop t))
  (let ((run 0)
        (failures 0))
    (loop for f in (list-directory dir)
       if (string= (pathname-type f) "txt")
       do (let ((passed (test-file f verbose quiet)))
            (incf run)
            (unless passed (incf failures))
            (if (and (not passed) stop) (return))))
    (if (zerop failures)
        (format t "~&All ~r tests passed!" run)
        (format t "~&~d failures of out of ~d tests." failures run))))


(defun test-file (txt-file verbose quiet)
  (let* ((json-file (make-pathname :type "json" :defaults txt-file))
         (json (listify (parse-json (file-text json-file)))))
    (multiple-value-bind (ok r) (markup (file-text txt-file) :subdocs '(:note :comment))
      (cond
        ((and ok (equalp json r))
         (unless quiet
           (format t "~&~a => ok" txt-file))
         t)
        (ok
         (format t "~&~a => bad" txt-file)
         (when verbose
           (format t "~{~&--- ~30,,,'-a~&~s~}~&~34,,,'-a" (list "Got " r "Expected " json) #\-))
         nil)
        (t
         (format t "~&~a => FAILED TO PARSE." txt-file)
         nil)))))


(defun listify (x)
  (typecase x
    (string x)
    (vector
     (let ((list (map 'list #'listify x)))
       (setf (car list) (keywordize (car list)))
       list))
    (t (error "Don't know how to listify: ~s" x))))
