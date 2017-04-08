;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun run-tests (dir &key (verbose t) (quiet t) (stop t))
  (loop for f in (list-directory dir)
     if (string= (pathname-type f) "txt")
     do (if (and  (not (test-file f verbose quiet)) stop) (return))))


(defun test-file (txt-file verbose quiet)
  (let* ((json-file (make-pathname :type "json" :defaults txt-file))
         (json (listify (parse-json (file-text json-file)))))
    (multiple-value-bind (ok r) (markup (file-text txt-file) 0 '(:note))
      (cond
        ((and ok (equalp json r))
         (unless quiet
           (format t "~&~a => ok" txt-file))
         t)
        (ok
         (format t "~&~a => bad" txt-file)
         (when verbose
           (format t "~& --- Got -----~&~s~&--- Expected -----~&~s~&" r json))
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
