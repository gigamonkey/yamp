(in-package :com.gigamonkeys.yamp)

;; Eval this expression to make a markup file automatically get
;; compiled to HTML on save. Obviously requires Lisp to be started and
;; the code to have been loaded.

(defun generate-html (file)
  (with-output-to-file (out (make-pathname :type "html" :defaults file))
    (monkeylib-text-output:with-text-output (out)
      (monkeylib-html:emit-html
       (markup-html
        (markup (file-text file))))))
  t)
