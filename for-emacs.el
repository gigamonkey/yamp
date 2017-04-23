(defun markup-autohtml ()
  (interactive)
  (add-hook 'after-save-hook #'markup-generate-html nil t))

(defun markup-generate-html ()
  (cond
   ((slime-connected-p)
    (cond
     ((markup-yamp-exists)
      (let ((resp (slime-eval `(com.gigamonkeys.yamp::generate-html ,(buffer-file-name)))))
        (if resp
          (message "Generated html.")
        (message "Problem generating html."))))
     (t
      (message "Lisp connected but YAMP not loaded."))))
   (t
    (message "Not connected to Lisp. Can't generate html."))))

(defun markup-yamp-exists ()
  (slime-eval '(cl:not (cl:not (cl:find-package "COM.GIGAMONKEYS.YAMP")))))
