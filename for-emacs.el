(defun markup-autogenerate-html ()
  (interactive)
  (add-hook 'after-save-hook #'markup-generate-html nil t))

(defun markup-generate-html ()
  (interactive)
  (cond
   ((not (slime-connected-p))
    (message "SLIME not connected to Lisp. Can't generate html."))
   ((not (markup-yamp-exists))
    (message "SLIME connected but YAMP not loaded. Can't generate html."))
   ((not (markup-send-slime-request))
    (message "Problem generating html."))
   (t (message "Generated html."))))

(defun markup-yamp-exists ()
  (slime-eval '(cl:not (cl:not (cl:find-package "COM.GIGAMONKEYS.YAMP")))))

(defun markup-send-slime-request ()
  (slime-eval `(com.gigamonkeys.yamp::generate-html ,(buffer-file-name))))
