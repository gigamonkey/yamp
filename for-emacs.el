(defun markup-autohtml ()
  (interactive)
  (add-hook 'after-save-hook #'markup-generate-html nil t))

(defun markup-generate-html ()
  (cond
   ((slime-connected-p)
    (slime-eval `(com.gigamonkeys.yamp::generate-html ,(buffer-file-name)))
    (message "Generated html."))
   (t
    (message "Not connected to Lisp. Can't generate HTML."))))
