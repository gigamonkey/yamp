;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defun htmlize (doc)
  "Rewrite the doc so :link elements are expanded into :a tags with the appropriate link."
  (let ((h1 (first (extract :h1 doc))))
    `(:progn
      (:noescape "<!DOCTYPE html>")
      (:html
        (:head
         (:meta :charset "UTF-8")
         (:title ,@(rest h1))
        ,doc)))))
