;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.yamp
  (:use :common-lisp
        :com.gigamonkeys.json
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.utilities)
  (:export :parse-file
           :parse-text)
  (:import-from :com.gigamonkeys.macro-utilities :gensyms)
  (:shadow :!))
