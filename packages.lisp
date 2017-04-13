;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.yamp
  (:use :common-lisp
        :com.gigamonkeys.json
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.utilities)
  (:export

   ;; Markup
   :markup

   ;; API
   :defparser
   :defparserfun
   :value

   ;; Parser functions
   :!
   :?
   :any-char
   :counted
   :eof
   :look-ahead
   :many
   :many1
   :not-char
   :optional
   :text
   :try

   ;; Tracing
   :tracing
   :tracer
   :tracemsg

   ;; Special syntactic symbols
   :&state
   :_
   :->
   :=>)
  (:import-from :com.gigamonkeys.macro-utilities :gensyms)
  (:shadow :!))
