;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.yamp
  :name "com.gigamonkeys.yamp"
  :description "Yet Another Markup Parser."
  :components
  ((:file "packages")
   (:file "markup" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.utilities
   :com.gigamonkeys.pathnames))
