;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.yamp
  :name "com.gigamonkeys.yamp"
  :description "Yet Another Markup Parser."
  :components
  ((:file "packages")
   (:file "parser" :depends-on ("packages"))
   (:file "markup" :depends-on ("packages" "parser"))
   (:file "math" :depends-on ("packages" "parser"))
   (:file "tests" :depends-on ("packages" "markup")))
  :depends-on
  (:com.gigamonkeys.json
   :com.gigamonkeys.macro-utilities
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.utilities))
