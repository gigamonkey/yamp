;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.yamp
  :name "com.gigamonkeys.yamp"
  :description "Yet Another Markup Parser."
  :components
  ((:file "packages")
   (:file "for-emacs" :depends-on ("packages" "html"))
   (:file "html" :depends-on ("packages" "trees"))
   (:file "markup" :depends-on ("packages" "parser"))
   (:file "math" :depends-on ("packages" "parser"))
   (:file "parser" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages" "markup"))
   (:file "trees" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.json
   :com.gigamonkeys.macro-utilities
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.utilities
   :monkeylib-html))
