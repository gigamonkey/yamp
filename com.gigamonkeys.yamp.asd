;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.yamp
  :name "com.gigamonkeys.yamp"
  :description "Yet Another Markup Parser."
  :components
  ((:file "packages")
   (:file "parser"      :depends-on ("packages"))
   (:file "combinators" :depends-on ("packages" "parser"))
   (:file "trees"       :depends-on ("packages"))
   (:file "html"        :depends-on ("packages" "trees"))
   (:file "for-emacs"   :depends-on ("packages" "html"))
   (:file "markup"      :depends-on ("packages" "parser" "combinators"))
   (:file "math"        :depends-on ("packages" "parser"))
   (:file "math-tests"  :depends-on ("packages" "math"))
   (:file "tests"       :depends-on ("packages" "markup")))
  :depends-on
  (:com.gigamonkeys.json
   :com.gigamonkeys.macro-utilities
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.utilities
   :monkeylib-html))
