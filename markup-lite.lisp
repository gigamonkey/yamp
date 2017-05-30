;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defparser markup-lite (&state subdocs (indent 0) (so-far 0) (subdoc-level 0))

  (simple-contents (many1 (or (text-until (or tag-open "}")) tagged-text)))

  ((text-until p)
   (=> (many1 (and (! p) (or escaped-char any-char))) (to-string _)))

  (escaped-char (and "\\" (or #\\ #\{ #\} #\* #\# #\- #\[ #\] #\% #\| #\<)))

  (tagged-text (-> tag-open tag) (=> simple-contents `(,tag ,@_)) "}")

  (tag-open (unescaped "\\") (=> name) "{")

  ((unescaped p) (! escaped-char) (match p))

  (name (=> (text (many1 (? any-char #'alpha-char-p))) (keywordize _))))
