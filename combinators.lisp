;; -*- fill-column: 80; -*-

;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

(defterm counted (n p)
  "Match P N times. Return a list of values matched by P."
  (if (zerop n) nil (and (-> (match p) first) (=> (counted (1- n) p) `(,first ,@_)))))
