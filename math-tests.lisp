(in-package :com.gigamonkeys.yamp)

(defun m (s)
  (nth-value 1 (math (cons s 0))))

(defun check (input expected)
  (let ((p (m input)))
    (if (equalp p expected)
        (format t "~&OK: ~a => ~a" input p)
        (format t "~&FAIL: ~a => ~a; expected: ~a"  input p expected))))


(defun math-tests ()
  (check "10" '10)
  (check "10+20" '(+ 10 20))
  (check "10*20" '(* 10 20))
  (check "10+20*30" '(+ 10 (* 20 30)))
  (check "10+20+30" '(+ 10 20 30))
  (check "10+20+30" '(+ 10 20 30))
  (check "10+20*30+40" '(+ 10 (* 20 30) 40))
  (check "10 + 20 * 30 + 40" '(+ 10 (* 20 30) 40))
  (check "(10 + 20) * (30 + 40)" '(* (+ 10 20) (+ 30 40)))
  (check "(10 + 20) * (30 + 40)" '(* (+ 10 20) (+ 30 40)))
  (check "10+20+30+40*(50+50*25*75+50)*60*70+80+90*100" '(+ 10 20 30 (* 40 (+ 50 (* 50 25 75) 50) 60 70) 80 (* 90 100))))
