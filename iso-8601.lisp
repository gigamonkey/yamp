(in-package :com.gigamonkeys.yamp)

(defun parse-iso-8601 (text)
  (multiple-value-bind (ok result) (iso-8601 (cons text 0))
    (when ok result)))

(defparser iso-8601 ()

  (iso-date-and-time
   (-> iso-date date)
   (-> (optional (and (optional "T") (=> iso-time))) time)
   eof
   `(,@date ,@time))

  (iso-date
   (or calendar-date week-date ordinal-date))

  (iso-time
   (-> hh hour)
   (-> (optional (or
                  (and ":"
                       (-> minute minute)
                       (=> (optional (and ":" (-> ss second) (=> (optional sss) `(:second ,second ,@(if _ `(:decimal ,_))))))
                           `(:minute ,minute ,@_)))
                  (and (->  minute minute)
                       (=> (optional (and (-> ss second) (=> (optional sss) `(:second ,second ,@(if _ `(:decimal ,_))))))
                           `(:minute ,minute ,@_)))))
       minute-and-second)
   (-> (optional iso-time-zone) tz)
   `(:hour ,hour ,@minute-and-second ,@tz))

  (iso-time-zone
   (=> (or "Z" offset) `(:tz ,_)))

  (offset
   (and (-> plus-minus sign)
        (-> hh hour)
        (-> (maybe ":" minute) minute)
        `(,sign ,hour ,@(if minute (list minute)))))

  ((maybe delim p) (optional (and (optional delim) (match p))))

  (plus-minus
   (or (and "+" '+) (and "-" '-)))

  (calendar-date
   (-> yyyy year)
   (or
    (and (optional "-") (-> mm month) (optional "-") (-> dd day)
         `(:year ,year :month ,month :day ,day))
    (and "-" (-> mm month) ;; yyyymm is not allowed by iso-8601 due to ambiguity ith yy-mm-dd
         `(:year ,year :month ,month))))

  (week-date
   (-> yyyy year)
   (optional "-")
   "W"
   (-> ww week)
   (-> (optional (and (optional "-") d)) day)
   `(:week-date ,year ,week ,@(and day (list day))))

  (ordinal-date
   (-> yyyy year)
   (optional "-")
   (-> ddd day)
   `(:ordinal-date ,year ,day))

  (yyyy (number 4))

  (mm (number 2))

  (dd (number 2))

  (ww (number-between 2 1 53))

  (d (number-between 1 1 7))

  (ddd (number-between 3 1 366))

  (hh (number-between 2 0 24))

  (minute (number-between 2 0 59))

  (ss (number-between 2 0 66))

  (sss (or "." ",") (=> (many1 digit) (parse-integer (concatenate 'string _))))

  (digit (? any-char #'digit-char-p))

  ((number ds)
   (=> (counted ds digit) (parse-integer (concatenate 'string _))))

  ((number-between ds min max)
   (? (number ds) (lambda (x) (<= min x max)))))
