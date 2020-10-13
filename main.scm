(define-module (jlife main)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (jlib numbers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19))

(define string-month->month_data
  '(("jan" . 1)
    ("feb" . 2)
    ("mar" . 3)
    ("apr" . 4)
    ("may" . 5)
    ("jun" . 6)
    ("jul" . 7)
    ("aug" . 8)
    ("sep" . 9)
    ("oct" . 10)
    ("nov" . 11)
    ("dec" . 12)))
(define (string-month->month s)
  (define m (assoc s string-month->month_data))
  (if m (cdr m) #f))
  

(define (parse/duration-spec)
  (parse/or_lit
    "s" "sec" "seconds"
    "m" "min" "minutes"
    "h" "hours"
    "d" "days"
    "w" "weeks"))

(define (parse/duration)
  (parse/apply
    (parse/and
      (parse/int)
      (parse/duration-spec))
    (lambda (parsed)
      (* (first parsed)
        (cond
          ((member (second parsed) '("s" "sec" "seconds")) 1)
          ((member (second parsed) '("m" "min" "minutes")) 60)
          ((member (second parsed) '("h" "hours"))         (* 60 60))
          ((member (second parsed) '("d" "days"))          (* 24 60 60))
          ((member (second parsed) '("w" "weeks"))         (* 7 24 60 60)))))))

(define (parse/time/weekday)
  (parse/or
    (parse/alias_lit "mo" "mon" "monday" "Mo" "Mon" "Monday" "MO" "MON" "MONDAY")
    (parse/alias_lit "tu" "tue" "tuesday" "Tu" "Tue" "Tuesday" "TU" "TUE" "TUESDAY")
    (parse/alias_lit "we" "wed" "wednesday" "We" "Wed" "Wednesday" "WE" "WED" "WEDNESDAY")
    (parse/alias_lit "th" "thu" "thursday" "Th" "Thu" "Thursday" "TH" "THU" "THURSDAY")
    (parse/alias_lit "fr" "fri" "friday" "Fr" "Fri" "Friday" "FR" "FRI" "FRIDAY")
    (parse/alias_lit "sa" "sat" "saturday" "Sa" "Sat" "Saturday" "SA" "SAT" "SATURDAY")
    (parse/alias_lit "su" "sun" "sunday" "Su" "Sun" "Sunday" "SU" "SUN" "SUNDAY")))

(define (parse/time/relativeday)
  (parse/or
    (parse/alias_lit "today" "Today" "TODAY")
    (parse/alias_lit "tomorrow" "Tomorrow" "TOMORROW" "tommorow" "tommorrow")))

(define (parse/time/month)
  (parse/or
    (parse/alias_lit "jan" "Jan" "JAN" "january" "January" "JANUARY")
    (parse/alias_lit "feb" "Feb" "FEB" "february" "February" "FEBRUARY")
    (parse/alias_lit "mar" "Mar" "MAR" "march" "March" "MARCH")
    (parse/alias_lit "apr" "Apr" "APR" "april" "April" "APRIL")
    (parse/alias_lit "may" "May" "MAY" "may" "May" "MAY")
    (parse/alias_lit "jun" "Jun" "JUN" "june" "June" "JUNE")
    (parse/alias_lit "jul" "Jul" "JUL" "july" "July" "JULY")
    (parse/alias_lit "aug" "Aug" "AUG" "august" "August" "AUGUST")
    (parse/alias_lit "sep" "Sep" "SEP" "september" "September" "SEPTEMBER")
    (parse/alias_lit "oct" "Oct" "OCT" "october" "October" "OCTOBER")
    (parse/alias_lit "nov" "Nov" "NOV" "november" "November" "NOVEMBER")
    (parse/alias_lit "dec" "Dec" "DEC" "december" "December" "DECEMBER")))


(define (parse/time/date)
  (parse/apply
    (parse/and
      (parse/time/month)
      (parse/int))
    (lambda (x)
      (if (or
           (and (string= (first x) "jan") (between? 1 (second x) 31))
           (and (string= (first x) "feb") (between? 1 (second x) 29))
           (and (string= (first x) "mar") (between? 1 (second x) 31))
           (and (string= (first x) "apr") (between? 1 (second x) 30))
           (and (string= (first x) "may") (between? 1 (second x) 31))
           (and (string= (first x) "jun") (between? 1 (second x) 30))
           (and (string= (first x) "jul") (between? 1 (second x) 31))
           (and (string= (first x) "aug") (between? 1 (second x) 31))
           (and (string= (first x) "sep") (between? 1 (second x) 30))
           (and (string= (first x) "oct") (between? 1 (second x) 31))
           (and (string= (first x) "nov") (between? 1 (second x) 30))
           (and (string= (first x) "dec") (between? 1 (second x) 31)))
          x #:parse-error))))

(define (parse/time/hour)
  (parse/apply
    (parse/apply
      (parse/and
        (parse/int)
        (parse/apply
          (parse/?
            (parse/and
              (parse/lit ":")
              (parse/int)))
          (lambda (x) (if (null? x) 0 (second x))))
        (parse/?
          (parse/or_lit "am" "pm")))
      (lambda (x)
        (if (null? (third x))
            (list (first x) (second x))
            (list
             (cond
              ((and (string= "am" (third x)) (= 12 (first x))) 0)
              ((and (string= "pm" (third x)) (= 12 (first x))) 12)
              ((and (string= "pm" (third x))) (+ 12 (first x)))
              ((and (string= "am" (third x))) (first x)))
             (second x)))))
    (lambda (x)
      (if (and (between? 0 (first x) 24)
               (between? 0 (second x) 59))
       x #:parse-error))))

(define (parse/time/day)
  (parse/or
    (parse/time/weekday)
    (parse/time/relativeday)))

(define (parse/time/at l1 l2)
  (parse/apply
   (parse/and l1 (parse/lit "@") l2)
   (lambda (x) (list (first x) (third x)))))
(define (parse/time/day-hour)
  (parse/time/at (parse/time/day) (parse/time/hour)))
(define (parse/time/date-hour)
  (parse/time/at (parse/time/date) (parse/time/hour)))


(define (add-seconds-to-date date seconds)
  (time-utc->date (add-duration (date->time-utc date)
                                (make-time time-duration 0 seconds))))

(define (assemble-datetime date hour)
  (define uhour (first hour))
  (define umin (second hour))
  (set-fields date
    ((date-hour) uhour)
    ((date-minute) umin)
    ((date-second) 0)
    ((date-nanosecond) 0)))


(define (infer-date-from-hour h)
  (define uhour (first h))
  (define umin (second h))

  (define today (current-date))
  (define tomorrow (add-seconds-to-date today (* 24 60 60)))
  (define hour (date-hour today))
  (define min (date-minute today))
  (if (or (< uhour hour)
          (and (= uhour hour)  (< umin min)))
      today
      tomorrow))

(define (infer-date-from-date date)
  (define umonth-string (first date))
  (define umonth (if (number? (first date)) (first date)
                     (string-month->month umonth-string)))
  (define uday (second date))

  (define today (current-date))
  (define year (date-year today))

  (define udate (current-date))
  (define udate-year (current-date))

  (set! udate
    (set-fields udate
      ((date-day) uday)
      ((date-month) umonth)))
  (set! udate-year
    (set-fields udate-year
      ((date-day) uday)
      ((date-month) umonth)
      ((date-year) (+ 1 year))))

  (if (time>=?
        (date->time-utc udate)
        (date->time-utc today))
    udate
    udate-year))

(define (infer-date d)
  (cond
   ((member d '("mo" "tu" "we" "th" "fr" "sa" "su")) (infer-date-from-weekday d))
   ((member d '("today" "tomorrow")) (infer-date-from-relative d))
   (else (error "unknown date type"))))

(define (infer-date-from-weekday day)
  (define today (current-date))
  (define weekday (date-week-day today))
  (define shifts
    (cond
     ((string= day "su") 0)
     ((string= day "mo") 1)
     ((string= day "tu") 2)
     ((string= day "we") 3)
     ((string= day "th") 4)
     ((string= day "fr") 5)
     ((string= day "sa") 6)
     (else (error "unknown week day type"))))
  (define diff (- shifts weekday))
  (define daystoadd (if (>= 0 diff) (+ 7 diff) diff))
  (add-seconds-to-date today (* daystoadd 24 60 60)))
  
(define (infer-date-from-relative day)
  (cond
   ((string= day "today") (current-date))
   ((string= day "tomorrow") (add-seconds-to-date (current-date) (* 24 60 60)))
   (else (error "unknown relative day type"))))
(define (parse/time)
  (parse/apply
    (parse/or
      (parse/apply (parse/time/day-hour) (lambda (x) (cons 'day-hour x)))
      (parse/apply (parse/time/date-hour) (lambda (x) (cons 'date-hour x)))
      (parse/apply (parse/time/day) (lambda (x) (cons 'day x)))
      (parse/apply (parse/time/hour) (lambda (x) (cons 'hour x)))
      (parse/apply (parse/time/date) (lambda (x) (cons 'date x))))
    (lambda (x)
      (define type (car x))
      (define val (cdr x))
      (define default-hour '(7 0))
      (case type
        ((day-hour) (assemble-datetime (infer-date (first val)) (second val)))
        ((date-hour) (assemble-datetime (infer-date-from-date (first val)) (second val)))
        ((date) (assemble-datetime (infer-date-from-date val) default-hour))
        ((day) (assemble-datetime (infer-date val) default-hour))
        ((hour) (assemble-datetime (infer-date-from-hour val) val))))))
      


(println ((parse/time) "tomorrow@8am"))
(println ((parse/time) "dec8@8am"))
(println ((parse/time) "10:45pm"))
(println ((parse/time) "11"))
(println ((parse/time) "tu"))
