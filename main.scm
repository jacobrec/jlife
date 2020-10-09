(define-module (jlife main)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (jlib numbers)
  #:use-module (srfi srfi-1))

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

;; TODO
(define (parse/time/weekday)
  1)
(define (parse/time/relativeday)
  1)
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
  (parse/and l1 (parse/lit "@") l2))
(define (parse/time/day-hour)
  (parse/time/at (parse/time/day) (parse/time/hour)))
(define (parse/time/date-hour)
  (parse/time/at (parse/time/date) (parse/time/hour)))



(define (parse/time)
  (parse/or
    (parse/time/day)
    (parse/time/hour)
    (parse/time/day-hour)
    (parse/time/date)
    (parse/time/date-hour)))

