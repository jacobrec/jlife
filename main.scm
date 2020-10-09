(define-module (jlife main)
  #:use-module (jlib parse)
  #:use-module (jlib print)
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
(define (parse/time/date)
  1)
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
      (define (between a b c) (and (<= a b) (<= b c)))
      (if (and (between 0 (first x) 24)
               (between 0 (second x) 59))
       x #:parse-error))))

  ; int (:int)? (am|pm)?
  ; 8:30
  ; 20
  ; 8
  ; 8pm
  ; 8:30pm

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

