(define-module (jlife durationparser)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (srfi srfi-1)
  #:export (parse/duration))


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
