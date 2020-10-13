(define-module (jlife repetitionparser)
  #:use-module (jlib print)
  #:use-module (jlib parse)
  #:use-module (jlife dateparser)
  #:export (parse/repeats))

(define (parse/time/weekday)
  (parse/or
    (parse/alias_lit "mo" "mon" "monday")
    (parse/alias_lit "tu" "tue" "tuesday")
    (parse/alias_lit "we" "wed" "wednesday")
    (parse/alias_lit "th" "thu" "thursday")
    (parse/alias_lit "fr" "fri" "friday")
    (parse/alias_lit "sa" "sat" "saturday")
    (parse/alias_lit "su" "sun" "sunday")))

(define (parse/repeats/subweekly)
  (parse/+ (parse/time/weekday)))

(define (parse/repeats/regular)
  (parse/or
    (parse/lit "daily")
    (parse/lit "weekly")
    (parse/lit "biweekly")
    (parse/lit "monthly")
    (parse/alias_lit "yearly" "annually")))

(define (parse/repeats/every-n)
  (parse/apply
    (parse/and
     (parse/int)
     (parse/or
      (parse/alias_lit "d" "days")
      (parse/alias_lit "w" "weeks")
      (parse/alias_lit "f" "fortnights")
      (parse/alias_lit "m" "months")
      (parse/alias_lit "y" "years")))
    (lambda (x) (cons (car x) (cadr x)))))

(define (parse/repeats)
  (parse/or
    (parse/repeats/regular)
    (parse/repeats/subweekly)
    (parse/repeats/every-n)))
#|
(cond
  ((string? x) "regular")
  ((list? x) "subweekly")
  ((pair? x) "every-n"))
|#
