(define-module (jlife frontend)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (jlib print)
  #:use-module (srfi srfi-19)
  #:export (display-raw
            display-json
            display-list
            display-pretty))

(define (display-raw data)
  (println "'(")
  (map (lambda (x) (print "  ") (writeln x)) data)
  (println ")"))

(define (display-json data)
  (println (event-list->json-string data)))

(define (display-pretty data)
  (display-raw data))

(define (time->pretty-str time)
  (date->string
    (time-utc->date
     (make-time time-utc 0 time))
    "~H:~M ~a, ~B ~e ~Y"))

(define count (make-parameter 0))
(define* (display-list d #:key show-type? (padding ""))
  (define (display-colored-event evt)
    (define typ (car evt))
    (define desc (event-desc evt))
    (define notes (event-notes evt))
    (define time (event-time evt))
    (define duration (event-duration evt))
    (define repeats (event-repeats evt))
    (define color
      (case typ
        ((todo) #:BLU)
        ((meeting) #:MAG)
        ((reminder) #:RED)))
    (with-foreground #:BLU (display (count)))
    (count (+ 1 (count)))
    (with-foreground #:RST
                     (begin
                       (display padding)
                       (when show-type?
                         (display typ)
                         (display ": "))
                       (display desc)
                       (when time
                         (display " @ ")
                         (display (time->pretty-str time)))))
    (println))
  (map display-colored-event d))

(define (display-pretty d)
  (define (time-or-inf x) (or (event-time x) (inf)))
  (define (date-sorter a b) (< (time-or-inf a) (time-or-inf b)))
  (define data (sort d date-sorter))
  (define reminders (filter has-past? (filter reminder? data)))
  (define todos (filter todo? data))
  (define meetings (filter meeting? data))
  (define pad " => ")
  (count 0)
  (println "Reminders:")
  (display-list reminders #:padding pad)
  (println "Todos:")
  (display-list todos #:padding pad)
  (println "Meetings:")
  (display-list meetings #:padding pad))
