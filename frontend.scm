(define-module (jlife frontend)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (jlib print)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-1)
  #:export (display-raw
            display-json
            display-list
            display-pretty
            display-pretty-list
            count))

(define (display-raw data)
  (println "'(")
  (map (lambda (x) (print "  ") (writeln x)) data)
  (println ")"))

(define (display-json data)
  (println (event-list->json-string data)))

(define (time->pretty-str time)
  (date->string
    (time-utc->date
     (make-time time-utc 0 time))
    "~H:~M ~a, ~B ~e ~Y"))

(define count (make-parameter 0))
(define* (display-list d #:key show-type? (padding "") show-count?)
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
    (when show-count?
      (with-foreground #:BLU (display (count))))
    (count (+ 1 (count)))
    (with-foreground #:RST
                     (begin
                       (with-effect #:BOLD (display padding))
                       (when show-type?
                         (display typ)
                         (display ": "))
                       (display desc)
                       (when time
                         (display " @ ")
                         (display (time->pretty-str time)))))
    (println))
  (map display-colored-event d))

(define (display-pretty-list d)
  (define (add-seconds-to-date date seconds)
    (time-utc->date (add-duration (date->time-utc date)
                                  (make-time time-duration 0 seconds))))
  (define (is-same-day? a b)
    (and (= (date-year a) (date-year b))
         (= (date-year-day a) (date-year-day b))))
  (define (is-today? x)
    (is-same-day?
     (time-utc->date (make-time time-utc 0 (or (event-time x) 0)))
     (current-date)))
  (define (is-tomorrow? x)
    (is-same-day?
     (time-utc->date (make-time time-utc 0 (or (event-time x) 0)))
     (add-seconds-to-date (current-date) (* 24 60 60))))
  (define (display-section-if title items)
    (when (> (length items) 0)
      (with-effect #:BOLD
        (with-effect #:UNDERLINE (print title)))
      (println)
      (display-list items #:padding pad)))

  (define (time-or-inf x) (or (event-time x) (inf)))
  (define (date-sorter a b) (< (time-or-inf a) (time-or-inf b)))
  (define data (sort d date-sorter))
  (define reminders (filter reminder? data))
  (define todos (filter todo? data))
  (define meetings (filter meeting? data))
  (define due-reminders (filter has-past? reminders))
  (define past-meetings (filter meeting-finished? meetings))
  (define ongoing-meetings (filter meeting-ongoing? meetings))
  (define upcoming-meetings (filter (lambda (x) (not (meeting-started? x))) meetings))
  (define todays-meetings (filter is-today? upcoming-meetings))
  (define tomorrows-meetings (filter is-tomorrow? upcoming-meetings))
  (define pad " - ")
  (define upcoming-meeting-count (min (length meetings) (cdr (assoc 'pretty-upcoming-meeting-count (read-config)))))
  (count 0)

  (in-box-double
    (lambda ()
      (display-section-if "Reminders:" due-reminders)
      (display-section-if "Todos:" todos)
      (display-section-if "Ongoing Meeting:" ongoing-meetings)
      (when (not (null? meetings))
        (cond
          ((= upcoming-meeting-count 1) (display-section-if "Next Meeting:" (list (car meetings))))
          ((< 1 upcoming-meeting-count) (display-section-if "Next Meetings:" (take meetings upcoming-meeting-count)))))
      (display-section-if "Today's Meeting:" todays-meetings)
      (display-section-if "Tomorrow's Meeting:" tomorrows-meetings)
      (when (= 0 (count))
        (with-effect #:UNDERLINE
                     (with-foreground #:CYN
                                      (print "All done. You have nothing - JLife")))
        (println)))))
