(define-module (jlife events)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (new-todo
            new-meeting
            new-reminder

            todo?
            meeting?
            reminder?

            event-time
            event-duration
            event-repeats
            event-desc
            event-notes

            has-past?
            todo-done?
            meeting-started?
            meeting-ongoing?
            meeting-finished?))


(define* (new-todo desc done? #:optional due)
  (new-event 'todo desc
             #:when? (new-time due)
             #:notes `((done . ,done?))))

(define (new-meeting name time length)
  (new-event 'meeting name #:when? (new-time time #:duration length)))

(define (new-reminder name time)
  (new-event 'reminder name #:when? (new-time time)))

(define* (new-time p #:key duration repeats)
  (if p
    (list p duration repeats)
    #f))

(define* (new-event etype desc #:key when? notes)
  (list etype desc when? notes))

(define (todo? x)
  (and (list? x) (eq? (car x) 'todo)))
(define (reminder? x)
  (and (list? x) (eq? (car x) 'reminder)))
(define (meeting? x)
  (and (list? x) (eq? (car x) 'meeting)))

(define (event-time x)
  (and (third x)
       (first (third x))))

(define (event-duration x)
  (and (third x)
       (second (third x))))

(define (event-repeats x)
  (and (third x)
       (third (third x))))

(define (event-desc x)
  (first x))

(define (event-notes x)
  (fourth x))

(define (has-past? x)
  (define time (event-time x))
  (time>?
    (current-time)
    (make-time time-utc 0 time)))

(define (todo-done? x)
  (and (todo? x)
       (assoc 'done (event-notes x))
       (cdr (assoc 'done (event-notes x)))))

(define (meeting-started? meet)
  (has-past? meet))
(define (meeting-ongoing? meet)
  (and (meeting-started? meet)
       (not (meeting-finished? meet))))
(define (meeting-finished? meet)
  (define time (event-time meet))
  (define dur (event-duration meet))
  (time>?
    (current-time)
    (make-time time-utc 0 (+ time dur))))
