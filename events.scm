(define-module (jlife events)
  #:use-module (json)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (new-todo
            new-meeting
            new-reminder
            new-note

            todo?
            meeting?
            reminder?
            note?

            event=?
            event-type
            event-time
            event-duration
            event-repeats
            event-desc
            event-notes

            has-past?
            todo-done?
            meeting-started?
            meeting-ongoing?
            meeting-finished?

            event->json-string
            json-string->event
            event-list->json-string
            json-string->event-list))


(define* (new-todo desc #:optional done? due)
  (new-event 'todo desc
             #:when? (new-time due)
             #:notes `((done . ,done?))))

(define (new-meeting name time len)
  (new-event 'meeting name #:when? (new-time time #:duration len)))

(define (new-reminder name time)
  (new-event 'reminder name #:when? (new-time time)))

(define (new-note desc)
  (new-event 'note desc))

(define* (new-time p #:key duration repeats)
  (if p
    (list p duration repeats)
    #f))

(define* (new-event etype desc #:key when? notes)
  (list etype desc when? notes))


(define (event-type b)
  (car b))

(define (event-time=? a b)
  (or
   (and (boolean? a) (boolean? b))
   (and
    (or
     (and (boolean? (first a)) (boolean? (first b)))
     (= (first a) (first b)))
    (or
     (and (boolean? (second a)) (boolean? (second b)))
     (= (second a) (second b)))
    (or
     (and (boolean? (third a)) (boolean? (third b)))
     (= (third a) (third b))))))
(define (event=? a b)
  (and
   (eq? (first a) (first b))
   (string= (second a) (second b))
   (event-time=? (third a) (third b))))

(define (note? x)
  (and (list? x) (eq? (car x) 'note)))
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
  (second x))

(define (event-notes x)
  (fourth x))

(define (has-past? x)
  (define time (event-time x))
  (and time
    (time>=?
      (current-time)
      (make-time time-utc 0 time))))

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
  (define time (or (event-time meet) (inf)))
  (define dur (or (event-duration meet) 0))
  (time>?
    (current-time)
    (make-time time-utc 0 (+ time dur))))

(define (event->json-scm evt)
  (define typ (car evt))
  (define desc (event-desc evt))
  (define notes (event-notes evt))
  (define time (event-time evt))
  (define duration (event-duration evt))
  (define repeats (event-repeats evt))
  `((type . ,typ)
    (desc . ,desc)
    (notes . ,notes)
    (time . ,time)
    (duration . ,duration)
    (repeats . ,repeats)))

(define (json-scm->event alist-obj)
  (define (aget key alist)
    (define v (assoc key alist))
    (if v (cdr v) #f))
  (new-event
   (string->symbol (aget "type" alist-obj))
   (aget "desc" alist-obj)
   #:when? (new-time
              (aget "time" alist-obj)
              #:duration (aget "duration" alist-obj)
              #:repeats (aget "repeats" alist-obj))
   #:notes (aget "notes" alist-obj)))


(define (event->json-string event)
  (scm->json-string (event->json-scm event) #:pretty #t))
(define (json-string->event str)
  (json-scm->event (json-string->scm str)))
(define (event-list->json-string events)
  (scm->json-string
    (list->vector
     (map event->json-scm events))
    #:pretty #t))
(define (json-string->event-list str)
  (map json-scm->event
    (vector->list
     (json-string->scm str))))
