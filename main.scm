(define-module (jlife main)
  #:use-module (jlib argparser)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (jlife config)
  #:use-module (jlife backend)
  #:use-module (jlife frontend)
  #:use-module (jlife dateparser)
  #:use-module (jlife durationparser)
  #:use-module (jlife events)
  #:use-module (jlife repetitionparser)
  #:use-module (srfi srfi-1))

;; cli utils
(define (option-default key default alist)
  (define v (assoc key alist))
  (if (and v (cdr v)) (cdr v) default))

(define (operator-default default ops)
  (if (null? ops) default
      (car ops)))

(define (operator-default-is default check ops)
  (string= check (operator-default default ops)))

(define (make-level default mappings)
  (lambda (ops args)
    (define (checker mappings)
      (if (null? mappings)
        (display-help (cdr ops) args)
        (let ((key (caar  mappings))
              (fn  (cdar mappings)))
          (if (operator-default-is default key ops)
              (fn (if (null? ops) ops (cdr ops)) args)
              (checker (cdr mappings))))))
    (checker mappings)))

(define (new-event-and-save event)
  (save-data (cons event (load-data))))

(define (find-event-by-index idx)
  (define events (load-data))
  (cond
   ((>= idx (length events))
    (cons #f '()))
   ((< idx 0)
    (cons #f '()))
   (else
    (let ((i 0))
      (define-values (matches not-matches)
        (partition (lambda (x) (set! i (+ 1 i)) (= (- i 1) idx)) events))
      (cons (car matches) not-matches)))))

(define (find-event-by-substring str)
  (define events (load-data))
  (define-values (matches not-matches)
    (partition (lambda (x)
                 (string-contains (event-desc x) str))
               events))
  (if (= 1 (length matches))
    (cons (car matches) not-matches)
    (cons #f matches)))

(define (find-event ops args)
  ;; returns a pair, car is either the found task or false if none, or
  ;; multiple tasks were found. cdr if car is false, is all found
  ;; tasks, or if car is true, all not found tasks
  (define s-str (string-join ops " "))
  (define res
    (if (string->number s-str)
      (find-event-by-index (string->number s-str))
      (find-event-by-substring s-str)))
  (unless (car res)
    (println "Failed to find a single event")
    (unless (null? (cdr res))
      (println "  Multiple events found")))
  res)

;; cli tools
(define (display-all ops args)
  (define default "pretty")
  (define data (load-data))
  (cond
   ((string= "raw" (option-default "display" default args)) (display-raw data))
   ((string= "json" (option-default "display" default args)) (display-json data))
   ((string= "list" (option-default "display" default args)) (display-list data))
   ((string= "pretty" (option-default "display" default args)) (display-pretty data))
   (else (display-raw data))))

(define (display-help ops args)
  (println "Invalid usage."))

(define (task-cli ops args)
  (define due-str (option-default "due" #f args))
  (define due (if due-str ((parse/time) due-str) due-str))
  (define desc (string-join ops " "))
  (define event (new-todo desc #f due))
  (new-event-and-save event))


(define (meeting-cli ops args)
  (define due-str (option-default "due" #f args))
  (define due (if due-str ((parse/time) due-str) due-str))
  (define dur-str (option-default "duration" #f args))
  (define dur (if due-str ((parse/duration) dur-str) 0))
  (define desc (string-join ops " "))
  (define event (new-meeting desc due dur))
  (new-event-and-save event))

(define (reminder-cli ops args)
  (define due-str (option-default "due" #f args))
  (define due (if due-str ((parse/time) due-str) due-str))
  (define desc (string-join ops " "))
  (define event (new-reminder desc due))
  (new-event-and-save event))

(define (remove-event-cli ops args)
  (define event-data (find-event ops args))
  (define found (car event-data))
  (define rest (cdr event-data))
  (save-data rest))


;; More CLI utils
(define top-level
  (make-level "display"
              `(("display"  . ,display-all)
                ("task"     . ,task-cli)
                ("todo"     . ,task-cli)
                ("meeting"  . ,meeting-cli)
                ("reminder" . ,reminder-cli)
                ("rm"       . ,remove-event-cli)
                ("remove"   . ,remove-event-cli)
                ("done"     . ,remove-event-cli)
                ("finish"   . ,remove-event-cli)
                ("dismiss"  . ,remove-event-cli))))

(define (main args)
  (define ops (cdr (assoc 'anon args)))
  (define default "display")
  (parameterize ((ignore-case #t)) 
    (top-level ops args)))

(main
  (parseargs
    '((#:str "o" "display")
      (#:str "d" "due")
      (#:str "r" "repeat")
      (#:str "l" "duration"))))
#|
; Manual testing
(define (is-r x)
  (print (todo? x))
  (print (meeting? x))
  (print (reminder? x))
  (print " ")
  x)
(writeln (is-r (new-todo "finish this!" #f)))
(writeln (is-r (new-meeting "standup" ((parse/time) "monday@8am") ((parse/duration) "30m"))))
(writeln (is-r (new-reminder "dont forget" ((parse/time) "tomorrow"))))
(writeln (has-past? (new-todo "finish this!" #t ((parse/time) "today@8am"))))
(writeln (has-past? (new-todo "finish this!" #f ((parse/time) "tomorrow@8am"))))
(writeln (todo-done? (new-todo "finish this!" #t ((parse/time) "today@8am"))))
(writeln (todo-done? (new-todo "finish this!" #f ((parse/time) "tomorrow@8am"))))
(writeln (meeting-ongoing? (new-meeting "test1"
                                        ((parse/time) "today@3pm")
                                        ((parse/duration) "30m"))))


(println  (new-meeting "test1"
                       ((parse/time) "today@3pm")
                       ((parse/duration) "30m")))
(println (event->json-string (new-meeting "test1"
                                          ((parse/time) "today@3pm")
                                          ((parse/duration) "30m"))))
(println (json-string->event
          (event->json-string (new-meeting "test1"
                                           ((parse/time) "today@3pm")
                                           ((parse/duration) "30m")))))

(parameterize ((ignore-case #t)) 
  (println ((parse/time) "tomorrow@8am"))
  (println ((parse/time) "dec8@8am"))
  (println ((parse/time) "10:45pm"))
  (println ((parse/time) "11"))
  (println ((parse/time) "tu"))
  (println ((parse/time) "we"))

  (println ((parse/duration) "30m"))
  (println ((parse/duration) "2houRs"))

  (println ((parse/repeats) "MoWeFr"))
  (println ((parse/repeats) "Daily"))
  (println ((parse/repeats) "Weekly"))
  (println ((parse/repeats) "Monthly"))
  (println ((parse/repeats) "Yearly"))
  (println ((parse/repeats) "4days"))
  (println ((parse/repeats) "2weeks")))

|#
