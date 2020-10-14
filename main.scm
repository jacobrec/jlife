(define-module (jlife main)
  #:use-module (jlib parse)
  #:use-module (jlife dateparser)
  #:use-module (jlife durationparser)
  #:use-module (jlife repetitionparser)
  #:use-module (jlife events)
  #:use-module (jlib print))


(define (writeln x)
  (write x)
  (newline))
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

#;
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
