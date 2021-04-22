;;;; Entry point
;;;  The main CLI
(define-module (jlife cli)
  #:use-module (jlib argparser)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:use-module (jlib shell)
  #:use-module (jlib lists)
  #:use-module (jlife config)
  #:use-module (jlife profile)
  #:use-module (jlife diff-data)
  #:use-module (jlife sync)
  #:use-module (jlife frontend)
  #:use-module (jlife dateparser)
  #:use-module (jlife durationparser)
  #:use-module (jlife events)
  #:use-module (jlife repetitionparser)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports))

(define find-event-type (make-parameter #f))
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

;; TODO: diff mode add
(define (new-event-and-save event)
  (unless (and
            (eq? 'note (event-type event))
            (string= "" (event-desc event)))
    (diff-add-event event)))
(define (display-delete-event-and-save event rest)
  (when event
    (println "Removed:")
    (display-list (list event) #:show-type? #t)
    (delete-event-and-save event rest)))
;; TODO: diff mode remove
(define (delete-event-and-save event rest)
  (diff-remove-event event))

(define (all-events-except target)
  (define d (filter (lambda (x) (not (event=? x target))) (jlife-data)))
  d)

(define (find-event-by-substring str)
  (define data (jlife-data))
  (define events (if (find-event-type)
                     (filter (lambda (x) (eq? 'note (event-type x))) data)
                     (filter (lambda (x) (not (eq? 'note (event-type x)))) data)))
  (define-values (matches not-matches)
    (partition (lambda (x)
                 (string-contains (event-desc x) str))
               events))
  (if (= 1 (length matches))
    (cons (car matches) (all-events-except (car matches)))
    (cons #f matches)))

(define (find-event ops args)
  ;; returns a pair, car is either the found task or false if none, or
  ;; multiple tasks were found. cdr if car is false, is all found
  ;; tasks, or if car is true, all not found tasks
  (define s-str (string-join ops " "))
  (define res (find-event-by-substring s-str))
  (unless (car res)
    (println "Failed to find a single event")
    (unless (null? (cdr res))
      (println "  Multiple events found")))
  res)

(define (read-number msg)
  (println msg)
  (let ((selected (read)))
    (if (and (number? selected)
             (exact? selected))
      selected
      (read-number msg))))

(define (find-event-with-help ops args)
  (define event-data (find-event ops args))
  (define found (car event-data))
  (define rest (cdr event-data))
  (if found
    event-data
    (begin
      (display-list rest #:show-type? #t #:show-count? #t #:padding " - ")
      (if (= 0 (length rest))
        (exit 1)
        (let* ((num (read-number "Please select a number to narrow it down"))
               (found (list-ref rest num))
               (others (all-events-except found)))
          (cons found others))))))

;; cli tools
(define (display-all ops args)
  (define default "pretty")
  (define data (jlife-data))
  (cond
   ((string= "raw" (option-default "display" default args)) (display-raw data))
   ((string= "json" (option-default "display" default args)) (display-json data))
   ((string= "list" (option-default "display" default args)) (display-list data #:show-type? #t))
   ((string= "pretty" (option-default "display" default args)) (display-pretty-list data))
   (else (display-raw data))))

(define (display-help ops args)
  (println "Invalid usage.")
  (println "try 'jlife help'"))

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
  (define event-data (find-event-with-help ops args))
  (define found (car event-data))
  (define rest (cdr event-data))
  (delete-event-and-save found rest))

(define (help-cli ops args)
  (println "jlife:")
  (println "  {todo|task} description of todo item [--due DATESTRING] [--repeat REPEATSTRING]")
  (println "  notes")
  (println "  meeting description of meeting [--due DATESTING] [--repeat REPEATSTRING] [--duration DURSTRING]")
  (println "  reminder description of reminder [--due DATESTING] [--repeat REPEATSTRING]")
  (println "  {done|rm|remove|finish|dismiss} description of item to remove")
  (println "Options:")
  (println "  --display {raw|list|pretty|json}")
  (println "Formats:")
  (println "  DATESTRING= ; ex) apr12@6pm ex) thu ex) monday@14")
  (println "  REPEATSTRING={EVERYNREPEAT|REGULARREPEAT|SUBWEEKREPEAT}")
  (println "  SUBWEEKREPEAT=[mon][tue][wed][thu][fri][sat][sun] ; ex) monwedfri")
  (println "  REGULARREPEAT={daily|weekly|biweekly|monthly|yearly|anually}")
  (println "  EVERYNREPEAT=NUMBER_EVERYITEM ; ex) 2weeks")
  (println "  EVERYITEM={years|y|months|m|fortnights|f|weeks|w|days}")
  (println "  DURSTRING=NUMBER_DURITEM ; ex) 3h")
  (println "  DURITEM={weeks|w|days|d|hours|h|minutes|m|seconds|s}")
  (println "  NUMBER=\\d+"))

(define (notes-display-cli ops args)
  (define data (jlife-data))
  (display-notes data))

(define (notes-edit-cli ops args)
  (parameterize ((find-event-type 'notes))
    (let* ((event-data (find-event-with-help ops args))
           (found (car event-data))
           (rest (cdr event-data)))
      (when found
        (delete-event-and-save found rest)
        (new-event-and-save (new-note (edit (event-desc found))))))))

(define (notes-add-cli ops args) ; TODO: use command line data as initial contents
  (define new-data (edit (string-join ops " ")))
  (define event (new-note new-data))
  (new-event-and-save event))

(define (notes-rm-cli ops args)
  (parameterize ((find-event-type 'notes))
    (let* ((event-data (find-event-with-help ops args))
           (found (car event-data))
           (rest (cdr event-data)))
      (display-delete-event-and-save found rest))))
(define (notes-help-cli ops args)
  (println "jlife notes")
  (println "jlife notes edit")
  (println "jlife notes rm")
  (println "jlife notes add")
  (println "jlife notes help"))

(define notes-cli
  (make-level "display"
              `(("display"  . ,notes-display-cli)
                ("edit"     . ,notes-edit-cli)
                ("rm"       . ,notes-rm-cli)
                ("add"      . ,notes-add-cli)
                ("help"     . ,notes-help-cli))))


(define (profile-display-cli ops args)
  (profile-list))
(define (profile-add-cli ops args)
  (profile-add (car ops)))
(define (profile-rm-cli ops args)
  (profile-remove (car ops)))
(define (profile-use-cli ops args)
  (profile-use (car ops)))
(define (profile-help-cli ops args)
  (println "jlife profiles")
  (println "jlife profiles use")
  (println "jlife profiles rm")
  (println "jlife profiles add")
  (println "jlife profiles help"))

(define profile-cli
  (make-level "list"
              `(("list"     . ,profile-display-cli)
                ("add"      . ,profile-add-cli)
                ("rm"       . ,profile-rm-cli)
                ("use"      . ,profile-use-cli)
                ("help"     . ,profile-help-cli))))

(define (sync-sync-cli ops args)
  (jlife-sync #t))
(define (sync-download-cli ops args)
  (jlife-sync-download))
(define (sync-upload-cli ops args)
  (jlife-sync-upload))
(define (sync-offline-cli ops args)
  (jlife-sync-offline))

(define sync-cli
  (make-level "sync"
              `(("sync"           . ,sync-sync-cli)
                ("force-download" . ,sync-download-cli)
                ("force-upload"   . ,sync-upload-cli)
                ("offline"        . ,sync-offline-cli))))



;; More CLI utils
(define top-level
  (make-level "display"
              `(("display"  . ,display-all)
                ("task"     . ,task-cli)
                ("profile"  . ,profile-cli)
                ("profiles" . ,profile-cli)
                ("sync"     . ,sync-cli)
                ("todo"     . ,task-cli)
                ("notes"    . ,notes-cli)
                ("note"     . ,notes-cli)
                ("meeting"  . ,meeting-cli)
                ("reminder" . ,reminder-cli)
                ("rm"       . ,remove-event-cli)
                ("remove"   . ,remove-event-cli)
                ("done"     . ,remove-event-cli)
                ("finish"   . ,remove-event-cli)
                ("dismiss"  . ,remove-event-cli)
                ("help"     . ,help-cli))))

(define (main args)
  (define ops (cdr (assoc 'anon args)))
  (define default "display")
  (parameterize ((ignore-case #t))
    (top-level ops args))
  (jlife-sync #f))

(main
  (parseargs
    '((#:str "o" "display")
      (#:str "d" "due")
      (#:str "r" "repeat")
      (#:str "l" "duration" "for"))))

;; Manual testing - removed a whole bunch of test cases. Last in 40f9180
