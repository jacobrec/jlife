(define-module (jlife diff-data)
  #:use-module (jlib print)
  #:use-module (jlife backend)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (srfi srfi-1)
  #:export (jlife-data
            diff-add-event
            diff-remove-event
            diff-reset-master))

(define (diff-remove? x)
  (equal? '("jdiff" . "sub") (assoc "jdiff" (event-notes x))))
(define (diff-add? x)
  (equal? '("jdiff" . "add") (assoc "jdiff" (event-notes x))))

(define (process-diff-on-master diff master)
  (cond
    ((diff-add? diff) (cons (event-remove-note! diff "jdiff") master))
    ((diff-remove? diff) (filter (lambda (y) (not (equal? y (event-remove-note! diff "jdiff")))) master))
    (else (println "Error item not a diff") (values))))
(define (jlife-data)
  (define master (load-data))
  (define diff (load-diff))
  (fold process-diff-on-master master diff))

(define (diff-add-event event)
  (add-diff (event-add-note! event '("jdiff" . "add"))))
(define (diff-remove-event event)
  (define diff (load-diff))
  (define diff-len (length diff))
  (define removed-diff (filter
                         (lambda (y)
                           (not (equal? event (event-remove-note! (map (lambda (x) x) y) "jdiff"))))
                         diff))
  (define removed-diff-len (length removed-diff))
  (if (= diff-len removed-diff-len)
    (add-diff (event-add-note! event '("jdiff" . "sub")))
    (save-diff removed-diff)))
(define (add-diff diff)
  (save-diff (cons diff (load-diff))))

(define (diff-reset-master)
  (save-data (jlife-data))
  (save-diff '()))
