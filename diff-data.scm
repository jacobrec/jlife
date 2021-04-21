(define-module (jlife diff-data)
  #:use-module (jlib print)
  #:use-module (jlife backend)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 copy-tree)
  #:export (jlife-data
            diff-add-event
            diff-remove-event
            diff-reset-master
            process-diffs-on-master))

(define (diff-remove? x)
  (equal? '("jdiff" . "sub") (assoc "jdiff" (event-notes x))))
(define (diff-add? x)
  (equal? '("jdiff" . "add") (assoc "jdiff" (event-notes x))))

(define (process-diff-on-master diff master)
  (cond
    ((diff-add? diff) (cons (event-remove-note! diff "jdiff") master))
    ((diff-remove? diff) (filter (lambda (y) (not (event=? y diff))) master))
    (else (println "Error item not a diff") (values))))
(define (process-diffs-on-master diffs master)
  (fold process-diff-on-master master diffs))
(define (jlife-data)
  (define master (load-data))
  (define diff (load-diff))
  (process-diffs-on-master diff master))

(define (diff-add-event event)
  (add-diff (event-add-note! event '("jdiff" . "add"))))
(define (diff-remove-event event)
  (define diff (load-diff))
  (define diff-len (length diff))
  (define removed-diff (filter
                         (lambda (y)
                           (not (equal? event (event-remove-note! (copy-tree y) "jdiff"))))
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

(define (generate-diffs-from-masters m1 m2)
  (define ignore (lset-intersection event=? m1 m2))
  (define added (lset-difference event=? m2 ignore))
  (define removed (lset-difference event=? m1 ignore))
  (append (map (lambda (x) (event-add-note! x '("jdiff" . "add"))) added)
          (map (lambda (x) (event-add-note! x '("jdiff" . "sub"))) removed)))

#|;; generate-diffs-from-masters test
(println
  (equal?
    '((todo "test4" #f (("jdiff" . "add") ("done" . #f)))
      (todo "test1" #f (("jdiff" . "sub") ("done" . #f))))
    (generate-diffs-from-masters
      (copy-tree
        '((todo "test1" #f (("done" . #f)))
          (todo "test2" #f (("done" . #f)))
          (todo "test3" #f (("done" . #f)))))
      (copy-tree
        '((todo "test4" #f (("done" . #f)))
          (todo "test2" #f (("done" . #f)))
          (todo "test3" #f (("done" . #f))))))))
;;|#

#|;; process-diff-on-master test
(define master '((todo "test1" #f (("done" . #f)))
                 (todo "test2" #f (("done" . #f)))
                 (todo "test3" #f (("done" . #f)))))
(define diff '((todo "test4" #f (("done" . #f) ("jdiff" . "add")))
               (todo "test1" #f (("jdiff" . "sub") ("done" . #f)))))
(define result '((todo "test4" #f (("done" . #f)))
                 (todo "test2" #f (("done" . #f)))
                 (todo "test3" #f (("done" . #f)))))
(println (equal? (process-diffs-on-master (copy-tree diff) (copy-tree master)) result))
;;|#


