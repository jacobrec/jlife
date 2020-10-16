;;;; Entry point
;;;  Intended to be run every minute will notify all messages that
;;;  have gone off
;; add the following line to crontab
;;
;; * * * * * GUILE_LOAD_PATH=/home/jacob/guile guile /home/jacob/guile/jlife/reminder-monitor.scm
;;              > ~/.jlife_log.txt 2> ~/.jlife_log.err

(define-module (jlife reminder-monitor)
  #:use-module (jlife backend)
  #:use-module (jlife events)
  #:use-module (jlib print))

(define (get-past-reminders)
  (define d (load-data))
  (define (time-or-inf x) (or (event-time x) (inf)))
  (define (date-sorter a b) (< (time-or-inf a) (time-or-inf b)))
  (define data (sort d date-sorter))
  (define reminders (filter reminder? data))
  (define due-reminders (filter has-past? reminders))
  due-reminders)

(define (send-notification note)
  (system* "/home/jacob/dotfiles/bash/notify.bash" (string-append "JLife: " note)))

(define (main)
  (define items (get-past-reminders))
  (define (send-event-desc x)
    (send-notification (event-desc x))
    (usleep (inexact->exact (round (* 1000000 (/ 1.0 (length items)))))))
  (map send-event-desc items))

(main)
