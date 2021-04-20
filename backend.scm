(define-module (jlife backend)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:export (load-data
            load-diff
            save-data
            save-diff))

(define backends '())

(define (load-data)
  (io-er with-input-from-file (lambda (back) ((first back))) (data-file-path)))

(define (load-diff)
  (io-er with-input-from-file (lambda (back) ((first back))) (diff-file-path)))

(define (save-data data)
  (io-er with-output-to-file (lambda (back) ((second back) data)) (data-file-path)))

(define (save-diff data)
  (io-er with-output-to-file (lambda (back) ((second back) data)) (diff-file-path)))

(define (io-er manip fn file)
  (define config (read-config))
  (define back (assoc-get (assoc-get 'backend config) backends))
  (unless (file-exists? file)
    (with-output-to-file file (lambda ()
                                (println (third back)))))
  (if back
    (manip file (lambda () (fn back)))))


(define (create-backend name loader saver default)
  (set! backends (cons (list name loader saver default)
                       backends)))

(define (read-json)
  (json-string->event-list (get-string-all (current-input-port))))
(define (write-json data)
  (display (event-list->json-string data)))

(create-backend 'scm read write "()")
(create-backend 'json read-json write-json "[]")


