(define-module (jlife backend)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlife config)
  #:use-module (jlife events)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:export (load-data
            save-data))

(define backends '())

(define (load-data)
  (io-er with-input-from-file (lambda (back) ((first back)))))

(define (save-data data)
  (io-er with-output-to-file (lambda (back) ((second back) data))))

(define (io-er manip fn)
  (define config (read-config))
  (define back (assoc-get (assoc-get 'backend config) backends))
  (unless (file-exists? (data-file-path))
    (with-output-to-file (data-file-path) (lambda ()
                                            (println (third back)))))
  (if back
    (manip (data-file-path) (lambda () (fn back)))))


(define (create-backend name loader saver default)
  (set! backends (cons (list name loader saver default)
                       backends)))

(define (read-json)
  (json-string->event-list (get-string-all (current-input-port))))
(define (write-json data)
  (display (event-list->json-string data)))

(create-backend 'scm read write "()")
(create-backend 'json read-json write-json "[]")


