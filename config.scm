(define-module (jlife config)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:export (store-path
            read-config
            data-file-path))

;; stick these in jlib somewhere
(define (mkdir-if path)
  (if (file-exists? path)
    (values)
    (mkdir path)))

;; config defaults
(define config-defaults
  '((backend . json)
    (data-file . "data")
    (pretty-upcoming-meeting-count . 5)))

(define (data-file-path)
  (define config (read-config))
  (define data-file (assoc-get 'data-file config))
  (string-append (store-path) data-file))

(define (store-path)
  (define env (getenv "JLIFE_LOCATION"))
  (if env env (string-append (getenv "HOME") "/.jlife/")))

(define (read-config)
  (define path (string-append (store-path) "config"))
  (mkdir-if (store-path))

  (unless (file-exists? (string-append (store-path) "config"))
    (with-output-to-file path
      (lambda ()
        (write '())
        (newline))))
  (with-input-from-file path
    (lambda ()
      (define new-vals (read))
      (define old-vals config-defaults)
      (append new-vals old-vals))))

(define (term-width)
  (define env (getenv "COLUMNS"))
  (if env (string->number env) 80))

(define (term-height)
  (define env (getenv "LINES"))
  (if env (string->number env) 24))
