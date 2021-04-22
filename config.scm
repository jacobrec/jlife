(define-module (jlife config)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlib files)
  #:use-module (ice-9 copy-tree)
  #:use-module (srfi srfi-1)
  #:export (store-path
            read-config
            data-file-path
            diff-file-path
            config-set-last-synced-now))


;; config defaults
(define config-defaults
  (copy-tree
    '((backend . json)
      (data-file . "data")
      (diff-file . "diff")
      (server . #f) ; (server . ("localhost:8080" "jacob" #f)) ; address, username, last synced
      (pretty-upcoming-meeting-count . 5))))

(define (save-config data)
  (with-output-to-file (string-append (store-path) "config")
                       (lambda ()
                         (writeln data))))

(define (config-set-last-synced-now)
  (define time (current-time))
  (define config (read-config))
  (define server (assoc-get 'server config))
  (when server
    (list-set! server 2 (current-time))
    (save-config (assoc-set! (assoc-remove! config 'server) 'server server))))

(define (data-file-path)
  (define config (read-config))
  (define data-file (assoc-get 'data-file config))
  (string-append (store-path) data-file))

(define (diff-file-path)
  (define config (read-config))
  (define diff-file (assoc-get 'diff-file config))
  (string-append (store-path) diff-file))


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
      (fold (lambda (x acc) (assoc-set! acc (car x) (cdr x))) old-vals new-vals))))

(define (term-width)
  (define env (getenv "COLUMNS"))
  (if env (string->number env) 80))

(define (term-height)
  (define env (getenv "LINES"))
  (if env (string->number env) 24))
