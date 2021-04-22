(define-module (jlife sync)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlife diff-data)
  #:use-module (jlife events)
  #:use-module (jlife backend)
  #:use-module (jlife config)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)

  #:export ( jlife-sync
             jlife-sync-download
             jlife-sync-upload
             jlife-sync-offline))

(define user (make-parameter ""))
(define server-loc (make-parameter ""))


(define-macro (catch-all perr . body)
  `(jcatch-all ,perr (lambda () ,@body)))
(define (jcatch-all perr thunk)
  (with-exception-handler
    (lambda (exn)
      (when perr
        (format #t "Failed to connect to server\n" exn))
      #t)
    thunk
    #:unwind? #t))

(define (with-server print-configured-error fn)
  (define server-config (assoc-get 'server (read-config)))
  (when server-config
    (parameterize ((user (second server-config))
                   (server-loc (first server-config)))
      (catch-all print-configured-error (fn))))
  (when (and print-configured-error (not server-config))
    (println "No server configured")))

(define (get-data-from-server)
  (get-data-from-loc (string-append "http://" (server-loc) "/data/" (user))))
(define (get-data-from-loc loc)
  (define-values (res data) (http-get loc))
  (values
    (map json-scm->event (vector->list (assoc-get "data" (json-string->scm (utf8->string data)))))
    (= 200 (response-code res))))

(define (http-post-json loc body-str)
  (define-values (res data) (http-post loc #:body body-str))
  (values
    (json-string->scm (utf8->string data))
    (= 200 (response-code res))))


(define (jlife-sync with-errors)
  (with-server with-errors
    (lambda ()
      (when with-errors
        (println "Syncing..."))
      (define diff (load-diff))
      (define-values (data success)
        (http-post-json
          (string-append "http://" (server-loc) "/data")
          (scm->json-string `((data . ,(list->vector (map event->json-scm diff)))
                              (uid . ,(user))))))
      (define serverdata (map json-scm->event (vector->list (assoc-get "data" data))))
      (if success
          (begin
            (save-data serverdata)
            (save-diff '()))
          (println "Syncing failed")))))



(define (jlife-sync-download)
  (with-server #t
    (lambda ()
      (define-values (data success) (get-data-from-server))
      (println "Syncing: Replacing local contents with server...")
      (if success
          (begin
            (save-data data)
            (save-diff '()))
          (println "Syncing failed")))))


(define (jlife-sync-upload)
  (with-server #t
    (lambda ()
      (println "Syncing: Replacing server contents with local...")
      (define localdata (jlife-data))
      (define-values (data success)
        (http-post-json
          (string-append "http://" (server-loc) "/data-replace")
          (scm->json-string `((data . ,(list->vector (map event->json-scm localdata)))
                              (uid . ,(user))))))
      (define serverdata (map json-scm->event (vector->list (assoc-get "data" data))))
      (if success
          (begin
            (save-data serverdata)
            (save-diff '()))
          (println "Syncing failed")))))

(define (jlife-sync-offline)
  (println "Syncing: Applying diff to data...")
  (diff-reset-master))

