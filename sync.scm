(define-module (jlife sync)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlife diff-data)
  #:use-module (jlife events)
  #:use-module (jlife backend)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)

  #:export ( jlife-sync
             jlife-sync-download
             jlife-sync-upload
             jlife-sync-offline))

(define user "jacob")
(define server-loc "localhost:8080")

(define (get-data-from-server)
  (get-data-from-loc (string-append "http://" server-loc "/data/" user)))
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


(define (jlife-sync)
  (println "Syncing...")
  (get-data-from-server))


(define (jlife-sync-download)
  (define-values (data success) (get-data-from-server))
  (println "Syncing: Replacing local contents with server...")
  (if success
      (begin
        (save-data data)
        (save-diff '()))
      (println "Syncing failed")))


(define (jlife-sync-upload)
  (println "Syncing: Replacing server contents with local...")
  (define localdata (jlife-data))
  (define-values (data success)
    (http-post-json
      (string-append "http://" server-loc "/data-replace")
      (scm->json-string `((data . ,(list->vector (map event->json-scm localdata)))
                          (uid . "jacob")))))
  (define serverdata (map json-scm->event (vector->list (assoc-get "data" data))))
  (save-data serverdata)
  (save-diff '()))

(define (jlife-sync-offline)
  (println "Syncing: Applying diff to data...")
  (diff-reset-master))

