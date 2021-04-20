(define-module (jlife sync)
  #:use-module (jlib print)
  #:export (jlife-sync jlife-sync-download jlife-sync-upload))

(define (jlife-sync)
  (println "Syncing..."))
(define (jlife-sync-download)
  (println "Syncing: Replacing local contents with server..."))
(define (jlife-sync-upload)
  (println "Syncing: Replacing server contents with local..."))

