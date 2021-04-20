(define-module (jlife sync)
  #:use-module (jlib print)
  #:use-module (jlife diff-data)
  #:export ( jlife-sync
             jlife-sync-download
             jlife-sync-upload
             jlife-sync-offline))

(define (jlife-sync)
  (println "Syncing..."))

(define (jlife-sync-download)
  (println "Syncing: Replacing local contents with server..."))

(define (jlife-sync-upload)
  (println "Syncing: Replacing server contents with local..."))

(define (jlife-sync-offline)
  (println "Syncing: Applying diff to data...")
  (diff-reset-master))

