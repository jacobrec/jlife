(define-module (jlife profile)
  #:use-module (jlib print)
  #:use-module (jlib shell)
  #:use-module (jlife config)
  #:use-module (jlife backend)
  #:use-module (jlife frontend)
  #:export (profile-add
            profile-remove
            profile-use
            profile-list))

(define (profile-list)
  (println "jlife profiles list"))

(define (profile-add p)
  (when (has-profile p)
    (println (string-append "Profile " p " already exists")))
  (unless (has-profile p)
    (println (string-append "Added profile " p))
    (call-with-output-file (profile-path p)
      (lambda (port)
        (write '() port)))))

(define (profile-rm p)
  (unless (has-profile p)
    (println (string-append "Failed to find profile " p)))
  (when (has-profile p)
    (println (string-append "Removing profile " p))
    (delete-file (profile-path p))))

(define (profile-use p)
  (unless (has-profile p)
    (println (string-append "Failed to find profile " p)))
  (when (has-profile p)
    (println (string-append "Using profile " p))
    (delete-file (profile-path ""))
    (symlink (profile-path p)
             (profile-path ""))))

(define (profile-path p)
  (define pre (string-append (getenv "HOME") "/.jlife/data"))
  (if (= 0 (string-length p))
    pre
    (string-append pre "-" p)))

(define (has-profile p)
  (file-exists? (profile-path p)))

