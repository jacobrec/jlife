(define-module (jlife profile)
  #:use-module (jlib print)
  #:use-module (jlib shell)
  #:use-module (jlib strings)
  #:use-module (jlife config)
  #:use-module (jlife backend)
  #:use-module (jlife frontend)
  #:export (profile-add
            profile-remove
            profile-use
            profile-list))

(define jlife-dir (string-append (getenv "HOME") "/.jlife/"))

(define (profile-list)
  (define d (opendir jlife-dir))
  (define (loop)
    (define entry (readdir d))
    (unless (eof-object? entry)
      (when (starts-with entry "data-")
        (println (substring entry 5)))
      (loop)))
  (loop))

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
  (define pre (string-append jlife-dir "data"))
  (if (= 0 (string-length p))
    pre (string-append pre "-" p)))

(define (has-profile p)
  (file-exists? (profile-path p)))

