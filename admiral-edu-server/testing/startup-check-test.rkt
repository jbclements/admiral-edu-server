#lang racket

(require "../storage/storage-basic.rkt"
         "../configuration.rkt"
         "test-configuration.rkt"
         racket/hash)

;; copied from test-configuration.rkt because of
;; weird chaperone error.
(define (modified-test-conf table)
  (define correct-key-names (hash-keys test-conf))
  (for ([k (in-hash-keys table)])
    (unless (member k correct-key-names)
      (raise-argument-error
       'modified-test-conf
       (format "table containing valid key names (failed on '~a')"
               k)
       0 table)))
  (hash-union test-conf
              table
              #:combine (λ (a b) b)))

(module+ test
  (require typed/rackunit)

  (parameterize ([current-configuration
                  (modified-test-conf
                   (hash "storage-mode" "cloud-storage"
                         "bucket" "nonexistent-bucket/"))])
  (check-exn
   #px"^HTTP"
   (λ () (startup-check))))

  (parameterize ([current-configuration
                  (modified-test-conf
                   (hash "storage-mode" "local"))])
  (check-equal?
   (startup-check)
   (void))))