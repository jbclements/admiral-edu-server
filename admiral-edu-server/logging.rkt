#lang typed/racket/base

;; provide some basic logging mechanism. One level higher
;; than printf.


(provide log-ct-access-info
         log-ct-access-info/nomacro
         log-ct-error-info
         log-ct-error-info/nomacro)

(define-logger ct-access)
(define-logger ct-error)

(define access-listener (make-log-receiver ct-access-logger 'info))
(define error-listener (make-log-receiver ct-error-logger 'info))

;; these are necessary because typed macros can't be used in untyped
;; code. You lose the macro's "hide when no one is listening" behavior,
;; but that's okay in this case
(: log-ct-access-info/nomacro (String -> Void))
(define (log-ct-access-info/nomacro str)
  (log-ct-access-info str))

(: log-ct-error-info/nomacro (String -> Void))
(define (log-ct-error-info/nomacro str)
  (log-ct-error-info str))

;; for now, all logs go to stdout

(: dump-listener-to-stdout (Log-Receiver -> Void))
(define (dump-listener-to-stdout listener)
  (thread
   (λ ()
     (let loop ()
       (define message (sync listener))
       (printf "~a\n" (vector-ref message 1))
       (flush-output)
       (loop))))
  (void))

(dump-listener-to-stdout access-listener)
(dump-listener-to-stdout error-listener)