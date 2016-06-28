#lang racket/base

;; provide some basic logging mechanism. One level higher
;; than printf.

(provide log-ct-access-info
         log-ct-error-info)

(define-logger ct-access)
(define-logger ct-error)

(define access-listener (make-log-receiver ct-access-logger 'info))
(define error-listener (make-log-receiver ct-error-logger 'info))

;; for now, all logs go to stdout

(define (dump-listener-to-stdout listener)
  (thread
   (λ ()
     (let loop ()
       (define message (sync listener))
       (printf "~a\n" (vector-ref message 1))
       (flush-output)
       (loop)))))

(dump-listener-to-stdout access-listener)
(dump-listener-to-stdout error-listener)