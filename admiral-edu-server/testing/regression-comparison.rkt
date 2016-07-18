#lang racket

;; this is very ad-hoc right now... looking for a way to compare
;; the output of two regression test run outputs.

(define a-tests
  (call-with-input-file "/tmp/regression-results-1468805660.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))

(define b-tests
  (call-with-input-file "/tmp/regression-results-tmp.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))


(require rackunit)

(for ([test-a (in-list a-tests)]
           [test-b (in-list b-tests)])
  
  (match-define (list i n args (list code-a code-msg-a ts-a encoding-a
                                     headers-a str-a))
    test-a)
  (match-define (list _ _ _ (list code-b code-msg-b ts-b encoding-b
                                  headers-b str-b))
    test-b)
  (unless (equal? (take test-a 2)
                  (take test-b 2))
    (error 'test-comparison
           "expected first 3 elements to be the same, got ~e and ~e"
           (take test-a 2) (take test-b 2)))
  (test-case
   (~v (list i n args))
   (check-equal? code-a code-b)
   (check-equal? code-msg-a code-msg-b)
   ;; ignore timestamp...
   (check-equal? encoding-a encoding-b)
   (check-equal? headers-a headers-b)
   (check-equal? str-a str-b)))