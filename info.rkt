#lang setup/infotab

(define collection 'multi)

(define compile-omit-paths '("testing/regression-comparison.rkt"))
(define test-omit-paths '("testing/regression-comparison.rkt"))

(define deps
  (list "base"
        "db-lib"
        "net-lib"
        "rackunit-lib"
        "typed-racket-lib"
        "web-server-lib"
        "aws"
        "yaml"
        "quickcheck"))

(define build-deps
  (list "typed-racket-more"))



