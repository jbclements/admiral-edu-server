#lang setup/infotab

(define collection 'multi)

(define deps
  (list "base"
        "db-lib"
        "net-lib"
        "rackunit-lib"
        "typed-racket-lib"
        "web-server-lib"
        "aws"
        "yaml"
        "quickcheck"
        ;; for testing:
        "html-parsing"))

(define build-deps
  (list "typed-racket-more"))



