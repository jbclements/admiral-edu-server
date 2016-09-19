#lang setup/infotab

(define collection 'multi)

(define deps
  (list
   "aws"
   "base"
   "db-lib"
   "net-lib"
   "typed-racket-lib"
   "web-server-lib"
   "yaml"))

(define build-deps
  (list "rackunit-lib"
        "typed-racket-more"))

(define pkg-desc "The Racket Captain Teach server")



