#lang typed/racket/base

(require "../database/mysql/review.rkt"
         )

(require/typed "testing-shim.rkt"
               [class-name-shim (-> String)])

(provide pending-review-hashes)

(: pending-review-hashes (String String -> (Listof String)))
(define (pending-review-hashes assignment uid)
  (map Record-hash
       (select-pending assignment (class-name-shim) uid)))