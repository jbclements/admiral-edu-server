#lang typed/racket/base

(require racket/match)

(provide (all-defined-out))

(: ors ((Listof Boolean) -> Boolean))
(define (ors els)
  (ormap (λ ([x : Boolean]) x) els))


(: ands ((Listof Boolean) -> Boolean))
(define (ands els)
  (andmap (λ ([x : Boolean]) x) els))


(: hash-has-keys? (All (A B) ((HashTable A B) A * -> Boolean)))
(define (hash-has-keys? hash . els)
  (ands (map (λ (x) (hash-has-key? hash x)) els)))