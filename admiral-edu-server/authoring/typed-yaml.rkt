#lang typed/racket/base

(require/typed yaml
               [(string->yaml lib:string->yaml) (String -> YAML)])

(provide YAML
         string->yaml)

(define-type YAML (Rec Y (U YAML-ATOM
                            '()
                            (Pairof Y Y)
                            (HashTable YAML-ATOM Y)
                            (Setof Y))))

(define-type YAML-ATOM (U 'null
                          Boolean
                          String
                          Integer
                          Inexact-Real
                          Bytes
                          date))

(: string->yaml (String -> (HashTable String Any)))
(define (string->yaml s)
  (define yaml (lib:string->yaml s))
  (cond [(and (hash? yaml)
              (andmap string? (hash-keys yaml)))
         ;; cast is necessary here to protect
         ;; from mutation, I believe:
         (cast yaml (HashTable String Any))]
        [else (raise-argument-error 'string->yaml
                                    "string parsable as YAML table"
                                    0 s)]))




