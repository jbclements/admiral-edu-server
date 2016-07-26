#lang typed/racket/base

(require/typed web-server/http/bindings
               [extract-binding/single (Symbol (Listof (Pairof Symbol String)) -> String)]
               [exists-binding? (Symbol (Listof (Pairof Symbol String)) -> Boolean)]
               [request-headers (Any -> (Listof (Pairof Symbol String)))])

(provide req->uid
         fixed-user)

;; this parameter is used for testing only. When it is not false,
;; the server treats every request as
;; being an authenticated one from a fixed user with the given
;; email address.
(: fixed-user (Parameter (U False String)))
(define fixed-user (make-parameter #f))

(define (req->uid req)
  (headers->uid (request-headers req)))

(: headers->uid ((Listof (Pairof Symbol String)) -> (U String 'invalid-session)))
(define (headers->uid headers)
  (or (fixed-user)
      (if (exists-binding? 'oidc_claim_email headers)
          (extract-binding/single 'oidc_claim_email headers)
          'invalid-session)))


