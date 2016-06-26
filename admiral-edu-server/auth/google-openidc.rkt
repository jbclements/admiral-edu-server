#lang typed/racket/base

(require/typed web-server/http/bindings
               [extract-binding/single (Symbol (Listof (Pairof Symbol String)) -> String)]
               [exists-binding? (Symbol (Listof (Pairof Symbol String)) -> Boolean)]
               [request-headers (Any -> (Listof (Pairof Symbol String)))])

(provide req->uid)

(define (req->uid req)
  (headers->uid (request-headers req)))

(: headers->uid ((Listof (Pairof Symbol String)) -> (U String 'invalid-session)))
(define (headers->uid headers)
  (if (exists-binding? 'oidc_claim_email headers)
      (extract-binding/single 'oidc_claim_email headers)
      'invalid-session))


