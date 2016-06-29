#lang typed/racket/base

(require/typed xml
               [xexpr->string (XExpr -> String)])

(require/typed "pages/templates.rkt"
               [string->plain-page-html (String String -> (Listof Bytes))])

(require racket/match
         racket/string
         racket/list
         (prefix-in error: "pages/errors.rkt")
         "pages/typed-xml.rkt"
         "pages/responses.rkt"
         "base.rkt"
         "ct-session.rkt")

(require (prefix-in assignments: "pages/assignments.rkt"))

(: user-exists? (ct-session -> Boolean))
(define (user-exists? session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:exists? class uid)))
    result))

(provide handlerPrime)
(: handlerPrime (Boolean Any ct-session Any Any (Listof String) -> Response))
(define (handlerPrime post post-data session bindings raw-bindings path)
  (match path
    [(cons "assignments" url) (render-xexprs "Assignments" session
                                             assignments:load url post)]
    [else (error:four-oh-four-response)]))


(: render-xexprs (String ct-session
                         (ct-session (Listof String) Boolean ->
                                     (U Response
                                        (Listof XExpr)))
                         (Listof String) Boolean
                         -> Response))
(define (render-xexprs title session page url post)
  (let ((valid-user (user-exists? session)))
    (if (not valid-user)
        (error:not-registered-response session)
        (let ()
          (define page-result (page session url post))
          (cond [(response? page-result) page-result]
                [else (xexprs->plain-page-response title page-result)])))))