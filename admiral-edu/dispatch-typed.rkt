#lang typed/racket/base

(require/typed xml
               [xexpr->string (XExpr -> String)])

(require/typed "pages/templates.rkt"
               [plain-page (String (Listof XExpr) -> Response)])

(require racket/match
         racket/string
         racket/list
         (prefix-in error: "pages/errors.rkt")
         "pages/typed-xml.rkt"
         "pages/responses.rkt"
         "base.rkt"
         "ct-session.rkt"
         "email/email.rkt")

(require (prefix-in assignments: "pages/assignments.rkt"))

;; looks like this is work in progress, migrating the dispatch function
;; to typed racket. Only one case here so far.
(provide handlerPrime)
(: handlerPrime (Boolean Any ct-session Any Any Any (Listof String) -> Response))
(define (handlerPrime post post-data session role bindings raw-bindings path)
  (match path
    [(cons "assignments" url)
     (define page-result (assignments:load session url post))
     ;; FIXME: this cond should be unnecessary, result should just be list of xexprs
     (cond [(response? page-result) page-result]
           [else (plain-page
                  "Assignments"
                  page-result)])]
    [(list "send-test-email")
     (cond [(send-email (ct-session-uid session)
                        "Test Email sent by Captain Teach"
                        "This is a test email, sent in response to a request to the send-test-email endpoint.\n")
            (plain-page "Assignments"
                        '((p "okay, email sent.")))]
           [else (plain-page "Assignments"
                             '((p "email not sent; disabled, or bad uid")))]
           )]
    [else (raise-404-not-found)]))