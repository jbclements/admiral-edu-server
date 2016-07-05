#lang racket/base

;; THE IDEA IS TO ALLOW THESE USES TO MOVE
;; UPWARD AND COALESCE AND DISAPPEAR.
(require racket/list
         xml
         "pages/templates.rkt"
         "pages/responses.rkt")

(provide dreadful-hack
         error
         render-hack)

(define (dreadful-hack xexprs)
  (apply string-append (add-between (map xexpr->string xexprs) "\n")))

;; FIXME replace with something that produces a reasonable error code
(provide XXerror)
(define (XXerror message)
  (xexpr->error-page-html message))


(define (render-hack result)
  (cond [(response? result) result]
        [(string? result) (string->response result)]
        [(and (list? result)
              (andmap xexpr? result))
         (xexprs->response result)]
        [else
         (raise-argument-error 'render-hack
                               "response or string or list of xexprs"
                               0 result)]))

