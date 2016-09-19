#lang racket/base

;; THE IDEA IS TO ALLOW THESE USES TO MOVE
;; UPWARD AND COALESCE AND DISAPPEAR.
(require racket/list
         xml
         "pages/templates.rkt"
         "pages/responses.rkt")

(provide dreadful-hack
         render-hack)

(define (dreadful-hack xexprs)
  (apply string-append (add-between (map xexpr->string xexprs) "\n")))


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

