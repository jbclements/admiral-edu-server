#lang racket/base

;; THE IDEA IS TO ALLOW THESE USES TO MOVE
;; UPWARD AND COALESCE AND DISAPPEAR.
(require racket/list
         xml
         "pages/templates.rkt")

(provide dreadful-hack
         error)

(define (dreadful-hack xexprs)
  (apply string-append (add-between (map xexpr->string xexprs) "\n")))

;; FIXME replace with something that produces a reasonable error code
(provide XXerror)
(define (XXerror message)
  (xexpr->error-page-html message))

