#lang racket/base

;; include-template apparently doesn't play nice with TR
;; (I guess I'm not too surprised)

(require racket/list
         web-server/templates
         xml)

(provide xexpr->error-page-html
         xexprs->error-page-html)

;; given an xexpr, returns an error page embedding that
;; message. Note that the good behavior of xexpr->string
;; ensures that passing this a string works too.
(define (xexpr->error-page-html xexpr)
  (xexprs->error-page-html (list xexpr)))

;; given a list of xexprs, returns an error page etc. etc.
(define (xexprs->error-page-html xexprs)
  (define display-message
    (apply string-append
           (add-between (map xexpr->string xexprs) "\n")))
  (include-template "html/error.html"))


(module+ test
  (require rackunit)

  (check-match
   (xexpr->error-page-html "abc<i>wow</i>tag")
   (regexp #px"&lt;i&gt;wow&lt;/i&gt;"))

  (check-match
   (xexpr->error-page-html '(p "abc" (i "wow") "tag") )
   (regexp #px"abc<i>wow</i>tag")))