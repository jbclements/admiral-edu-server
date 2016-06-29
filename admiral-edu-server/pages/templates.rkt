#lang racket/base

;; include-template apparently doesn't play nice with TR
;; (I guess I'm not too surprised)

(require racket/list
         web-server/templates
         xml)

(provide ;; FIXME get rid of this one:
         string->plain-page-html
         xexprs->plain-page-html
         xexpr->error-page-html
         xexprs->error-page-html)

;; FIXME temporary function... replace with xexpr-based thing.
(define (string->plain-page-html title body)
  (include-template "html/plain.html"))

;; given a title and a list of xexprs, return
;; a string representing html text
(define (xexprs->plain-page-html title xexprs)
  (define body (xexprs->1string xexprs))
  (include-template "html/plain.html"))

;; given an xexpr, returns an error page embedding that
;; message. Note that the good behavior of xexpr->string
;; ensures that passing this a string works too.
(define (xexpr->error-page-html xexpr)
  (xexprs->error-page-html (list xexpr)))

;; given a list of xexprs, returns an error page etc. etc.
(define (xexprs->error-page-html xexprs)
  (define display-message (xexprs->1string xexprs))
  (include-template "html/error.html"))

;; use xexpr->string on each, join with newlines
(define (xexprs->1string xexprs)
  (apply string-append
         (add-between (map xexpr->string xexprs) "\n")))


(module+ test
  (require rackunit)

  (check-match
   (xexpr->error-page-html "abc<i>wow</i>tag")
   (regexp #px"&lt;i&gt;wow&lt;/i&gt;"))

  (check-match
   (xexpr->error-page-html '(p "abc" (i "wow") "tag") )
   (regexp #px"abc<i>wow</i>tag"))

  (check-match
   (xexprs->plain-page-html '((p "goofy")))
   (regexp #px"<p>goofy</p>")))