
#lang racket/base

;; include-template apparently doesn't play nice with TR
;; (I guess I'm not too surprised)

(require racket/list
         web-server/templates
         xml
         racket/contract
         web-server/servlet)

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

;; this is a simple way of ensuring that certain templates
;; are included only in this context:
(define bogusbinding "")

;; given a URL specification, return a URL.
;; currently the identity.
;; FIXME come up with an abstract representation of a
;; captain teach URL that fences out weird errors
(define (urlgen url)
  url)
(define ct-url? string?)

(define (maybe-hidden-class hidden?)
  (if hidden? "hidden" ""))

;; given a boolean indicating whether a checkbox is
;; checked, return a string to be placed inside the
;; 'input' tag.
(define (checkbox-checked c)
  (if c "CHECKED" ""))

;; given a list of xexprs, convert them to strings
;; for use in a template
(define (xexprs->string xexprs)
  (apply string-append (map xexpr->string xexprs)))

;; FIXME return responses rather than strings!

;; given values for the fields, construct the feedback
;; page using the template
(provide (contract-out
          [feedback-page (-> ct-url? ct-url? (listof xexpr?) boolean? xexpr?
                             response?)]))
(define (feedback-page load-url file-container display-message review-flagged? review-feedback)
  (response-200
   (include-template "html/feedback.html")))

;; given values for the fields, construct the review
;; page using the template
(provide (contract-out
          [review-page (-> ct-url? ct-url? ct-url? (listof xexpr?) boolean? ct-url? response?)]))
(define (review-page save-url load-url file-container no-modifications submit-hidden? submit-url)
  (response-200
   (include-template "html/review.html")))

;; wrap a string as a 200 Okay response. The idea is to use
;; this only directly on the result of a template
(define (response-200 str)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   '()
   (list (string->bytes/utf-8 str))))

(module+ test
  (require rackunit)

  (check-match
   (xexpr->error-page-html "abc<i>wow</i>tag")
   (regexp #px"&lt;i&gt;wow&lt;/i&gt;"))

  (check-match
   (xexpr->error-page-html '(p "abc" (i "wow") "tag") )
   (regexp #px"abc<i>wow</i>tag"))

  (check-match
   (xexprs->plain-page-html "Quadra!" '((p "goofy")))
   (regexp #px"Quadra!.*<p>goofy</p>")))