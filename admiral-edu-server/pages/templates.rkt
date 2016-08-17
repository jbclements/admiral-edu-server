
#lang racket/base

;; include-template apparently doesn't play nice with TR
;; (I guess I'm not too surprised)

(require racket/list
         web-server/templates
         xml
         racket/contract
         web-server/servlet
         "../paths.rkt")

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
;; FIXME disallow strings and move everything
;; over to custom Url-Path structure to prevent
;; XSS vulns
(define (urlgen url)
  (cond [(string? url) url]
        [(url-path? url) (url-path->url-string url)]))
(define ct-url? (or/c string? url-path?))

(define (maybe-hidden-class hidden?)
  (if hidden? "hidden" ""))

;; given a boolean indicating whether a checkbox is
;; checked, return a string to be placed inside the
;; 'input' tag.
(define (checkbox-checked c)
  (if c "CHECKED" ""))

(define (safe-id? c)
  (and (string? c) (legal-path-elt? c)))

(define (safe-id s)
  (unless (safe-id? s)
    (raise-argument-error 'safe-id "simple alphanumeric id" 0 s))
  (xexpr->string s))

(define (ct-url-or-false s)
  (cond [(false? s) "false"]
        ;; FIXME not sure about the definition of javascript
        ;; string quoting
        [(url-path? s)
         (js-str-format (url-path->url-string s))]
        [(string? s)
         ;; FIXME ELIMINATE WHEN POSSIBLE:
         (js-str-format s)]))

;; wrap in single quotes, map ' to \' and \ to \\
;; NOTE: this will not work for strings with newlines, "reverse solidus"es
;; and other nutty stuff. This is designed to work for things that
;; come out of url-path->url-string
(define (js-str-format s)
  (string-append
   "'"
   (regexp-replace* #px"'"
                    (regexp-replace* #px"\\\\" s "\\\\\\\\")
                    "\\\\'")
   "'"))

(define (false? s)
  (eq? s #f))



;; FIXME the set of safe filenames should *definitely* be checked
;; further upstream.
(define (filename? f)
  (and (string? f) (regexp-match #px"^[-_a-zA-Z0-9\\.]+$" f)))

(define (filename f)
  (unless (filename? f)
    (raise-argument-error 'filename "legal filename" 0 f))
  f)

;; blecch...
(define (filename-or-empty? f)
  (or (equal? f "") (filename? f)))

(define (filename-or-empty f)
  #;(unless (filename-or-empty? f)
    (raise-argument-error 'filename-or-empty "legal filename" 0 f))
  f)

;; given a list of xexprs, convert them to strings
;; for use in a template
(define (xexprs->string xexprs)
  (apply string-append (map xexpr->string xexprs)))

;; given a string, ensure that it doesn't contain
;; backslashes, single- or double-quotes
(define (js-str? s)
  (and (string? s) (regexp-match #px"^[^\\\\\"']+$" s)))

(define (js-str s)
  (unless (js-str? s)
    (raise-argument-error 'js-str "javascript string piece" 0 s))
  s)

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

;; given the values for the fields, construct the dependencies
;; page using the template
(provide (contract-out
          [dependencies-page (-> ct-url? (listof xexpr?) response?)]))
(define (dependencies-page load-url dependency-form)
  (response-200
   (include-template "html/dependency.html")))

;; given the values for the fields, construct the authoring page
;; using the template
(provide (contract-out
          [authoring-page (-> safe-id? ct-url? (listof xexpr?)
                              (listof xexpr?)
                              response?)]))
(define (authoring-page class-name save-url content message)
  (response-200
   (include-template "html/authoring.html")))

;; given values for the fields, construct the file-container page
;; using the template
(provide
 (contract-out
  [file-container-page
   (-> js-str? ct-url? ct-url? safe-id? xexpr? (listof xexpr?) (listof xexpr?)
       (or/c ct-url? false?) response?)]))
(define (file-container-page default-mode save-url load-url assignment step path content file-url)
  (response-200
  (include-template "html/file-container.html")))

;; given values for the fields, construct the browse-file-container page
(provide
 (contract-out
  [browse-file-container-page
   (-> safe-id? xexpr? (listof xexpr?) js-str? (listof xexpr?)
       (or/c ct-url? false?) string?)]))
(define (browse-file-container-page assignment step path default-mode content file-url)
  ;; FIXME wrap with response-200
  (include-template "html/browse-file-container.html"))

;; given values for the fields, construct the feedback-file-container page
(provide
 (contract-out
  [feedback-file-container-page
   (-> safe-id? xexpr? (listof xexpr?) js-str? (listof xexpr?) ct-url?
       (or/c ct-url? false?) response?)]))
(define (feedback-file-container-page assignment step path default-mode content load-url file-url)
  (response-200
   (include-template "html/feedback-file-container.html")))

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

  (check-not-exn
   (λ ()
     (browse-file-container-page
      "abc" `(i "wiper") (list "ath" "ghi.def") "abcd"
      '("contenty" (i "mumble"))
      (strs->abs-ct-path/testing (list "waffle-house" "abc.txt")))))

  (check-not-exn
   (λ ()
     (browse-file-container-page
      "abc" `(i "wiper") (list "ath" "ghi.def") "abcd"
      '("contenty" (i "mumble"))
      #f)))
  
  (check-match
   (xexpr->error-page-html "abc<i>wow</i>tag")
   (regexp #px"&lt;i&gt;wow&lt;/i&gt;"))

  (check-match
   (xexpr->error-page-html '(p "abc" (i "wow") "tag") )
   (regexp #px"abc<i>wow</i>tag"))

  (check-match
   (xexprs->plain-page-html "Quadra!" '((p "goofy")))
   (regexp #px"Quadra!.*<p>goofy</p>"))
  
  (check-equal? (ct-url-or-false #f) "false")
  (check-equal? (ct-url-or-false "abc") "'abc'")
  (check-equal? (ct-url-or-false "abc'de'\\n\\" )
                "'abc\\'de\\'\\\\n\\\\'")
  (check-equal? (ct-url-or-false (strs->abs-ct-path/testing
                                  (list "big" "wig '\\dig")))
                "'/big/wig%20\\'%5Cdig'"))