#lang racket/base

;; include-template apparently doesn't play nice with TR
;; (I guess I'm not too surprised)

(require racket/list
         racket/port
         web-server/templates
         xml
         racket/contract
         web-server/servlet
         "../paths.rkt")

(provide (contract-out
          [plain-page (-> string? (listof xexpr?) response?)]
          [basic-page (-> xexpr? (listof xexpr?) (listof xexpr?)
                          response?)])
         error-page)

;; given a title and a list of xexprs, return
;; a string representing html text
(define (plain-page title body)
  (response-200
   (include-template "html/plain.html")))

;; FIXME merge with 'plain' page?
(define (basic-page header extra-message body)
  (response-200
   (include-template "html/basic.html")))

;; given a list of xexprs, returns an error page etc. etc.
(define (error-page display-message)
  (include-template "html/error.html"))

;; this is a simple way of ensuring that certain templates
;; are included only in this context:
(define bogusbinding "")

;; given a URL specification, return a URL.
;; currently the identity.
;; FIXME disallow strings and move everything
;; over to custom Url-Path structure to prevent
;; XSS vulns
(define (urlgen url)
  (cond
    [(string? url)
     (string-append "\"" url "\"")]
    [(url-path? url)
     (string-append "\"" (url-path->url-string url) "\"")]))

;; FIXME this can go away when we no longer supply urls as
;; strings to the template....
;; is this a string that we could drop into html or js in
;; a safe way in a template?
(define (legal-url-string? s)
  (and (string? s)
       (regexp-match? #px"^[^\\\\\"]*$" s)))

(define ct-url? (or/c legal-url-string? url-path?))

(define (maybe-hidden-class hidden?)
  (if hidden? "hidden" ""))

;; given a boolean indicating whether a checkbox is
;; checked, return a string to be placed inside the
;; 'input' tag.
(define (checkbox-checked c)
  (if c "CHECKED" ""))

(define (safe-id s)
  (unless (ct-id? s)
    (raise-argument-error 'safe-id "simple alphanumeric id" 0 s))
  (xexpr->string s))

(define (ct-url-or-false s)
  (cond [(false? s) "false"]
        [(url-path? s)
         (js-str-format (url-path->url-string s))]
        ;; FIXME eliminate this whole clause when possible.
        [(js-str? s)
         (js-str-format s)]
        [else
         (raise-argument-error
          'ct-url-or-false
          "legal path"
          0 s)]))

;; wrap in single quotes. NOTE: only safe for strings
;; that don't contain single quotes, backslashes, "reverse solidus"es,
;; newlines, etc. This is guaranteed for us by ct-id?
(define (js-str-format s)
  (string-append
   "'"
   s
   ;; this is not necessary any more, because we're just fencing
   ;; out quotes and backslashes in ct-id.
   #;(regexp-replace* #px"'"
                    (regexp-replace* #px"\\\\" s "\\\\\\\\")
                    "\\\\'")
   "'"))

(define (false? s)
  (eq? s #f))



;; FIXME the set of safe filenames should *definitely* be checked
;; further upstream.
(define (filename? f)
  (and (string? f) (regexp-match? #px"^[-_a-zA-Z0-9\\.]+$" f)))

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
  (apply string-append
         (add-between (map xexpr->string xexprs) "\n")))

;; given a string, ensure that it doesn't contain
;; backslashes, single- or double-quotes
(define (js-str? s)
  (and (string? s) (regexp-match? #px"^[^\\\\\"']*$" s)))

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
          [authoring-page (-> ct-id? ct-url? (listof xexpr?)
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
   (-> js-str? ct-url? ct-url? ct-id? xexpr? (listof xexpr?) (listof xexpr?)
       (or/c ct-url? false?) response?)]))
(define (file-container-page default-mode save-url load-url assignment step path content file-url)
  (response-200
  (include-template "html/file-container.html")))

;; given values for the fields, construct the browse-file-container page
(provide
 (contract-out
  [browse-file-container-page
   (-> ct-id? xexpr? (listof xexpr?) js-str? (listof xexpr?)
       (or/c ct-url? false?) response?)]))
(define (browse-file-container-page assignment step path default-mode content file-url)
  ;; FIXME wrap with response-200
  (response-200
   (include-template "html/browse-file-container.html")))

;; given values for the fields, construct the feedback-file-container page
(provide
 (contract-out
  [feedback-file-container-page
   (-> ct-id? xexpr? (listof xexpr?) js-str? (listof xexpr?) ct-url?
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

  (define (response->str r)
    (call-with-output-string (response-output r)))
  
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
   (error-page (list "abc<i>wow</i>tag"))
   (regexp #px"&lt;i&gt;wow&lt;/i&gt;"))

  (check-match
   (error-page '((p "abc" (i "wow") "tag")) )
   (regexp #px"abc<i>wow</i>tag"))

  (check-match
   (response->str
    (plain-page "Quadra!" '((p "goofy"))))
   (regexp #px"Quadra!.*<p>goofy</p>"))

  (check-equal? (urlgen "abcd") "\"abcd\"")
  (check-equal? (urlgen
                 (strs->abs-ct-path/testing (list "waffle-house" "abc.txt")))
                "\"/waffle-house/abc.txt\"")
  
  (check-equal? (ct-url-or-false #f) "false")
  (check-equal? (ct-url-or-false "abc") "'abc'")
  ;; neither of these are legal any more
  #;(check-equal? (ct-url-or-false "abc'de'\\n\\" )
                "'abc\\'de\\'\\\\n\\\\'")
  #;(check-equal? (ct-url-or-false (strs->abs-ct-path/testing
                                  (list "big" "wig '\\dig")))
                "'/big/wig%20\\'%5Cdig'")

  (check-equal? (legal-url-string? "") #t)
  (check-equal? (legal-url-string? "acd.th") #t)
  (check-equal? (legal-url-string? "\"acd.th\"") #f)
  (check-equal? (legal-url-string? "ac/d.t/h") #t)
  (check-equal? (legal-url-string? "ac/d\\.t/h") #f)
  (check-equal? (legal-url-string? "ac/d.\"t/h") #f))