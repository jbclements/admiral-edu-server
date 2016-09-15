#lang typed/racket/base


(require "typed-xml.rkt"
         "../paths.rkt"
         net/uri-codec)

;; construct an 'a' element, performing uri-path-segment-encoding
;; on all hrefs
;; NOTE: this name is maybe too short, but it has to replace 'a'
;; in xexpr trees without feeling too heavy.
(provide cta)
(: cta ((Listof (List Symbol (U String Ct-Path))) XExpr * -> XExpr))
(define (cta attrs . elts)
  `(a ,(map process-a-attr attrs) ,@elts))

;; if the attribute is an 'href', perform uri-path-segment-encoding
;; on each element of the path. Otherwise leave it alone
(: process-a-attr ((List Symbol (U String Ct-Path))
                   -> (List Symbol String)))
(define (process-a-attr pr)
  (cond [(eq? (car pr) 'href)
         (define url-path
           (cond [(Ct-Path? (cadr pr)) (cadr pr)]
                 ;; FIXME you *can't* really do this job right;
                 ;; it's too late, especially if you want to
                 ;; allow e.g. strings beginning with http: or
                 ;; strings with query fragments.
                 ;; let's see how broken it is without it:
                 [(string? (cadr pr))
                  (error 'cta "not a ct-path: ~e\n" (cadr pr))
                  #;(abs-path->ct-path (cadr pr))]))
         (list
          'href (url-path->url-string url-path))]
        [else
         (cond [(string? (cadr pr)) pr]
               [else (raise-argument-error
                      'process-a-attr
                      "attribute with ct-paths ocurring only in hrefs"
                      0 pr)])]))



(module+ test
  (require typed/rackunit
           "../ct-session.rkt")

  (define session
    (ct-session "test-class"
                "bob@example.com"
                #f
                (hash)))
  
  (check-equal? (process-a-attr `(href ,(ct-url-path
                                         session "abc" "de%%f")))
                `(href "/test-class/abc/de%25%25f"))

  #;(check-equal? (process-a-attr `(href "/abc/de%%f"))
                `(href "/abc/de%25%25f"))

  (check-equal? (process-a-attr `(foo "/abc/de%%f"))
                `(foo "/abc/de%%f")))