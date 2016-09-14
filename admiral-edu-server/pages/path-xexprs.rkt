#lang typed/racket/base


(require "typed-xml.rkt"
         "../paths.rkt"
         net/uri-codec)

;; construct an 'a' element, performing uri-path-segment-encoding
;; on all hrefs
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
                 ;; FIXME drop this path
                 [(string? (cadr pr))
                  (path->ct-path (cadr pr))]))
         (list
          'href (url-path->url-string/encode url-path))]
        [else pr]))



(module+ test
  (require typed/rackunit)

  (define session
    (ct-session "test-class"
                "bob@example.com"
                #f
                (hash)))
  
  (check-expect (process-a-attr `(href ,(ct-url-path
                                         session "abc" "de%%f")))
                `(href "/abc/de%22%22f")))