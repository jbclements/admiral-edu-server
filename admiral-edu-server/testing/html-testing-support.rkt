#lang racket

(require html-parsing
         rackunit)

(provide no-italics
         anchor-link-equal?
         has-anchor-link
         has-anchor-link/bool
         has-anchor-links
         has-anchor-links/bool
         has-iframe-link
         ormap-xexp
         has-string
         no-double-encode)

(define (no-italics result)
  (check (compose not string-contains?) (sixth result) "<i>"))

;; given a link, return a predicate usable with ormap-xexp
(define (anchor-link-equal? l)
  (anchor-link/pred? (λ (link) (equal? link l))))

;; given a link, return a predicate usable with ormap-xexp
(define (anchor-link-matches? pat)
  (anchor-link/pred? (λ (link) (not (not (regexp-match pat link))))))

(define (anchor-link/pred? pred)
  (λ (e) (match e
           ;; NB fails for <a>'s with more than one href...
           [(list 'a (list '@ _1 ... (list 'href link) _2 ...) _3 ...)
            (pred link)]
           [other #f])))



;; does the result contain an <a> element with an href of
;; the form /test-class/[l] ? Performs a check.
(define ((has-anchor-link l) result)
  (check ormap-xexp
         (anchor-link-equal? l)
         (html->xexp (sixth result))))

;; does the result contain an <a> element with an href whose
;; link matches the given pattern
(define ((has-anchor-link/pat l) result)
  (check ormap-xexp
         (anchor-link-equal? l)
         (html->xexp (sixth result))))

;; does the result contain an <a> element with an href of
;; the form /test-class/[l] ?
(define ((has-anchor-link/bool l) result)
  (ormap-xexp
   (anchor-link-equal? l)
   (html->xexp (sixth result))))


;; andmap over has-anchor-link. performs checks
(define ((has-anchor-links ls) result)
  (for-each (λ (l) ((has-anchor-link l) result)) ls))

;; andmap over has-anchor-link. returns a boolean rather
;; than performing the check.
(define ((has-anchor-links/bool ls) result)
  (andmap (λ (l) ((has-anchor-link/bool l) result)) ls))

(define ((has-iframe-link l) result)
  (check ormap-xexp
         (λ (e) (match e
                  ;; NB fails for <iframe>'s with more than one href...
                  [(list 'iframe (list '@ _1 ... (list 'src link) _2 ...) _3 ...)
                   (equal? link l)]
                  [other #f]))
         (html->xexp (sixth result))))


  
;; does the xexp contain this element? (doesn't search attributes)
(define (ormap-xexp pred xexp)
  (or (pred xexp)
      (match xexp
        [(list tag (list '@ attr ...) sub-elts ...)
         (ormap (λ (xexp) (ormap-xexp pred xexp)) sub-elts)]
        [(list tag sub-elts ...)
         (ormap (λ (xexp) (ormap-xexp pred xexp)) sub-elts)]
        [other #f])))

  
(define ((has-string l) result)
  (check string-contains? (sixth result) l))

;; does this string contain &lt; ? used to check for
;; xexpr->string applied to html strings, likely signals
;; of incorrect translation to xexprs
(define (no-double-encode str)
  (not (string-contains? str "&lt;")))

(module+ test
  (define (equal-maker x) (λ (e) (equal? x e)))

  (check-equal? (ormap-xexp (equal-maker "def") '(a (@ (aoeu 3) (dch 4)) "abc" "def")) #t)
  (check-equal? (ormap-xexp (equal-maker "abc") '(a (@ (aoeu 3) (dch 4)) "abc" "def")) #t)
  (check-equal? (ormap-xexp (equal-maker "oth") '(a (@ (aoeu 3) (dch 4)) "abc" "def")) #f)
  (check-equal? (ormap-xexp (equal-maker '(a (@ (aoeu 3) (dch 4)) "abc" "def")) '(a (@ (aoeu 3) (dch 4)) "abc" "def")) #t)
  (check-equal? (ormap-xexp (equal-maker '(@ (aoeu 3) (dch 4))) '(a (@ (aoeu 3) (dch 4)) "abc" "def")) #f)
  (check-equal? (ormap-xexp (equal-maker "abc") '(b (a (@ (aoeu 3) (dch 4)) "abc" "def"))) #t)
  (check-equal? (ormap-xexp (λ (elt)
                              (match elt
                                [(list 'a (list '@ _1 ... (list 'dch 4) _2 ...) _3 ...)
                                 #t]
                                [other #f]))
                            '(b (a (@ (aoeu 3) (dch 4)) "abc" "def"))) #t)

  (check-true ((anchor-link-matches? #px"zz")
               '(a (@ (z 4) (href "chrzz.,ht")) "ont.h")))
  
  (check-true ((anchor-link/pred? (λ (l)
                                    (not
                                     (not
                                      (regexp-match #px"zz" l)))))
               '(a (@ (z 4) (href "chrzz.,ht")) "ont.h")))

  (check-false ((anchor-link/pred? (λ (l) (regexp-match #px"zz" l)))
                '(a (@ (z 4) (href "chrz.,ht")) "ont.h"))))

  
