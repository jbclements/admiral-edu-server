#lang typed/racket/base

(require racket/match
         "../../configuration.rkt"
         "../typed-xml.rkt"
         "../../ct-session.rkt"
         "../../paths.rkt"
         "../path-xexprs.rkt")

;; looks like this file provides wrappers for quick construction
;; of '<a>' elements.

;; to see an example of what the 'link-maker' macro does, take
;; a look at the test case below.

;; given a session, a list of path elements, and a body, return
;; an <a> xexpr containing the given body with an href of
;; the corresponding path elements
(: build-xexpr-anchor (ct-session (U (Pair 'noslash (Listof String)) (Listof String)) XExpr -> XExpr))
(define (build-xexpr-anchor session path-elts-spec body-xexpr)
  (define url-path
    (match path-elts-spec
      [(list 'noslash path-elts ...)
       (apply ct-url-path session path-elts)]
      [(list path-elts ...)
       (apply ct-url-path-/ session (cast path-elts (Listof String)))]))
  (cta `((href ,url-path))
       body-xexpr))

;; a macro to shorten the three-step process of specifying a link-maker
(define-syntax link-maker
  (syntax-rules ()
    [(_ name (argname ...) taglistmaker)
     (begin
       (provide name)
       (define (name [session : ct-session] [argname : String] ... [body-xexpr : XExpr]) : XExpr
         (build-xexpr-anchor session taglistmaker body-xexpr)))]))


;; Links to assignments page
(link-maker assignments [] (list "assignments"))

;; FIXME do all of these really need to be exported?
(provide OPEN)
(define OPEN "open")

(link-maker open [assignment-id] (list "assignments" OPEN assignment-id))

(provide CLOSE)
(define CLOSE "close")

(link-maker close [assignment-id] (list "assignments" CLOSE assignment-id))

(provide DELETE)
(define DELETE "delete")

(link-maker delete [assignment-id] (list "assignments" DELETE assignment-id))

(provide LIST)
(define LIST "")

(provide DASHBOARD)
(define DASHBOARD "dashboard")

(link-maker dashboard [assignment-id] (list "assignments" DASHBOARD assignment-id))

(link-maker dependencies [assignment-id] (list "dependencies" assignment-id))
(link-maker edit [assignment-id] (list "author" "edit" assignment-id))
(link-maker export [assignment-id] (list 'noslash "export" assignment-id (string-append assignment-id ".zip")))

(provide STATUS)
(define STATUS "status")

(link-maker status [assignment-id] (list "assignments" "status" assignment-id))
(link-maker step-status [assignment-id step-id]
            (list "assignments"  "status" assignment-id step-id))
(link-maker review-status [assignment-id step-id review-id]
            (list "assignments" "status" assignment-id step-id review-id))

(module+ test
  (require typed/rackunit
           "../../ct-session.rkt")

  (define test-session (ct-session "test-class" "bob@example.com" #f (hash)))
  (check-equal? (dashboard test-session "test-assignment%" `(p "yay"))
                `(a ((href "/test-class/assignments/dashboard/test-assignment%25/"))
                    (p "yay"))))