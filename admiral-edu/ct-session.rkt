#lang typed/racket/base

(require racket/match
         "util/basic-types.rkt")

;; I'm starting to think that the word "session" is entirely misleading
;; here, as it appears that this session is not actually preserved
;; across requests.

;; it seems to be an abstraction of certain parts of a request including
;; url and bindings mostly.

;; Captain Teach Session information
;; FIXME use this class name rather than calling (class-name) all over the place.
(provide (struct-out ct-session))
(struct: ct-session ((class : String)
                     (uid : String)
                     (su-from-uid : (U False String))
                     (table : (HashTable Session-Var String)))
  #:transparent)

;; a session contains the name of a class, a uid, and
;; a table mapping variables to values:
(define-type Session-Var
  (U 'start-url 'sort-by 'order 'review-hash 'action 'user-id))
(define-predicate session-var? Session-Var)
;; start-url : the relative url of the current page, used to generate
;;   urls with a lot of 'up 'up 'up... I think this is unnecessary.
;; sort-by : the column on which to sort certain table displays
;; order : the ordering within that column
;; review-hash : ?
;; action : action (on the status page only?), one of
;;  - "mark-incomplete"
;;  - "mark-complete"
;;  - "publish"
;;  - "unpublish"
;;  - "unpublish-all"
;;  - "publish-all"
;; user-id : ?

;; actually, it seems like these should just be fields of the session.

(provide Order)
(define-type Order (U 'asc 'desc))

;; describes the ordering of elements in displayed sorted lists (ascending or descending)
; If the session has dir specified and it is 'asc or 'desc returns that dir
; otherwise returns 'asc.
;(provide get-dir)
(: get-order (ct-session -> Order))
(provide get-order)
(define (get-order session)
  (let ((table (ct-session-table session)))
    (cond [(not (hash-has-key? table 'order)) 'asc]
          [else (let ((dir-string (hash-ref table 'order)))
                  (match dir-string
                    ["asc" 'asc]
                    ["desc" 'desc]
                    [else 'asc]))])))

(: opposite-order (Order -> Order))
(provide opposite-order)
(define (opposite-order order)
  (match order
    ['asc 'desc]
    ['desc 'asc]))

(: clean-bindings ((Listof Any) -> (Listof (Pairof Symbol String))))
;; discard elements of the list that are not of the desired form
(define (clean-bindings ls)
  (match ls
    ['() '()]
    [(cons `(,symbol . ,string) tail)
     (cond [(and (symbol? symbol) (string? string)) (cons `(,symbol . ,string) (clean-bindings tail))]
           [else (clean-bindings tail)])]))

(provide get-binding)
(: get-binding (Session-Var ct-session -> (Result String)))
(define (get-binding binding session)
  (let ((table (ct-session-table session)))
    (cond [(not (hash-has-key? table binding))
           (Failure (format "No binding found: ~a" binding))]
          [else (Success (hash-ref table binding))])))

(provide make-table)
;; constructs a session table from a starting URL and a list of
;; bindings
(: make-table (String (Listof Any) -> (HashTable Session-Var String)))
(define (make-table start-rel-url bindings)
  (: pairs (Listof (Pairof Session-Var String)))
  (define pairs ((inst cons (Pairof Session-Var String) Nothing)
                 `(start-url . ,start-rel-url)
                 (filter (make-predicate (Pairof Session-Var String))
                         (clean-bindings bindings))))
  (make-hash pairs))