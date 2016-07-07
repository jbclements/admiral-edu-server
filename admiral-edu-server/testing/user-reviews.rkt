#lang typed/racket/base

(require "../database/mysql/review.rkt"
         "../storage/storage.rkt"
         racket/match)

(require/typed "testing-shim.rkt"
               [class-name-shim (-> String)])

(provide pending-review-hashes
         patch-path)

(: pending-review-hashes (String String -> (Listof String)))
(define (pending-review-hashes assignment uid)
  (map Record-hash
       (select-pending assignment (class-name-shim) uid)))

;; given an assignment, a user, and the hash of a pending
;; review, return a list of the files associated with that
;; submission
(: hash->files ((Pair String String) String -> (Listof String)))
(define (hash->files key hash)
  (define assignment (car key))
  (define user (cdr key))
  (define records (select-pending assignment (class-name-shim) user))
  (define the-record (findf (λ ([r : Record])
                              (equal? hash (Record-hash r)))
                            records))
  (cond [(eq? #f the-record)
         (error 'hash->files "no matching record for assignment/user/hash: ~e"
                (list assignment user hash))]
        [else
         (list-files
          (submission-path (class-name-shim)
                           assignment
                           (Record-reviewee-id the-record)
                           ;; not sure about the "step" argument
                           "tests"))
         ]))

;; helper function to pull key pieces apart
(: pending-review-hashes/key ((Pair String String) -> (Listof String)))
(define (pending-review-hashes/key key)
  (pending-review-hashes (car key) (cdr key)))

(define-type UAKey (Pair String String))

;; replace <HASHn> elements with hashes from the list of pendings
;; do some nasty mutable-state stuff to prevent hashes from being used
;; inappropriately.
(: patch-path ((Listof String) UAKey -> (Listof String)))
(define (patch-path path key)
  (for/list ([p (in-list path)])
    (match p
      [(regexp #px"^<HASH([0-9]+)>$" (list _ nstr))
       (fetch-hash key pending-review-hashes/key
                   (cast (string->number (cast nstr String))
                         Natural))]
      [(regexp #px"^<FILE([0-9]+)\\|([0-9]+)>$" (list _ nstr fnstr))
       (define hash (fetch-hash key pending-review-hashes/key
                                (cast (string->number (cast nstr String))
                                      Natural)))
       (define files (hash->files key hash))
       (define nth-file (cast (string->number (cast fnstr String))
                              Natural))
       (unless (< nth-file (length files))
         (error 'patch-path
                "expected nthfile (~e) to be less than length of list of files ~e"
                nth-file files))
       (list-ref files nth-file)]
      [other other])))

;; given a key and a map from keys to hashes and a natural number,
;; return an already-assigned hash if it exists, otherwise pick a
;; new one and add it to the map.
(: fetch-hash (UAKey (UAKey -> (Listof String)) Natural -> String))
(define (fetch-hash key key->hashes n)
  (unless (hash-has-key? used-hash-map key)
    (hash-set! used-hash-map key
               (ann (make-hash)
                    (HashTable Natural String))))
  (define used-table (hash-ref used-hash-map key))
  (cond [(hash-has-key? used-table n)
         (hash-ref used-table n)]
        [else
         (define unused-hashes (remove*
                                (hash-values used-table)
                                (key->hashes key)))
         (when (null? unused-hashes)
           (error 'patch-path
                  "no-unused-hashes for user/assignment pair: ~e"
                  key))
         (define new-hash (car
                           unused-hashes))
         (hash-set! used-table n new-hash)
         new-hash]))

;; map from user x assignment to hashes from small integers to hashes
(: used-hash-map (HashTable (Pair String String)
                            (HashTable Natural String)))
(define used-hash-map (make-hash))

(module+ test
  (require typed/rackunit
           racket/match)
  
  (define (returner k)
    (match k
      [(cons "a" "b") '("abc" "def" "ghi")]
      [(cons "c" "d") '("123" "456" "789")]))

  (define test-key1 (cons "a" "b"))
  (define test-key2 (cons "c" "d"))
  (check-equal? (fetch-hash test-key1 returner 2)
                "abc")
  (check-equal? (fetch-hash test-key1 returner 1)
                "def")
  (check-equal? (fetch-hash test-key1 returner 2)
                "abc")
  (check-equal? (fetch-hash test-key2 returner 2)
                "123"))