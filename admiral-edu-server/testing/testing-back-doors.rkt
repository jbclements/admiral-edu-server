#lang typed/racket/base

(require "../database/mysql/review.rkt"
         "../storage/storage.rkt"
         "./testing-shim.rkt"
         racket/match)

;; grotty back doors into the system to deal with
;; nondeterminism in the assignment of hashes

(require/typed "testing-shim.rkt"
               [class-name-shim (-> String)]
               [list-files-shim (-> Any (Listof String))])

(provide pending-review-hashes
         pending-review-hashes/reviewee
         completed-review-hashes
         feedback-hashes
         patch-path)

(: pending-review-hashes ((Pair String String) -> (Listof String)))
(define (pending-review-hashes key)
  (map Record-hash (pending-reviews key)))

(: pending-reviews ((Pair String String) -> (Listof Record)))
(define (pending-reviews key)
  (match-define (cons assignment uid) key)
  (select-pending assignment (class-name-shim) uid))

(: completed-review-hashes ((Pair String String) -> (Listof String)))
(define (completed-review-hashes key)
  (map Record-hash (completed-reviews key)))

(: completed-reviews ((Pair String String) -> (Listof Record)))
(define (completed-reviews key)
  (match-define (cons assignment uid) key)
  (select-completed assignment (class-name-shim) uid))

;; return the hashes of pending reviews where the reviewee is the given one
(: pending-review-hashes/reviewee ((Pair String String) String -> (Listof String)))
(define (pending-review-hashes/reviewee key reviewee)
  (map Record-hash
       (filter (λ ([review : Record])
                 (equal? (Record-reviewee-id review) reviewee))
               (pending-reviews key))))

(: feedback-hashes ((Pair String String) -> (Listof String)))
(define (feedback-hashes key)
  (map Record-hash (feedbacks key)))

(: feedbacks ((Pair String String) -> (Listof Record)))
(define (feedbacks key)
  (match-define (cons assignment uid) key)
  (select-feedback (class-name-shim) assignment uid))

;; given an assignment, a user, and the hash of a pending
;; review, return a list of the files associated with that
;; submission
(: hash->files ((Listof Record) String -> (Listof String)))
(define (hash->files records hash)
  (define the-record (findf (λ ([r : Record])
                              (equal? hash (Record-hash r)))
                            records))
  (cond [(eq? #f the-record)
         (error 'hash->files "no matching record for hash: ~e"
                hash)]
        [else
         (list-files-shim
          (submission-path (class-name-shim)
                           (Record-assignment-id the-record)
                           (Record-reviewee-id the-record)
                           ;; not sure about the "step" argument
                           "tests"))
         ]))

(define-type AUKey (Pair String String))

;; replace <HASHn> and <HASHVn> and <FILEn> and <FILEVn> elements with
;; hashes from the list of pendings or feedbacks respectively.
;; Do some nasty mutable-state stuff to prevent hashes from being used
;; inappropriately.
(: patch-path ((Listof String) AUKey -> (Listof String)))
(define (patch-path path key)
  (for/list ([p (in-list path)])
    (match p
      [(regexp #px"^<HASH([0-9]+)>$" (list _ nstr))
       (fetch-hash key review-index pending-review-hashes (strtonum nstr))]
      [(regexp #px"^<HASHV([0-9]+)>$" (list _ nstr))
       (fetch-hash key feedback-index feedback-hashes (strtonum nstr))]
      [(regexp #px"^<FILE([0-9]+)\\|([0-9]+)>$" (list _ nstr fnstr))
       (define hash (fetch-hash key review-index pending-review-hashes (strtonum nstr)))
       (get-nth-file (pending-reviews key) hash (strtonum fnstr))]
      [(regexp #px"^<FILEV([0-9]+)\\|([0-9]+)>$" (list _ nstr fnstr))
       (define hash (fetch-hash key feedback-index feedback-hashes (strtonum nstr)))
       (get-nth-file (feedbacks key) hash (strtonum fnstr))]
      [other other])))

;; I happen to know these are strings that match the regexp [0-9]+
(: strtonum (Any -> Natural))
(define (strtonum s)
  (cast (string->number (cast s String)) Natural))

;; return the name of the nth file associated with the AUKey and hash
(: get-nth-file ((Listof Record) String Natural -> String))
(define (get-nth-file records hash n)
  (define files (hash->files records hash))
  (unless (< n (length files))
    (error 'patch-path
           "expected nthfile (~e) to be less than length of list of files ~e"
           n files))
  (list-ref files n))

;; given a key and a map from keys to hashes and a natural number,
;; return an already-assigned hash if it exists, otherwise pick a
;; new one and add it to the map.
(: fetch-hash (AUKey (HashTable AUKey (HashTable Natural String))
                     (AUKey -> (Listof String)) Natural -> String))
(define (fetch-hash key map key->hashes n)
  (unless (hash-has-key? map key)
    (hash-set! map key
               (ann (make-hash)
                    (HashTable Natural String))))
  (define used-table (hash-ref map key))
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
         (define new-hash (car unused-hashes))
         (hash-set! used-table n new-hash)
         new-hash]))

(define-type UserIndex (HashTable AUKey
                            (HashTable Natural String)))

;; map from user x assignment to hashes from small integers to hashes
(: review-index UserIndex)
(define review-index (make-hash))

(: feedback-index UserIndex)
(define feedback-index (make-hash))

(module+ test
  (require typed/rackunit
           racket/match)

  (: test-table UserIndex)
  (define test-table (make-hash))
  
  (define (returner k)
    (match k
      [(cons "a" "b") '("abc" "def" "ghi")]
      [(cons "c" "d") '("123" "456" "789")]))

  (define test-key1 (cons "a" "b"))
  (define test-key2 (cons "c" "d"))
  (check-equal? (fetch-hash test-key1 test-table returner 2)
                "abc")
  (check-equal? (fetch-hash test-key1 test-table returner 1)
                "def")
  (check-equal? (fetch-hash test-key1 test-table returner 2)
                "abc")
  (check-equal? (fetch-hash test-key2 test-table returner 2)
                "123"))