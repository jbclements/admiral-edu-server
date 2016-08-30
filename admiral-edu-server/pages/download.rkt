#lang typed/racket/base

(require racket/path
         "../ct-session.rkt"
         "../util/basic-types.rkt"
         "../storage/storage.rkt"
         "responses.rkt"
         "../paths.rkt"
         (prefix-in review: "../database/mysql/review.rkt"))

(provide do-download
         do-browse-download)


;; given a session, a review hash, and a nonempty file path,
;; validate hash and permissions and
;; return a response containing the file's bytes
(: do-download (ct-session String (Pairof String
                                             (Listof String))
                              -> Response))
(define (do-download session r-hash path-strs)
  (define maybe-review (review:maybe-select-by-hash r-hash))
  (define review
    (cond [maybe-review maybe-review]
          [else (raise-403-not-authorized)]))
  (when (not (validate review session))
    (raise-403-not-authorized))
  (define class (ct-session-class session))
  (define assignment (review:Record-assignment-id review))
  (define stepName (review:Record-step-id review))
  (define reviewee (review:Record-reviewee-id review))
  ;; FIXME url handling
  (define path-str
    (some-system-path->string (apply build-path/convention-type
                                     'unix
                                     path-strs)))
  (define data (maybe-get-file-bytes class assignment stepName
                                     reviewee path-str))
  ;; FIXME isn't this an internal error?
  (unless data
    (raise-403-not-authorized "You are not authorized to see this page."))
  (bytes->file-response data))

;; given a session, an assignment and step, and a nonempty file path,
;; validate hash and permissions and
;; return a response containing the file's bytes
;; no auth check because the uid is part of the file's path.
(: do-browse-download (ct-session String String
                                  (Pairof String
                                             (Listof String))
                              -> Response))
(define (do-browse-download session assignment step path-strs)
  (define class (ct-session-class session))
  (define user (ct-session-uid session))
  (define data (maybe-get-file-bytes class assignment step user
                                     (apply rel-ct-path path-strs)))
  (unless data
    (raise-403-not-authorized "You are not authorized to see this page."))
  (bytes->file-response data))

(: validate (review:Record ct-session -> Boolean))
(define (validate review session)
  (define uid (ct-session-uid session))
  (define reviewer (review:Record-reviewer-id review))
  (define reviewee (review:Record-reviewee-id review))
  (or (equal? uid reviewer)
      (equal? uid reviewee)))