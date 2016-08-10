#lang typed/racket/base

(require racket/path
         "../ct-session.rkt"
         "../util/basic-types.rkt"
         "../storage/storage.rkt"
         "responses.rkt"
         (prefix-in review: "../database/mysql/review.rkt"))

(provide do-download)


;; given a session, a review hash, and a nonempty file path,
;; return a response containing the file's bytes
;; FIXME call download.rkt instead.
;; perform a download
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

(: validate (review:Record ct-session -> Boolean))
(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewer (review:Record-reviewer-id review)))
    (equal? uid reviewer)))