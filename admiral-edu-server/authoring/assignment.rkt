#lang typed/racket/base

(require/typed json
               [jsexpr->string (Any -> String)])

(require racket/string
         racket/match
         racket/list
         "typed-yaml.rkt"
         "../storage/storage.rkt"
         "assignment-structs.rkt"
         "assignment-parser.rkt"
         "../base.rkt")

(provide (all-from-out "assignment-structs.rkt")
         delete-assignment
         yaml-bytes->create-or-save-assignment
         next-step
         submit-step
         assignment-id->assignment-dependencies
         handle-dependency
         find-dependencies
         check-ready
         assignment-id->assignment
         create-or-save-assignment)

;; represents success for functions that don't return a useful value
(define success (Success (void)))

;; assignment-id -> void
;; Completely remove all trace of an assignment
(: delete-assignment (String -> Void))
(define (delete-assignment assignment-id)
  (database:delete-assignment (class-name) assignment-id)
  (delete-path (string-append (class-name) "/" assignment-id)))


;; return the (alphabetically) first duplicated result of the getter
(: repeat-id? (All (A) ((A -> String) -> ((Listof A) -> (U String #f)))))
(define (repeat-id? getter)
  (lambda (list)
    (check-duplicates (sort (map getter list) string<?))))

;; dead code?
(: validate-step (Step -> (Result Void)))
(define (validate-step step)
  (sdo (validate-id (Step-id step))
       (let* ((reviews (Step-reviews step))
              (ids (map Review-id reviews))
              (sorted (sort ids string<?)))
         (let ([repeat (check-duplicates sorted)]
               [invalid-id-messages
                (map Failure-message (filter Failure? (map validate-id ids)))])
           (cond [repeat
                  (Failure
                   (string-append "Each review id must be unique for the step "
                                  "it is in. Found duplicate review id '"
                                  repeat "' in the step '" (Step-id step) "'."))]
                 [(not (null? invalid-id-messages))
                  (Failure (string-join invalid-id-messages "\n"))]
                 [else success])))))

(: validate-id (String -> (Result Void)))
(define (validate-id id)
  (cond
    [(regexp-match-exact? #px"[a-zA-Z0-9-]*" id) success]
    [else
     (Failure
      (string-append
       "Id can only contain letters, numbers, and '-' characters. Rejected '" id "'."))]))

;; given an assignment and a boolean indicating whether this one should replace
;; another one with the same name, return a (Result Void)
(: validate-assignment (Assignment Boolean -> (Result Void)))
(define (validate-assignment assignment override)
  (cond [(and (not override) (assignment:exists? (Assignment-id assignment) (class-name)))
         (Failure
          (string-append "The specified assignment id '"
                         (Assignment-id assignment)
                         "' already exists."))]
        [else
         (define check-steps ((repeat-id? Step-id) (Assignment-steps assignment)))
         (let* ([check-review-ids (filter string? 
                                          (map (repeat-id? Review-id) 
                                               (map Step-reviews (Assignment-steps assignment))))]
                (ids (append (map Step-id (Assignment-steps assignment))
                             (apply append
                                    (map (λ ([step : Step])
                                           (map Review-id (Step-reviews step)))
                                         (Assignment-steps assignment)))))
                [valid-step-ids (filter string? (map validate-id ids))])
           (sdo (validate-id (Assignment-id assignment))
                (cond
                  [(not (null? check-review-ids))
                   (Failure
                    (string-append "Found duplicate review-ids: " (string-join check-review-ids ", ")))]
                  [check-steps
                   (Failure
                    (string-append "Assignment may not have multiple steps with the same id. Found multiple instances of '"
                                   check-steps "'"))]
                  [(not (null? valid-step-ids))
                   (Failure
                    (string-join valid-step-ids ""))]
                  [else (Success (void))])))]))

;; given post bytes representing YAML for a new assignment
;; and whether this is a create or save (overwrite),
;; parse and save the assignment.
(: yaml-bytes->create-or-save-assignment (Bytes Boolean -> (Result Void)))
(define (yaml-bytes->create-or-save-assignment bytes create?)
  (let ((yaml-string (bytes->string/utf-8 bytes)))
    (define yaml (with-handlers
                     ([exn:fail? raise-could-not-parse])
                   (string->assignment-yaml yaml-string)))
    (define assignment
      (with-handlers ([exn:fail? raise-invalid-yaml])
        (yaml->assignment yaml)))
    (sdo (create-or-save-assignment assignment create?)
         (Success (save-assignment-description (class-name) (Assignment-id assignment) yaml-string))
         (Success (void)))))


;; TODO: check to see if assignment has the same name as before
(: create-or-save-assignment (Assignment Boolean -> (Result Void)))
(define (create-or-save-assignment assignment create?)
  (sdo (validate-assignment assignment (not create?))
       (begin
         (when create? (create-database-entries assignment))
         (create-base-rubrics assignment)
         (check-no-reviews assignment)
         (Success (void)))))


(: check-no-reviews (Assignment -> Void))
(define (check-no-reviews assignment)
  (let ((no-reviews (null? (filter no-reviews? (Assignment-steps assignment))))
        (assignment-id (Assignment-id assignment)))
    (if no-reviews (assignment:mark-ready assignment-id (class-name))
        (assignment:mark-not-ready assignment-id (class-name)))))


(: no-reviews? (Step -> Boolean))
(define (no-reviews? step)
  (not (null? (Step-reviews step))))
    
(: create-database-entries (Assignment -> (U #t 'no-such-class 'duplicate-assignment)))
(define (create-database-entries assignment)
  (let ((id (Assignment-id assignment)))
    (assignment:create id (class-name))))


(: create-base-rubrics (Assignment -> Void))
(define (create-base-rubrics assignment)
  (let* ((class (class-name))
         (assign (Assignment-id assignment))
         (steps (Assignment-steps assignment))
         (create (lambda ([step : Step]) 
                   (let* ((stepName (Step-id step))
                          (reviews (Step-reviews step))
                          (create (lambda: ([review : Review])
                                    (let ((review-id (Review-id review))
                                          (rubric (jsexpr->string (rubric->json (Review-rubric review)))))
                                      (create-default-rubric class assign stepName rubric review-id)))))
                     (map create reviews)))))
    (map create steps)
    (void)))


(: next-step (String String -> (U MustReviewNext MustSubmitNext #t)))
(define (next-step assignment-id uid)
  (let* ((assignment (yaml->assignment (string->assignment-yaml (retrieve-assignment-description (class-name) assignment-id))))
         (handler (Assignment-assignment-handler assignment))
         (next-action (AssignmentHandler-next-action handler)))
    (next-action assignment (Assignment-steps assignment) uid)))


;; FIXME does two different things? Confusing.
;; Attempts to publish for the specified uid, assignment, and step-id. If this is not the next expected action,
;; This returns a failure with a message describing what the user should do next.
;; If file-name and data are supplied and not #f, it is uploaded as a submission before creating a database record
(: submit-step (->* (String String String) ((U String #f) (U Bytes #f)) (Result String)))
(define (submit-step assignment-id step-id uid [file-name #f] [data #f])
  ;; Assignment must exist
  (cond 
    [(not (assignment:exists? assignment-id (class-name))) (failure "The specified assignment '" assignment-id "' does not exist.")]
    [else (let* ((assignment (yaml->assignment (string->assignment-yaml (retrieve-assignment-description (class-name) assignment-id))))
                 (steps (Assignment-steps assignment))
                 (handler (Assignment-assignment-handler assignment))
                 (next-action (AssignmentHandler-next-action handler)) 
                 (next (next-action assignment steps uid))
                 (do-submit-step (AssignmentHandler-do-submit-step handler))) 
            (cond
              [(and (MustSubmitNext? next) 
                    (equal? (Step-id (MustSubmitNext-step next)) step-id))
               (do-submit-step assignment (step-id->step assignment-id step-id) uid file-name data steps)]
              [else (failure "Could not submit to the step '" step-id "'." (next-action-error next))]))]))


(: next-action-error ((U MustSubmitNext MustReviewNext #t) -> String))
(define (next-action-error next)
  (cond [(MustSubmitNext? next)
         (string-append "Your next action is to submit to on '"
                        (Step-id (MustSubmitNext-step next)) "'.")]
        [(MustReviewNext? next)
         (string-append "Your next action is to complete reviews for '"
                        (Step-id (MustReviewNext-step next)) "'.")]
        [(eq? #t next) "You have completed this assignment."]
        [else ""]))


(: assignment-id->assignment-dependencies (String -> (Listof Dependency)))
(define (assignment-id->assignment-dependencies id)
  (let* ((assignment (assignment-id->assignment id))
         (handler (Assignment-assignment-handler assignment))
         (get-deps-f (AssignmentHandler-get-dependencies handler)))
    (get-deps-f assignment)))


(: handle-dependency (String Dependency (Listof (Pairof Symbol (U String Bytes))) (Listof Any) -> (Result String)))
(define (handle-dependency assignment-id dependency bindings raw-bindings)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (handler (Assignment-assignment-handler assignment))
         (take-dependency (AssignmentHandler-take-dependency handler)))
    (take-dependency assignment-id dependency bindings raw-bindings)))
         

(: find-dependencies (String String String -> (Listof Dependency)))
(define (find-dependencies assignment-id step-id review-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (f (lambda (dep) (and (review-dependency? dep) 
                              (equal? (review-dependency-step-id dep) step-id) 
                              (equal? (review-dependency-review-id dep) review-id)))))
    (filter f deps)))


(: check-ready (String -> (U Void #f)))
(define (check-ready assignment-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (filter-function (lambda ([dep : Dependency]) (not (dependency-met dep)))))
    (if (null? (filter filter-function deps)) (assignment:mark-ready assignment-id (class-name)) #f)))


(: assignment-id->assignment (String -> Assignment))
(define (assignment-id->assignment id)
  (yaml->assignment (string->assignment-yaml (retrieve-assignment-description (class-name) id))))


(: step-id->step (String String -> Step))
(define (step-id->step assignment-id step-id)
  (lookup-step (assignment-id->assignment assignment-id) step-id))

(: lookup-step (Assignment String -> Step))
(define (lookup-step assignment step-id)
  (let* ((steps (Assignment-steps assignment))
         (filter-f (lambda ([step : Step]) (equal? step-id (Step-id step))))
         (result (filter filter-f steps)))
    (car result)))


;; this is really a hack; this cast fail could happen much later. The
;; problem is that since there's no ImmutableHashTable type, you can't
;; formulate this as a flat predicate.
(: string->assignment-yaml (String -> Assignment-YAML))
(define (string->assignment-yaml s)
  (cast (string->yaml s) Assignment-YAML))