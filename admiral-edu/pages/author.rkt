#lang racket/base

(require racket/match
         racket/contract
         "../storage/storage.rkt"
         "../base.rkt"
         "../authoring/assignment.rkt"
         "templates.rkt"
         "responses.rkt"
         "errors.rkt"
         (only-in xml xexpr?))

;; FIXME looks like it could easily be converted to typed racket...

(require )

(define NEW-ACTION "new")
(define EDIT-ACTION "edit")

(define warning-message
  '((p (b "Warning:" ) " You are editing an existing assignment."
       "In general it is safe to change instructions and add steps."
       "However, if students have started this assignment, changing ids"
       "and rubric structures may cause inconsistencies in the exported"
       "assignment data.")))

(provide (contract-out
          [load (->* (ct-session? any/c any/c) ((listof xexpr?)) response?)]
          [validate (-> ct-session? bytes? boolean? response?)]))

;; allow user to create or edit an assignment. returns response
(define (load session role rest [message '()])
  (when (not (roles:Record-can-edit role))
    (raise-403-not-authorized))
  (match rest
    [(or (list)
         (list-rest "new" _))
     (authoring-page (class-name) "validate" '() '())]
    [(list-rest "edit" tail)
     (edit tail warning-message)]
    [other (raise-404-not-found)]))

;; returns response
(define (edit rest [message '()])
  (when (< (length rest) 1)
    (raise-404-not-found "Invalid URL. Expected /author/edit/assignment-id/"))
  (define assignment-id (car rest))
  (when (not (assignment:exists? assignment-id (class-name)))
    (raise-404-not-found (string-append "No such assignment: " assignment-id)))
  (define contents (list
                    (retrieve-assignment-description
                     (class-name) assignment-id)))
  (authoring-page (class-name) "validate-save" contents message))


;; ensure the assignment is valid, add or overwrite, indicate
;; success.
(define (validate session post-data create?)
  (with-handlers
      ([exn:user-error?
        (λ (exn)
          (plain-text-response (string-append "Fail: " (exn-message exn))))])
    (match (yaml-bytes->create-or-save-assignment post-data create?)
      [(Success _) (plain-text-response "Success")]
      [(Failure msg) (plain-text-response (string-append "Fail: " msg))])))

  
