#lang racket/base

(require racket/string
         racket/list
         racket/match
         racket/contract
         web-server/http/bindings
         web-server/templates
         xml
         json
         yaml)

;; FIXME looks like it could easily be converted to typed racket...

(require "../storage/storage.rkt"
         "../base.rkt"
         "../authoring/assignment.rkt"
         "templates.rkt"
         "responses.rkt"
         "errors.rkt")

(define NEW-ACTION "new")
(define EDIT-ACTION "edit")
(define VALIDATE-ACTION "validate")
(define VALIDATE-AND-SAVE-ACTION "validate-save")

(define warning-message
  '((p (b "Warning:" ) " You are editing an existing assignment."
       "In general it is safe to change instructions and add steps."
       "However, if students have started this assignment, changing ids"
       "and rubric structures may cause inconsistencies in the exported"
       "assignment data.")))

(provide (contract-out
          [load (->* (ct-session? any/c any/c) (any/c) response?)]))

;; allow user to create or edit an assignment. returns response
(define (load session role rest [message '()])
  (when (not (roles:Record-can-edit role))
    (raise-403-not-authorized))
  ;; FIXME ad-hoc url parsing
  (define len (length rest))
  (define action (if (= 0 len) NEW-ACTION (car rest)))
  (cond [(equal? NEW-ACTION action)
         (authoring-page (class-name) "'validate'" '() '())]
        [(equal? EDIT-ACTION action) (edit (cdr rest) warning-message)]
        [else (raise-400-bad-request)]))

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
  (authoring-page (class-name) "'validate-save'" contents message))


;; ensure the assignment is valid, add or overwrite, indicate
;; success.
(provide validate)
(define (validate session post-data create?)
  (match (yaml-bytes->create-or-save-assignment post-data create?)
    [(Success _) (xexprs->response '("Success"))]
    [(Failure msg) (error-xexprs->400-response (list msg))]))

  
