#lang typed/racket/base

(require (prefix-in action: "action.rkt")
         (prefix-in list: "list.rkt")
         "../typed-xml.rkt"
         "../responses.rkt"
         "../../base.rkt"
         "../../authoring/assignment.rkt"
         "../../paths.rkt")

(provide load)
(: load (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) Response))
(define (load session url message [post #f])
  (let ((assignment-id (car url)))
    (check-ready assignment-id)
    (let* ((assignment (assignment:select (class-name) assignment-id))
           (open (assignment:Record-open assignment))
           (ready (assignment:Record-ready assignment))
           (status (if ready (if open "Open" "Closed") "Missing Dependencies")))
           (xexprs->response
            `((h1 () ,(action:assignments session "Assignments"))
              (h2 () ,assignment-id)
              ,(if message message "")
              (h3 () ,(action:status session assignment-id "Status") " : " ,status)
              ,(if ready
                   (cond [open `(p () ,(action:close session assignment-id "Close Assignment"))]
                         [else `(p () ,(action:open session assignment-id "Open Assignment"))])
                   "")
              (p () ,(action:dependencies session assignment-id "Upload Dependencies"))
              (p () ,(action:edit session assignment-id "Edit Assignment Description"))
              (p () ,(action:export session assignment-id "Export Assignment Data"))
              (p () ,(action:delete session assignment-id "Delete Assignment")))))))

;; open an assignment
(provide open)
(: open (->* (ct-session (Listof String) (U XExpr #f)) (Boolean)
             Response))
(define (open session url message [post #f])
  (let ((assignment-id (car url)))
    (assignment:open assignment-id (class-name))
    (load session url message)))


(provide close)
(: close (->* (ct-session (Listof String) (U XExpr #f)) (Boolean)
              Response))
(define (close session url message [post #f])
  (let ((assignment-id (car url)))
    (assignment:close assignment-id (class-name))
    (load session url message)))

(provide delete)
(: delete (->* (ct-session (Listof String) (U XExpr #f)) (Boolean)
               Response))
(define (delete session url message [post #f])
  (let ((assignment-id (car url)))
    (cond [post (run-delete session url message assignment-id)]
          [else 
           (xexprs->response
            `((h1 () ,(action:assignments session "Assignments"))
              (h2 () ,(action:dashboard session assignment-id assignment-id))
              (p (b "You are about to delete this assignment. This action is irreversible. Click the submit button below to proceed."))
              (form ((action ,(url-path->url-string
                               (ct-url-path session "assignments" "delete" assignment-id)))
                     (method "POST"))
                    (input ((type "submit"))))))])))


(: run-delete (ct-session (Listof String) (U XExpr #f) String ->
                          Response))
(define (run-delete session url message assignment-id)
  (delete-assignment assignment-id)
  (list:load session url `(p () (b () ,(string-append "Assignment '" assignment-id "' deleted.")))))