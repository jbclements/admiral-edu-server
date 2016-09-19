#lang typed/racket/base

(require racket/list
         "../../base.rkt"
         "../path-xexprs.rkt"
         "../../paths.rkt"
         "../typed-xml.rkt"
         "../templates.rkt"
         "../responses.rkt")

(require/typed "../templates.rkt"
               [plain-page (String (Listof XExpr) -> Response)])

; ct-session -> (listof string) -> (#f xexpr?) -> bool? -> (listof xexpr? void?)
(provide load)
(: load (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) Response))
(define (load session url message [post #f])
  (plain-page
   "Assignments"
   `((h1 "Assignments")
     ,(if message message "")
     (h2 "Open Assignments")
     (ul ,@(list-open-assignments session))
     (h2 "Closed Assignments")
     (ul ,@(list-closed-assignments session)))))


; string-url -> (listof xexpr? void?)
(: list-open-assignments (ct-session -> (Listof XExpr)))
(define (list-open-assignments session)
  (let*: ([assignments : (U 'no-such-class (Listof assignment:Record)) (assignment:list (class-name))]
          (open-assignments (filter assignment:Record-open (cast assignments (Listof assignment:Record)))))
    (cond [(empty? open-assignments) '((p "There are currently no open assignments."))]
          [else (map (open-assignment-element session) open-assignments)])))


; string-url -> (listof xexpr? void?)
(: open-assignment-element (ct-session -> (assignment:Record -> XExpr)))
(define (open-assignment-element session)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
      `(li ,(cta `((href ,(ct-url-path session "feedback" assignment-id)))
                    assignment-id)))))



; ct-session -> (listof xexpr? void?)
(: list-closed-assignments (ct-session -> (Listof XExpr)))
(define (list-closed-assignments session)
    (let*: ((uid (ct-session-uid session))
            [assignment-list : (U 'no-such-class (Listof assignment:Record)) (assignment:list (class-name))]
            (closed-assignments (filter (show-closed? uid) (cast assignment-list (Listof assignment:Record)))))
      (cond [(empty? closed-assignments) '((p "There are currently no closed assignments."))]
            [else (map (closed-assignment-element session) closed-assignments)])))



(: show-closed? (String -> (assignment:Record -> Boolean)))
(define (show-closed? uid)
  (lambda (record)
    (let* ((assignment-id (assignment:Record-id record))
           (closed (not (assignment:Record-open record)))
           (has-submitted (submission:has-submitted? assignment-id (class-name) uid)))
      (and closed has-submitted))))


(: closed-assignment-element (ct-session -> (assignment:Record -> XExpr)))
(define (closed-assignment-element session)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
      `(li ,(cta `((href ,(ct-url-path session "assignments" "feedback" assignment-id)))
                 assignment-id)))))
