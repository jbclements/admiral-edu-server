#lang typed/racket/base

(require/typed xml
               [xexpr->string (XExpr -> String)])

(require racket/match
         "../../base.rkt"
         "../typed-xml.rkt"
         "../responses.rkt"
         "../errors.rkt"
         (prefix-in action: "action.rkt"))

(provide load)
(: load (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) Response))
(define (load session url message [post #f])
      (match (assignment:list (class-name))
        ['no-such-class
         ;; is this an internal error?
         (error-xexprs->400-response '((h2 "No such class found.")))]
        [records
         (let*: ((assign-list (cast records (Listof assignment:Record)))
                 (open-assignments (filter assignment:Record-open assign-list))
                 (closed-assignments (filter (lambda: ([x : assignment:Record]) (not (assignment:Record-open x))) assign-list))
                 [open-xexpr : XExpr (cons 'ul (map record->html open-assignments))]
                 [closed-xexpr : XExpr (cons 'ul (map record->html closed-assignments))])
           (xexprs->response
            `((h1 "Assignments")
              ,(if message message "")
              (p () (a ((href ,(string-append "/" (class-name) "/author/"))) "New Assignment"))
              (h2 "Open Assignments")
              ,open-xexpr
              (h2 "Closed Assignments")
              ,closed-xexpr)))]))

(: record->html (assignment:Record -> XExpr))
(define (record->html record)
    (let*: ((id (assignment:Record-id (cast record assignment:Record)))
            [elem : XExpr (action:dashboard id id)])
      `(li () ,elem)))
