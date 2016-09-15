#lang typed/racket/base

(require/typed xml
               [xexpr->string (XExpr -> String)])

(require racket/match
         "../../base.rkt"
         "../typed-xml.rkt"
         "../responses.rkt"
         "../errors.rkt"
         "../path-xexprs.rkt"
         "../../ct-session.rkt"
         "../../paths.rkt"
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
                 [open-xexpr : XExpr (cons 'ul (map (record->html session) open-assignments))]
                 [closed-xexpr : XExpr (cons 'ul (map (record->html session) closed-assignments))])
           (xexprs->response
            `((h1 "Assignments")
              ,(if message message "")
              (p () ,(cta `((href ,(ct-url-path session "author"))) "New Assignment"))
              (h2 "Open Assignments")
              ,open-xexpr
              (h2 "Closed Assignments")
              ,closed-xexpr)))]))

(: record->html (ct-session -> (assignment:Record -> XExpr)))
(define ((record->html session) record)
    (let*: ((id (assignment:Record-id (cast record assignment:Record)))
            [elem : XExpr (action:dashboard session id id)])
      `(li () ,elem)))
