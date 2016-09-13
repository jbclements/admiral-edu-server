#lang racket/base

(require racket/string
         racket/contract
         racket/match
         web-server/templates
         web-server/http/bindings
         (only-in web-server/http/response-structs response?)
         (only-in xml xexpr?))

(require "../base.rkt"
         "../util/roster.rkt"
         "../paths.rkt"
         "templates.rkt"
         (prefix-in error: "errors.rkt")
         (prefix-in authorized: "../util/authorized.rkt"))
         
(define DISPLAY-ROSTER "render-roster")
(define NEW-STUDENT "new-student")
(define UPLOAD-ROSTER "upload-roster")
(define EDIT-USER "edit")
(define CREATE-STUDENT "create-student")
(define PROCESS-ROSTER "process-roster")
(define ILLEGAL-ACTION "illegal-action")

;; show the roster, allow an instructor to edit it
(provide
 (contract-out [load (->* (ct-session? any/c (listof string?))
                          ((listof xexpr?))
                          response?)]))
(define (load session role rest [message '()])
  (authorized:check-can-edit session)
  (do-load session rest message))

;; add a user or users to the roster (?)
(provide post)
(define (post post-data bindings session role rest [message '()])
  (authorized:check-can-edit session)
  (post->do-load post-data bindings session rest message))

;; update the roster, then show the /roster page again
(define (post->do-load post-data bindings session rest message)
  (unless (exists-binding? 'action bindings)
    (raise-400-bad-request "The action you took could not be processed."))
  (define action (extract-binding/single 'action bindings))
  (define message (do-post action post-data bindings))
  ;; yikes, interesting pun... goes to the matching load endpoint after
  ;; finishing action:
  (do-load session rest message))

;; add the student or roster, return a list of xexprs
(define (do-post action post-data bindings)
  (cond [(equal? action CREATE-STUDENT) (post->create-student bindings)]
        [(equal? action PROCESS-ROSTER) (post->process-roster bindings)]
        ;;> FIXME should presumably be a 404
        [else '()]))

;; returns list of xexprs
(define (post->create-student bindings)
  ;; FIXME check much earlier?
  (cond [(not (exists-binding? 'uid bindings)) '((p "Missing User ID.")) ]
        ;; FIXME use raw bindings
        [else (let* ((uid (extract-binding/single 'uid bindings))
                     (result (register-uid uid)))
                (cond [(Failure? result) `((p ,(Failure-message result)))]
                      [(Success? result) `((p "User added."))]))]))

;; returns list of xexprs
(define (post->process-roster bindings)
  (cond [(not (exists-binding? 'file bindings)) `((p "No roster file found."))]
        [else (let* ((data (extract-binding/single 'file bindings))
                     (results (register-roster (bytes->string/utf-8 data))))
                (map render-result results))]))

;; render a result as an xexpr
(define (render-result result)
  (cond [(Success? result) `(p ,(Success-result result) " added.")]
        [(Failure? result) `(p ((style "font-weight:bold; color:red"))
                               ,(Failure-message result))]))



;; react to a #"get", *or* to a #"post" after its business
;; is done. in the second case, the 'message' contains text
;; about the result of that operation.
(define (do-load session rest message)
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (action (if (null? rest) DISPLAY-ROSTER (car rest)))
         [extra-message (if (null? message) '() message)]
         [body (select-body action rest session)]
         [header (string-append (class-name) " - Roster")])
    (basic-page header extra-message body)))

(define (select-body action rest session)
  (cond [(equal? action DISPLAY-ROSTER) (display-roster session)]
        [(equal? action NEW-STUDENT) (new-student session)]
        [(equal? action UPLOAD-ROSTER) (upload-roster session)]
        [(equal? action EDIT-USER) (edit-user rest session)]
        [else (display-roster session)]))

;; FIXME API DESIGN ickiness: any string other than the two specified
;; ones turns into an edit-user? ick.
(define (edit-user rest session)
  (match rest
    [(list _1 uid "change-role" thingy)
     (do-change-role uid thingy session)]
    [(list _1 uid "drop")
     (do-drop-user uid session)]
    [(list-rest _1 uid _2)
     `((h2 "Editing User")
       (p "User ID: " ,uid)
       ,(role-change-xexpr session uid "student-role"    "Student")
       ,(role-change-xexpr session uid "ta-role"         "Teaching Assistant")
       ,(role-change-xexpr session uid "instructor-role" "Instructor")
       (p (a ((href ,(url-path->url-string
                      (ct-url-path session "roster" "edit" uid "drop"))))
             "Drop User from Course")))]
    [other
     (raise-404-not-found)]))

;; a role change item
(define (role-change-xexpr session uid role role-str)
  `(p (a ((href ,(url-path->url-string
                  (ct-url-path session "roster" "edit" uid "change-role" role))))
         "Set Role: " ,role-str)))

(define (do-change-role uid role session)
  (define result (change-role uid (string->symbol role)))
  (cond [(Success? result)
         `((p "User role changed.") ,(roster-link session))]
        [(Failure? result)
         `((p ,(Failure-message result)) ,(roster-link session))]))

(define (do-drop-user uid session)
  (let ((result (drop-uid uid)))
    (cond [(Success? result)
           `((p "User dropped.") ,(roster-link session))]
          [(Failure? result)
           `((p (Failure-message result)) ,(roster-link session))])))

(define (roster-link session)
  `(p (a ((href ,(url-path->url-string (ct-url-path session "roster"))))
         "Back to Roster")))
         

(define (new-student session)
  `((h2 "New User")
    (p "Enter the User ID you would like to add to the roster.")
    (form ((action ,(url-path->url-string
                     (ct-url-path session "roster")))
           (method "post"))
          (input ((type "hidden") (name "action") (value ,CREATE-STUDENT)))
          
          (p "User ID: " (input ((name "uid") (type "text"))))
          (p (input ((type "submit") (value "Submit")))))))

(define (upload-roster session)
  `((h2 "Upload Roster")
    (p "Select a file that has one user id per line. Each user id will be"
       " added as a student. Their role may be changed later.") 
    (form ((action ,(url-path->url-string
                     (ct-url-path session "roster")))
           (method "post")
           (enctype "multipart/form-data"))
          (input ((type "hidden")
                  (name "action")
                  (value ,PROCESS-ROSTER)))
          (p (input ((type "file") (name "file"))))
          (p (input ((type "submit")))))))

(define (display-roster session)
  (let ((records (role:all (class-name))))
    (display-roster/render records session)))

;; given list of roles and session, render display page.
;; this one's testable
(define (display-roster/render records session)
  (append
   `((p (a ((href ,(url-path->url-string
                    (ct-url-path session "roster" UPLOAD-ROSTER))))
           "Upload Roster"))
     (p (a ((href ,(url-path->url-string
                    (ct-url-path session "roster" NEW-STUDENT))))
           "New User")))
   (render-instructors records session)
   (render-tas records session)
   (render-students records session)))

(define (is-role? id)
  (lambda (record)
    (let* ((role (role:ClassRole-role record))
           (role-id (roles:Record-id role)))
      (= role-id id))))

(define is-instructor? (is-role? instructor-role))
(define is-ta? (is-role? ta-role))
(define is-student? (is-role? student-role))

(define (render-role title pred?)
  (lambda (records session)
    (let* ((records (filter pred? records))
           (output (map (render-record session) records)))
      (cons title output))))

(define render-instructors (render-role `(h2 "Instructors") is-instructor?))
(define render-tas (render-role `(h2 "Teaching Assistants") is-ta?))
(define render-students (render-role `(h2 "Students") is-student?))

(define (render-record session)
  (lambda (record)
    (let* ((uid (role:ClassRole-uid record)))
      `(p ,uid " - "
          (a ((href ,(url-path->url-string
                      (ct-url-path session "roster" EDIT-USER uid))))
             "Edit User")))))

(module+ test
  (require rackunit
           html-parsing
           (prefix-in roles: "../database/mysql/roles.rkt"))

  (define session (ct-session "classabcd"
                              "user3@example.com"
                              #f
                              ;; bogus start-url, eliminate
                              ;; after regression testing...
                              (hash)))
  (define base-url (ct-url-path session))
  
  ;; REGRESSION TESTING
  (check-equal? ((render-record session)
                 (role:ClassRole "test-class-y"
                                 "bogus12@example.com"
                                 (roles:Record instructor-role "Joe Bloggs" #f)))
                '(p "bogus12@example.com" " - "
                    (a ((href "/classabcd/roster/edit/bogus12@example.com"))
                       "Edit User")))

  (define records
    (list
     (role:ClassRole "test-class-y1"
                     "bogus12@example.com"
                     (roles:Record instructor-role "Instructor" #t))
     (role:ClassRole "test-class-y2"
                     "bogus13@example.com"
                     (roles:Record ta-role "Zorgwardohho" #f))
     (role:ClassRole "test-class-y2"
                     "bogus14@example.com"
                     (roles:Record ta-role "Zorgwaroud" #f))
     (role:ClassRole "test-class-y2"
                     "bogus15@example.com"
                     (roles:Record student-role "1124Zorgward" #t))
     (role:ClassRole "test-class-y2"
                     "bogus16@example.com"
                     (roles:Record ta-role "Zorgwaaaard" #t))
     (role:ClassRole "test-class-y2"
                     "bogus17@example.com"
                     (roles:Record instructor-role "Zoooorgward" #f))
     (role:ClassRole "test-class-y2"
                     "bogus18@example.com"
                     (roles:Record student-role  "Zaouorgward" #f))))

  ;; regression test
  (check-equal?
   (display-roster/render records session)
   '((p (a ((href "/classabcd/roster/upload-roster"))
           "Upload Roster"))
     (p (a ((href "/classabcd/roster/new-student")) "New User"))
     (h2 "Instructors")
     (p "bogus12@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus12@example.com"))
           "Edit User"))
     (p "bogus17@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus17@example.com"))
           "Edit User"))
     (h2 "Teaching Assistants")
     (p "bogus13@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus13@example.com"))
           "Edit User"))
     (p "bogus14@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus14@example.com"))
           "Edit User"))
     (p "bogus16@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus16@example.com"))
           "Edit User"))
     (h2 "Students")
     (p "bogus15@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus15@example.com"))
           "Edit User"))
     (p "bogus18@example.com" " - "
        (a ((href "/classabcd/roster/edit/bogus18@example.com"))
           "Edit User"))))

  ;; regression test
  (check-equal?
   (upload-roster session)
   '((h2 "Upload Roster")
     (p "Select a file that has one user id per line. Each user id will be"
        " added as a student. Their role may be changed later.")
     (form ((action "/classabcd/roster")
            (method "post")
            (enctype "multipart/form-data"))
           (input ((type "hidden")
                   (name "action")
                   (value "process-roster")))
           (p (input ((type "file") (name "file"))))
           (p (input ((type "submit")))))))

  ;; regression test

  (check-equal?
   (new-student session)
   '((h2 "New User")
     (p "Enter the User ID you would like to add to the roster.")
     (form ((action "/classabcd/roster") (method "post"))
           (input ((type "hidden") (name "action") (value "create-student")))
           (p "User ID: " (input ((name "uid") (type "text"))))
           (p (input ((type "submit") (value "Submit")))))))

  ;; regression testing
  (check-equal?
   (edit-user (list "edit" "bogo23@example.com") session)
   '((h2 "Editing User")
     (p "User ID: " "bogo23@example.com")
     (p (a ((href "/classabcd/roster/edit/bogo23@example.com/change-role/student-role"))
           "Set Role: " "Student"))
     (p (a ((href "/classabcd/roster/edit/bogo23@example.com/change-role/ta-role"))
           "Set Role: " "Teaching Assistant"))
     (p (a ((href "/classabcd/roster/edit/bogo23@example.com/change-role/instructor-role"))
           "Set Role: " "Instructor"))
     (p (a ((href "/classabcd/roster/edit/bogo23@example.com/drop"))
           "Drop User from Course"))))

  (check-equal? (render-result (Success "zigbo"))
                `(p "zigbo" " added."))

  (check-equal? (render-result (Failure "drngpa not added"))
                `(p ((style "font-weight:bold; color:red"))
                    "drngpa not added"))
  
  )