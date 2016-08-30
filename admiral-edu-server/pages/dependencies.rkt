#lang racket/base

(require racket/string
         racket/list
         web-server/http/bindings
         web-server/http/request-structs
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../storage/storage.rkt"
         "../base.rkt"
         "../paths.rkt"
         (only-in "templates.rkt" error-page)
         (prefix-in assign: "../authoring/assignment.rkt")
         "templates.rkt")

(define THREE-STUDY-ACTION "three-study")
(define LIST-DEPENDENCIES "list-dependencies")
(define (base-url) (string-append "/" (class-name) "/dependencies/"))

(provide dependencies)
(define (dependencies session role rest [message '()])
  (let* ((len (length rest)))
    (cond [(= len 1)
           (assignment-dependencies session (car rest))]
          [(and (= len 2) (string=? THREE-STUDY-ACTION (cadr rest)))
           (three-study-form session (car rest))]
          ;; returns response
          [(> len 2)
           (dependencies-form (car rest) (cadr rest) (caddr rest) rest)])))

(define (assignment-dependencies session assignment-id [message '()])
  (when (not (assignment:exists? assignment-id (class-name)))
    (raise-assignment-not-found assignment-id))
  (define deps (assign:assignment-id->assignment-dependencies assignment-id))
  (define assignment-link
    (ct-url-path session "assignments"))
  (define header `(a ((href ,(url-path->url-string assignment-link)))
                     "Assignments"))
  (define body (assignment-dependencies-body session assignment-id
                                             message deps))
  (basic-page header '() body))

;; this function is testable...
(define (assignment-dependencies-body session assignment-id message deps)
  (define dependency-list
    (map (dep->html session assignment-id) deps))
  `((h2 (a ((href
             ,(url-path->url-string
               (ct-url-path session
                            "assignments"
                            "dashboard"
                            assignment-id))))
           ,assignment-id))
    ,@message
    (p "The links below allow you to preview each rubric "
       "and upload file dependencies.")
    (ul
     ,@dependency-list)))

(define (three-study-form session assignment-id [message #f])
  (when (not (assignment:exists? assignment-id (class-name)))
    (raise-assignment-not-found assignment-id))
  (let* ([header assignment-id]
         [extra-message ""]
         [body (render-three-study-form session assignment-id)])
    (basic-page header '() body)))
 

(define (render-three-study-form session assignment-id)
  `((p "You are uploading the 3 condition study yaml file.")
    (form ((method "post")
           (action
            ,(url-path->url-string
              (ct-url-path session
                           "dependencies"
                           assignment-id
                           THREE-STUDY-ACTION)))
           (enctype "multipart/form-data"))
          (input ((type "file")
                  (name "three-condition-file")))
          (input ((type "submit") (value "Upload"))))))



;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define ((dep->html session assignment-id) dep)
  (cond [(assign:review-dependency? dep)
         (let* ((sid (assign:review-dependency-step-id dep))
                (rid (assign:review-dependency-review-id dep))
                (inst (if (assign:instructor-solution-dependency? dep)
                          '(" - " (b "Instructor Solution"))
                          '()))
                (dependency-link
                 `(a ((href ,(url-path->url-string
                              (ct-url-path session "dependencies"
                                           assignment-id sid rid))))
                     ,sid ":" ,rid ,@inst))
                (ready (if (assign:dependency-met dep)
                           " - Ready"
                           " - Dependencies Missing")))
           `(li ,dependency-link ,ready))]
        [(assign:three-study-config-dependency? dep)
         (define ready (if (assign:dependency-met dep)
                           " - Ready"
                           " - Dependency Missing"))
         `(li (a ((href
                   ,(url-path->url-string
                    (ct-url-path session "dependencies" assignment-id
                                 THREE-STUDY-ACTION))))
                 "Three Study Configuration File" ,ready))]
        [else (raise "Unknown dependency")]))



;; return the upload-dependencies form page. returns response
(define (dependencies-form assignment step review-id rest)
  (define dep (car (assign:find-dependencies assignment step review-id)))
  (define met (assign:dependency-met dep))
  (define
    load-url (xexpr->string (string-append "\"" (base-url) (string-join rest "/") "/load\"")))
  (define dependency-form
    (generate-dependency-form assignment step review-id))
  ;; what is this commented-out code for?
  ;;(if met (dependency-met assignment step review-id) (generate-dependency-form assignment step review-id))])
  (dependencies-page load-url dependency-form))

;; handle a post to /dependencies
(provide post)
(define (post session rest bindings raw-bindings)
  (let* ((class (class-name))
         (assignment (car rest))
         ;; FIXME: fails on empty list (this happens in a lot of places...):
         (action (last rest)))
    (cond [(equal? action "load")
           (let ((stepName (cadr rest))
                 (review-id (caddr rest)))
             (load-rubric class assignment stepName review-id))]
          [(equal? action "upload")
           ;; FIXME: check for existence of these!
           (let ((stepName (cadr rest))
                 (review-id (caddr rest)))
             (upload-dependencies session class assignment stepName review-id bindings raw-bindings))]
          [(string=? action THREE-STUDY-ACTION)
           (upload-three-condition assignment bindings raw-bindings)])))

(define (load-rubric class assignment stepName review-id)
  (let ((data (retrieve-default-rubric class assignment stepName review-id)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (upload-three-condition session assignment-id bindings raw-bindings)
  (let* ((dep (car (filter assign:three-study-config-dependency? (assign:assignment-id->assignment-dependencies assignment-id))))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
        (assign:check-ready assignment-id)
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (render-result session assignment-id result))))))

;; upload review dependencies, return a response. should be a list of xexprs?
(define (upload-dependencies session class assignment-id step-id review-id bindings raw-bindings)
  ;; FIXME what's up with the car here? alarming. cf fixme on enforcing exactly one
  ;; dependency in assignment-structs.rkt
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
    (printf "debug...\n")
    (assign:check-ready assignment-id)
    (render-result session assignment-id result)))

(define (render-result session assignment-id result)
  (cond [(Success? result)
         ;; come back here...
         (assignment-dependencies session assignment-id
                                  `((sp ,(Success-result result))))]
        [(Failure? result)
         (error-page (list (Failure-message result)))]
        [else (raise (format "Unknown result: ~a" result))]))



;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (generate-dependency-form assignment-id step-id review-id)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (if (assign:student-submission-dependency? dep) (assign:student-submission-dependency-amount dep) 1))
         (instructor-solution (assign:instructor-solution-dependency? dep)))
    `((p "Assignment id:" ,assignment-id)
      (p "Submission Step id:" ,step-id)
      (p "Review id:" ,review-id)
      (p "This review step requires " ,(number->string amount) " default solution(s).")
      (form ((action ,(string-append (base-url) assignment-id "/" step-id "/" review-id "/upload/"))
             (method "post")
             (enctype "multipart/form-data"))
            ,@(generate-form-string amount)
            (input ((type "submit") (value "Upload")))))))

;; generate the file entry elements of the dependency upload form
;; returns a list of xexprs
(define (generate-form-string n)
  (apply
   append
   (for/list ([i (in-range n)])
     (define ns (number->string (add1 i)))
     `((p ((style "font-weight:bold;")) "Solution #" ,ns)
       (p ((style "margin-left:10px;"))
          (input ((name ,(string-append "file-"ns))
                  (type "file")
                  (id ,(string-append "file-"ns)))))))))


(define (raise-assignment-not-found assignment-id)
  (raise-404-not-found
   (string-append "The assignment id '" assignment-id "' was not found.")))


(module+ test
  (require rackunit
           "../testing/test-configuration.rkt")
  (current-configuration test-conf)

  (define session
    (ct-session (class-name) "bogus1@example.com" #f (hash)))
  
  ;; REGRESSION TEST
  (check-equal?
   (render-three-study-form session
                            "ass-1234")
   '((p "You are uploading the 3 condition study yaml file.")
     (form ((method "post")
            (action "/test-class/dependencies/ass-1234/three-study")
            (enctype "multipart/form-data"))
           (input ((type "file")
                   (name "three-condition-file")))
           (input ((type "submit") (value "Upload"))))))

  ;; REGRESSION TEST
  (check-equal?
   ((dep->html session "ass-id") (assign:review-dependency #f "abc" "def"))
   '(li (a ((href "/test-class/dependencies/ass-id/abc/def"))
           "abc" ":" "def")
        " - Dependencies Missing"))

  ;; REGRESSION TEST
  (check-equal?
   ((dep->html session "ass-id") (assign:review-dependency #t "abc" "def"))
   '(li (a ((href "/test-class/dependencies/ass-id/abc/def"))
           "abc" ":" "def")
        " - Ready"))

  ;; REGRESSION TEST
  (check-equal?
   ((dep->html session "ass-id") (assign:instructor-solution-dependency #f "abc" "def"))
   '(li (a ((href "/test-class/dependencies/ass-id/abc/def"))
           "abc" ":" "def"
           " - "
           (b "Instructor Solution"))
        " - Dependencies Missing")
   )

  ;; REGRESSION
  (check-equal?
   ((dep->html session "ass-id")
    (assign:three-study-config-dependency #f))
   '(li (a ((href "/test-class/dependencies/ass-id/three-study"))
           "Three Study Configuration File"
           " - Dependency Missing")))

  ;; REGRESSION
  (check-equal?
   (assignment-dependencies-body
    session "ass-id" '((p "the message"))
    (list (assign:instructor-solution-dependency #f "abc" "def")
          (assign:review-dependency #t "ghi" "jkl")))
   '((h2 (a ((href "/test-class/assignments/dashboard/ass-id"))
            "ass-id"))
     (p "the message")
     (p
      "The links below allow you to preview each rubric "
      "and upload file dependencies.")
     (ul (li (a ((href "/test-class/dependencies/ass-id/abc/def"))
                "abc" ":" "def"
                " - "
                (b "Instructor Solution"))
             " - Dependencies Missing")
         (li (a ((href "/test-class/dependencies/ass-id/ghi/jkl"))
                "ghi" ":" "jkl")
             " - Ready")))))