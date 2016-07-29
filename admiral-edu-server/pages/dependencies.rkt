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
         (only-in "templates.rkt"
                  xexpr->error-page-html)
         (prefix-in assign: "../authoring/assignment.rkt")
         (prefix-in error: "errors.rkt")
         "templates.rkt")

(define THREE-STUDY-ACTION "three-study")
(define LIST-DEPENDENCIES "list-dependencies")
(define (base-url) (string-append "/" (class-name) "/dependencies/"))

(provide dependencies)
(define (dependencies session role rest [message '()])
  (let* ((len (length rest)))
    (cond [(= len 1) (assignment-dependencies (car rest))]
          [(and (= len 2) (string=? THREE-STUDY-ACTION (cadr rest))) (three-study-form (car rest))]
          ;; returns response
          [(> len 2) (dependencies-form (car rest) (cadr rest) (caddr rest) rest)])))

(define (assignment-dependencies assignment-id [message ""])
  (cond [(not (assignment:exists? assignment-id (class-name)))
         (assignment-not-found-response assignment-id)]
        [else (let* ((deps (assign:assignment-id->assignment-dependencies assignment-id))
                     [header (string-append "<a href='/" (class-name) "/assignments/'>Assignments</a>")]
                     (dependency-list (string-append (string-join (map (dep->html assignment-id) deps) "\n")))
                     [extra-message ""]
                     [body (string-append "<h2><a href='/" (class-name) "/assignments/dashboard/" assignment-id "/'>" assignment-id "</a></h2>"
                                          "<p>" message "</p>"
                                          "<p>The links below allow you to preview each rubric and upload file dependencies.</p>"
                                          "<ul>" dependency-list "</ul>"
                                          )])
                (include-template "html/basic.html"))]))

(define (three-study-form assignment-id [message #f])
  (cond [(not (assignment:exists? assignment-id (class-name)))
         (assignment-not-found-response assignment-id)]
        [else 
         (let* ([header assignment-id]
                [extra-message ""]
                [body (render-three-study-form assignment-id)])
           (include-template "html/basic.html"))]))


(define (render-three-study-form assignment-id)
  ;; FIXME should be xexpr...
  (string-append "<p>You are uploading the 3 condition study yaml file.</p>"
                 "<form method='post' action='" (base-url) assignment-id "/" THREE-STUDY-ACTION "/" "' enctype='multipart/form-data'>"
                 "<input type='file' name='three-condition-file'>"
                 "<input type='submit' value='Upload'>"
                 "</form>"))

;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (dep->html assignment-id)
  (lambda (dep)
    (cond [(assign:review-dependency? dep)
           (begin
             ;; FIXME string pasting
             (let* ((sid (assign:review-dependency-step-id dep))
                    (rid (assign:review-dependency-review-id dep))
                    (inst (if (assign:instructor-solution-dependency? dep) " - <b>Instructor Solution</b>" ""))
                    (a-start (string-append "<a href=\"" (base-url) assignment-id "/" sid "/" rid "/\">"))
                    (a-end (if (assign:dependency-met dep) " - Ready" " - Dependencies Missing")))
               (string-append "<li>" 
                              a-start
                              sid ":" rid inst  "</a>"
                              a-end 
                              "</li>")))]
          [(assign:three-study-config-dependency? dep) (begin
                                                         (let ((ready (if (assign:dependency-met dep) " - Ready" " - Dependency Missing")))
                                                           (string-append "<li>"
                                                                          "<a href='" (base-url) assignment-id "/" THREE-STUDY-ACTION "/'>"
                                                                          "Three Study Configuration File" ready
                                                                          "</a>"
                                                                          "</li>")))]

          [else (raise "Unknown dependency")])))

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
    (cond [(equal? action "load") (let ((stepName (cadr rest))
                                        (review-id (caddr rest)))
                                    (load-rubric class assignment stepName review-id))]
          [(equal? action "upload")
           ;; FIXME: check for existence of these!
           (let ((stepName (cadr rest))
                 (review-id (caddr rest)))
             (upload-dependencies class assignment stepName review-id bindings raw-bindings))]
          [(string=? action THREE-STUDY-ACTION) (upload-three-condition assignment bindings raw-bindings)])))

(define (load-rubric class assignment stepName review-id)
  (let ((data (retrieve-default-rubric class assignment stepName review-id)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (upload-three-condition assignment-id bindings raw-bindings)
  (let* ((dep (car (filter assign:three-study-config-dependency? (assign:assignment-id->assignment-dependencies assignment-id))))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
        (assign:check-ready assignment-id)
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (render-result assignment-id result))))))

;; upload review dependencies, return a response. should be a list of xexprs?
(define (upload-dependencies class assignment-id step-id review-id bindings raw-bindings)
  ;; FIXME what's up with the car here? alarming. cf fixme on enforcing exactly one
  ;; dependency in assignment-structs.rkt
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
    (assign:check-ready assignment-id)
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (render-result assignment-id result))))))

(define (render-result assignment-id result)
  (cond [(Success? result) (assignment-dependencies assignment-id (string-append "<p>" (Success-result result) "</p>"))]
        [(Failure? result) (xexpr->error-page-html (Failure-message result))]
        [else (raise (format "Unknown result: ~a" result))]))



;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (generate-dependency-form assignment-id step-id review-id)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (if (assign:student-submission-dependency? dep) (assign:student-submission-dependency-amount dep) 1))
         (instructor-solution (assign:instructor-solution-dependency? dep)))
    ;; FIXME urg strings bad bad
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


(define (assignment-not-found-response assignment-id)
  ;; 400 or 404?
  (error:error-xexprs->400-response
   (string-append "The assignment id '" assignment-id "' was not found.")))