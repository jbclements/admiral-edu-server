#lang racket/base

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         yaml)

(require "../base.rkt"
         "../temporary-hacks.rkt"
         (prefix-in error: "errors.rkt")
         "../authoring/assignment.rkt"
         "../storage/storage.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide next)
(define (next session role rest [message '()])
  (let* ((assignment-id (car rest))
         (assignment-record (assignment:select (class-name) assignment-id))
         (is-open (assignment:Record-open assignment-record))
         (start-url (hash-ref (ct-session-table session) 'start-url))
         (user-id (ct-session-uid session)))
    (if (not is-open)
        (error:assignment-closed-response)
        (let* ((uid (ct-session-uid session))
               (assignment (car rest))     
               (do-next (next-step assignment-id uid)))
          (dreadful-hack
           (cond 
             [(MustSubmitNext? do-next) (handle-submit-next session assignment user-id do-next start-url)]
             [(MustReviewNext? do-next) (handle-review-next do-next start-url)]
             [(eq? #t do-next) (assignment-completed)]
             [else (error "Unknown next-action.")]))))))

(define (handle-submit-next session assignment-id user-id action start-url)
  (let* ((step (MustSubmitNext-step action))
         (instruction (Step-instructions step))
         (step-id (Step-id step))
         (exists (submission:exists? assignment-id (class-name) step-id user-id)))
    (cond [exists (view-publish session step-id instruction start-url assignment-id)]
          ;; hack to translate to string ... remove when view-publish is updated to xexprs:
          [else (view-upload step-id instruction start-url assignment-id)])))



(define (view-publish session step-id instruction start-url assignment-id)
  ;; FIXME path-appending, need security for ids
  (let* ((submit-url (string-append start-url "../../submit/" assignment-id  "/" step-id "/"))
         (browse-url (string-append start-url "../../browse/" assignment-id "/" step-id "/"))
         (class (ct-session-class session))
         (user-id (ct-session-uid session))
         (the-path (submission-path class assignment-id user-id step-id))
         (publish-okay (> (length (list-files the-path)) 0)))
    ;; FIXME CSS BUGS, use xexprs:
    `((p "Below is your current submission to '" ,step-id "'. It has not yet "
         "been published. You may make changes until you are ready to publish.")
      (iframe ((width "800px") (height "600px") (style "border: none")
                               (src ,browse-url) (scrolling "no")) " ")
      (h3 "Publish Current Submission")
      (p (b "Warning:") "After publishing, you may not make any changes to "
         "this submission step. Make sure all files you would like to submit "
         "for this step are present in the preview above before clicking the "
         "button below.")
      ,(cond [publish-okay
              `(form ((action ,submit-url) (method "post"))
                     (input ((type "hidden") (name "action") (value "submit")))
                     (input ((type "submit") (value "Publish Submission"))))]
             [else
              `(p "Your submission contains no files. Please upload a new "
                 "submission before publishing this one.")])
      (h3 "Upload new submission")
      (p (b "Warning:") "This will overwrite your current submission.")
      (form ((action ,submit-url) (method "post") (enctype "multipart/form-data"))
            (p "Instructions: " ,instruction)
            (p "File:")
            (p (input ((type "file") (id "file") (name "file"))))
            (script "function enableUpload(){"
                   "if(document.getElementById('verify').checked){"
                   "document.getElementById('upload').disabled = false;"
                   "} else {"
                   " document.getElementById('upload').disabled = true;"
                   "}""}")
            (p (input ((id "verify") (type "checkbox") (onchange "enableUpload()")
                                     (value "understand")))
               "I understand that I am overwriting my current submission.")
            (p (input ((id "upload") (type "submit") (disabled "true")
                                     (value "Upload"))))))))
        
(define (view-upload step-id instruction start-url assignment-id)
  `((p "You are uploading a submission to '" ,step-id "'.")
    (p "Instructions: " ,instruction)
    ;; FIXME PATH BULDING
    (form ((action ,(string-append start-url "../../submit/" assignment-id "/" step-id "/"))
           (method "post")
           (enctype "multipart/form-data"))
          (p "File:")
          (p (input ((type "file") (id "file") (name "file"))))
          (p (input ((type "submit") (value "Upload")))))))

;; returns a list of xexprs
(define (handle-review-next action start-url)
  (let* ((step (MustReviewNext-step action))
         (step-id (Step-id step))
         (reviews (MustReviewNext-reviews action))
         )
    `((p "You must complete the following reviews: ")
      . ,(map (review-link start-url) reviews))))

;; returns a single xexpr
(define (review-link start-url)
  (lambda (hash)
    (let* ((review (review:select-by-hash hash))
           (completed (review:Record-completed review))
           (reviewee (review:Record-reviewee-id review)))
      (cond [completed ""]
            [(string=? reviewee "HOLD")
             `(p "This review is on hold. You will be notified when this review is assigned.")]
            [else
             `(p (a ((href ,(string-append start-url "../../review/" hash "/")))
                    "Review"))]))))

(define (assignment-completed)
  '("You have completed this assignment."))
                      
