#lang racket/base

(require racket/list
         racket/string
         racket/match
         web-server/http/bindings
         web-server/templates
         xml
         web-server/http/bindings)

(require "../storage/storage.rkt"
         "../base.rkt"
         "../urls.rkt"
         (prefix-in error: "errors.rkt")
         "responses.rkt"
         "templates.rkt"
         "../util/file-extension-type.rkt"
         "../authoring/assignment.rkt"
         "file-container-helpers.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
;; handles a GET to /feedback/...
;; FIXME message is never passed?
(define (load session role rest [message '()])
  (when (empty? rest)
    (raise-404-not-found "Path not found."))
  (let ((action (car rest)))
    ;; this one returns a response:
    (cond [(equal? "view" action) (do-view session (cdr rest) message)]
          [(equal? "file-container" action) (do-file-container session role (cdr rest) message)]
          [else (do-default session role rest message)])))

(provide post)
;; handles a POST to /feedback/...
(define (post session role rest bindings post-data)
  ;; FIXME if there's no rest...? URL patterns please
  (let ((action (car rest))
        (submit? (exists-binding? 'feedback bindings)))
    (cond [submit? (post->do-feedback-submit session (cadr rest) bindings )]
          [(equal? "file-container" action) (post->do-file-container session role (cdr rest) post-data)]
          [(equal? "view" action) (post->do-view session (cdr rest) post-data)]
          [else (raise-403-not-authorized
                 "You are not authorized to perform this action.")])))

;; save feedback and/or flag on a review
(define (post->do-feedback-submit session review-hash bindings)
  (let* ((review (review:select-by-hash review-hash))
         (uid (ct-session-uid session))
         (reviewee (review:Record-reviewee-id review))
         (match (equal? uid reviewee))
         (feedback (if (exists-binding? 'feedback bindings) (extract-binding/single 'feedback bindings) ""))
         (flag (if (exists-binding? 'flag bindings) #t #f)))
    (when (not match)
      (raise-403-not-authorized))
    (review:set-flagged review-hash flag)
    (save-review-feedback review feedback)
    (do-view session (list review-hash) `((p "Feedback submitted.")))))

(define (do-default session role rest message)
  (let* ((uid (ct-session-uid session))
         (start-url (hash-ref (ct-session-table session) 'start-url))
         (assignment (car rest))
         (reviews (review:select-feedback (class-name) assignment uid))
         (submissions (submission:select-from-assignment assignment (class-name) uid))
         (results 
          (string-append (gen-status assignment uid start-url)
                         (gen-submissions submissions start-url)
                         (gen-pending-reviews assignment uid start-url)
                         (gen-completed-reviews assignment uid start-url)
                         (gen-review-feedback reviews start-url))))
    (string-append "<h1>" assignment "</h1>"
                   results)))

(define (gen-status assignment uid start-url)
  ;; FIXME strings
  (string-append "<h2>Next Required Action</h2>"
                 (let ((do-next (next-step assignment uid)))
                   (cond 
                     [(MustSubmitNext? do-next) (gen-submit-next start-url assignment do-next)]
                     [(MustReviewNext? do-next) "<p>You must complete pending reviews before you can proceed to the next step.</p>"]
                     [(eq? #t do-next) "You have completed all of the steps for this assignment."]
                     [else (error "Unknown next-action.")]))))

(define (gen-submit-next start-url assignment msn)
  (string-append "<p>You must <a href='" start-url "../../next/" assignment "/'>publish a submission</a> for the next step: '" (Step-id (MustSubmitNext-step msn)) "'.</p>"))
  

(define (gen-review-feedback reviews start-url)
  (let* ((feedback (gen-reviews reviews start-url))
         (message (if (empty? reviews) "You have not received any feedback for this assignment."
                      "The links below are to reviews completed on your submissions.")))
    ;; FIXME strings
    (string-append "<h2>Review Feedback</h2><p>" message "</p>" feedback)))

;; generate page text informing user of reviews to be completed
(define (gen-pending-reviews assignment uid start-url)
  (let* ((reviews (review:select-pending assignment (class-name) uid))
         (pending (map (gen-pending-review start-url) reviews))
         (message (if (empty? pending) "No pending reviews."
                      (string-append "The links below are to reviews that you have not yet completed. "
                                     "As you work on them, they automatically save. If you want, you may "
                                     "work on them and come back later. Once you are satisfied with your "
                                     "review, press submit to send it to the author. Once you have submitted, "
                                     "you may not make additional changes."))))
    (string-join
     (map xexpr->string
          (append `((h2 "Pending Reviews") (p ,message)) pending)))))

;; generate page text informing user of a single review to be completed
(define (gen-pending-review start-url)
  (lambda (record)
    (let ((step (review:Record-step-id record))
          (hash (review:Record-hash record))
          (reviewee (review:Record-reviewee-id record)))
      (cond [(string=? reviewee "HOLD") '(li (p "There are currently no submissions available for you to review. You will be notified as soon as one has been assigned to you."))]
            [else `(li (a ((href ,(string-append start-url "../../review/" hash "/"))) "Pending Review for '" ,step "'"))]))))

(define (gen-completed-reviews assignment uid start-url)
  (let* ((reviews (review:select-completed assignment (class-name) uid))
         (completed (map (gen-completed-review start-url) reviews))
         (message (if (empty? completed) "You have not completed any reviews." 
                      "The links below are to reviews that you have already completed.")))
    (string-join
     (map xexpr->string
          (append `((h2 "Completed Reviews") (p ,message)) completed)))))

(define (gen-completed-review start-url)
  (lambda (record)
    (let ((step (review:Record-step-id record))
          (hash (review:Record-hash record)))
      ;; FIXME paths
      `(li (a ((href ,(string-append start-url "../../review/" hash "/"))) "Completed Review for '" ,step "'")))))

(define (gen-submissions submissions start-url)
  (let* ((submissions (map (gen-submission start-url) submissions))
         (message (if (empty? submissions) "You have not made any submissions for this assignment yet."
                      "The links below are to the submissions you've made for this assignment.")))
  (string-join
   (map xexpr->string
        (append `((h2 "Browse Submissions") (p ,message)) submissions)))))

(define (gen-submission start-url)
  (lambda (record)
    (let ((assignment-id (submission:Record-assignment record))
          (step-id (submission:Record-step record)))
      ;; FIXME paths
    `(li (a ((href ,(string-append start-url "../../browse/" assignment-id "/" step-id "/"))) ,step-id)))))

(define (gen-reviews reviews start-url) (gen-reviews-helper reviews 1 start-url))

(define (gen-reviews-helper reviews cur start-url)
  (if (null? reviews) ""
      (let* ((review (car reviews))
             (hash (review:Record-hash review))
             (step (review:Record-step-id review))
             (rest (cdr reviews)))
        ;; FIXME XSS
        (string-append "<li><a href='" start-url "../view/" hash "/'>Review #" (number->string cur) ": " step "</a></li>" (gen-reviews-helper rest (+ 1 cur) start-url)))))
         
;; show review, allow feedback. returns response
(define (do-view session rest message)
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (assignment (review:Record-assignment-id review))
         (step (review:Record-step-id review))
         ;; FIXME these urls are silly, I believe.
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir)
         [display-message message]
         [review-feedback (load-review-feedback review)]
         [review-flagged? (review:Record-flagged review)]
         [submit-url (string-append start-url root-url "review/submit/" r-hash "/")]
         (updir-rubric (apply string-append (repeat "../" (- (length rest) 1))))
         [file-container (string-append start-url updir "file-container/" (to-path rest))]
         [load-url (xexpr->string (string-append "\"" start-url updir-rubric step "/load\""))]
         (reviewer (ct-session-uid session))
         (class (ct-session-class session)))
    (review:mark-feedback-viewed r-hash)
    (when (not (validate review session))
      (raise-403-not-authorized))
    (feedback-page load-url file-container display-message review-flagged? review-feedback)))

(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewee (review:Record-reviewee-id review)))
    (equal? uid reviewee)))

;; generate the file-container iframe for feedback viewing.
;; FIXME once again lots and lots of duplicated code with the
;; other file-container pages.
;; 'rest' represents the path to the file in the local storage.
(define (do-file-container session role rest [message '()])
  (define r-hash (car rest))
  ;; FIXME a server error on a bogus hash here:
  (define review (review:select-by-hash r-hash))
  (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (class (ct-session-class session))
         [assignment (review:Record-assignment-id review)]
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         [default-mode (determine-mode-from-filename (last rest))]
         ;; FIXME is this going to work with subdirectories?
         [load-url (string-append "'" start-url "load" "'")]
         [step (to-step-link stepName (- (length rest) 2))]
         (last-path (last rest))
         (prefix (if (equal? last-path "") "" (string-append last-path "/")))
         [path (to-path-html (cdr rest))]
         (file (to-path (cdr rest)))
         (test-prime (newline))
         (file-path (submission-file-path class assignment reviewee stepName file)))
    (define is-dir (is-directory? file-path))
    (define contents (if is-dir
                         (render-directory file-path start-url #:show-download #f)
                         ;; FIXME shouldn't there be a 403 check like in review.rkt?
                         render-file))
    (define maybe-file-url
      (if is-dir #f
          (construct-url-path session (cons "download" rest))))
    (feedback-file-container-page assignment step path default-mode contents load-url maybe-file-url)))


#;(retrieve-file file-path)

(define (determine-mode-from-filename filename)
  (let* ((split (string-split filename "."))
         (ext (if (null? split) "" (last split))))
    (extension->file-type ext)))

(define (prepare-url word rest)
  (let* ((last-el (last rest))
         (prefix (if (equal? last-el "") "" (string-append last-el "/"))))
    (string-append "\"" prefix word "\"")))

(define (prepare-load-url rest)
  (prepare-url "load" rest))

(define (prepare-save-url rest)
  (prepare-url "save" rest))


;;TODO: Also in pages/review.rkt Should abstract to common function place
(define (to-path ls)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let ((new-acc (cons "/" (cons head acc))))
                           
                                           
                                           (helper new-acc tail))]))))
    (helper '() ls)))



(define (post->do-file-container session role rest post-data)
  (let* ((hash (car rest))
         (path (string-join (take (cdr rest) (- (length rest) 2))  "/"))
         (review (review:select-by-hash hash)))
    ;; FIXME can now return response..
    (if (not (validate review session))
        (error:not-authorized-response)
        (post->load session path review))))

(define (post->load session path review)
  (let* (
         (class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewer (review:Record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee path)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))


(define (post->do-view session rest post-data)
  (let* ((hash (car rest))
         (review (review:select-by-hash hash)))
    (if (not (validate review session))
        ;; FIXME can return response
        (error:not-authorized-response)
        (post->load-rubric session review))))
  
(define (post->load-rubric session review)
  (let* ((class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewer (review:Record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (retrieve-rubric class assignment stepName review-id reviewer reviewee)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))
