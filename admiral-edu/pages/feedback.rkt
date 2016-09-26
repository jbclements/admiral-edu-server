#lang racket/base

(require racket/list
         racket/string
         racket/contract
         racket/match
         web-server/http/bindings
         web-server/templates
         xml
         web-server/http/bindings)

(require "../storage/storage.rkt"
         "../base.rkt"
         "../paths.rkt"
         "path-xexprs.rkt"
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

(provide post)
;; handles a POST to /feedback/...
(define (post session role rest bindings post-data)
  ;; FIXME if there's no rest...? URL patterns please
  (let ((action (car rest))
        ;; FIXME gnarr... no special endpoint for this?
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

;; this is kind of an assignment dashboard, actually....
(provide do-default)
(define (do-default session assignment)
  (let* ((uid (ct-session-uid session))
         (start-url (hash-ref (ct-session-table session) 'start-url))
         (reviews (review:select-feedback (class-name) assignment uid))
         (submissions (submission:select-from-assignment assignment (class-name) uid))
         ;; FIXME xexprs, please.
         (results 
          (string-append (string-join
                          (map xexpr->string
                               (gen-status session assignment uid)))
                         (string-join
                          (map xexpr->string
                               (gen-submissions submissions session)))
                         (gen-pending-reviews assignment uid start-url)
                         (gen-completed-reviews assignment uid start-url)
                         (gen-review-feedback reviews start-url))))
    (string->response
     (string-append (xexpr->string `(h1 ,assignment)) "\n"
                    results))))

(define/contract (gen-status session assignment uid)
  (-> ct-session? basic-ct-id? ct-id? (listof xexpr?))
  `((h2 "Next Required Action")
    ,(let ((do-next (next-step assignment uid)))
       (cond 
         [(MustSubmitNext? do-next)
          (gen-submit-next session assignment do-next)]
         [(MustReviewNext? do-next)
          `(p "You must complete pending reviews before you can proceed to the next step.")]
         [(eq? #t do-next)
          `(p "You have completed all of the steps for this assignment.")]
         [else (error "Unknown next-action.")]))))

(define (gen-submit-next session assignment msn)
  `(p "You must "
      ,(cta `((href ,(ct-url-path-/ session "next" assignment))) "publish a submission")
      " for the next step: '" ,(Step-id (MustSubmitNext-step msn)) "'."))
  

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

(define/contract (gen-submissions submissions session)
  (-> any/c any/c (listof xexpr?))
  (let* ((submissions (map (gen-submission session) submissions))
         (message (if (empty? submissions)
                      "You have not made any submissions for this assignment yet."
                      "The links below are to the submissions you've made for this assignment.")))
    `((h2 "Browse Submissions") (p ,message) ,@submissions)))

(define (gen-submission session)
  (lambda (record)
    (let ((assignment-id (submission:Record-assignment record))
          (step-id (submission:Record-step record)))
      ;; FIXME paths
    `(li ,(cta `((href ,(ct-url-path-/ session "browse" assignment-id step-id)))
               step-id)))))

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
(provide do-view)
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
         ;; FIXME STRINGS
         [load-url (xexpr->string (string-append start-url updir-rubric step "/load"))]
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
;; returns response
(provide do-file-container)
(define (do-file-container session role r-hash rest [message '()])
  ;; FIXME a server error on a bogus hash here:
  (define review (review:maybe-select-by-hash r-hash))
  (when (not review)
    (raise-403-not-authorized))
  (when (not (validate review session))
      (raise-403-not-authorized))
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (class (ct-session-class session))
         [assignment (review:Record-assignment-id review)]
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         ;; FIXME is this going to work with subdirectories?
         ;; FIXME go to ct-paths
         [load-url (string-append start-url "load")]
         [step (to-step-link stepName (- (length rest) 1))]
         [path (to-path-html rest)]
         (file (to-path rest))
         (test-prime (newline))
         (file-path (submission-file-path class assignment reviewee stepName
                                          (apply rel-ct-path rest))))
    (define is-dir (is-directory? (ct-path->path file-path)))
    (define (link-maker path)
      (ct-path-join (ct-url-path-/ session "feedback" "file-container" r-hash)
                    path))
    (define (download-link-maker path)
      (ct-path-join (ct-url-path-/ session "download" r-hash)
                    path))
    (cond [is-dir
           (define contents (render-directory link-maker download-link-maker file-path #:show-download #f))
           (feedback-file-container-page
            assignment step path "bogus" contents load-url #f)]
          [else
           (define default-mode (determine-mode-from-filename (last rest)))
           ;; FIXME shouldn't there be a 403 check like in review.rkt?
           (define contents render-file)
           (define file-url
             (download-link-maker (apply rel-ct-path rest)))
           (feedback-file-container-page
            assignment step path default-mode contents load-url file-url)])))

(define (determine-mode-from-filename filename)
  (let* ((split (string-split filename "."))
         (ext (if (null? split) "" (last split))))
    (extension->file-type ext)))


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
        (raise-403-not-authorized)
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
