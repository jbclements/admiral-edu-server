#lang racket/base

(require racket/string
         racket/list
         racket/contract
         racket/match
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../storage/storage.rkt"
         "../base.rkt"
         "../email/email.rkt"
         "../util/file-extension-type.rkt"
         (prefix-in error: "errors.rkt")
         "templates.rkt"
         "file-container-helpers.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

;; displays page for performing review, but also handles
;; click on submit review link.
(provide load)
(define (load session role rest [message '()])
  (let ((submit? (equal? "submit" (car rest))))
    (if submit? 
        (do-submit-review session role rest message)
        ;; now returns a response:
        (do-load session role rest message))))

(provide check-download)
(define (check-download session role rest)
  ;; FIXME rest check needed
  (let* ((r-hash (car rest))
         (review (try-select-by-hash r-hash)))
    (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
    (define path (cdr rest))
    (define len (length path))
    (define file-path (string-join (append (take path (- len 2)) (list (last path))) "/"))
    (push->download session file-path review))) 

;; display the review page
(define (do-load session role rest message)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define r-hash (car rest))
  (define review (try-select-by-hash r-hash))
  (define step (review:Record-step-id review))
  (define completed (review:Record-completed review))
  (define updir (apply string-append (repeat "../" (+ (length rest) 1))))
  (define root-url updir)
  (define no-modifications
    (if completed
        `((p "This review has already been submitted. Modifications will not be saved."))
        `()))
  (define submit-url (if completed "#" (string-append start-url root-url "review/submit/" r-hash "/")))
  (define updir-rubric (apply string-append (repeat "../" (- (length rest) 1))))
  (define file-container (string-append start-url updir "file-container/" (to-path rest)))
  (define save-url (xexpr->string (string-append "\"" start-url updir-rubric step "/save\"")))
  (define load-url (xexpr->string (string-append "\"" start-url updir-rubric step "/load\"")))
  (when (not (validate review session))
    (raise-403-not-authorized "You are not authorized to see this page."))
  (review-page save-url load-url file-container no-modifications completed submit-url))

(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewer (review:Record-reviewer-id review)))
    (equal? uid reviewer)))

;; user clicked on the "submit" button for the review.
;; note that the data is already saved by this point; this
;; call just marks the review as submitted and prevents
;; later changes to the review.
(define (do-submit-review session role rest message)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define r-hash (cadr rest))
  (define review (try-select-by-hash r-hash))
  ;; FIXME needs attention: use xexprs, raise errors, etc. etc.
  (let* ((assignment (review:Record-assignment-id review))
         (step (review:Record-step-id review))
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir)
         (completed (review:Record-completed review)))
    (cond [completed "<p>The review you were trying to submit has already been submitted. You may not submit it again.</p>"]
          [else
           (review:mark-complete r-hash)
           (send-review-ready-email review)
           (string-append "<p>Review Submitted</p>"
                          "<p><a href='" start-url root-url "feedback/" assignment "/'>Continue</a></p>")])))

(define (send-review-ready-email review)
  (let* ((uid (review:Record-reviewee-id review))
         [assignment-id (review:Record-assignment-id review)]
         [step-id (review:Record-step-id review)]
         [access-url (string-append "https://" (sub-domain) (server-name) "/" (class-name) "/feedback/" assignment-id "/")]
         (message (include-template "../email/templates/review-ready.txt")))
    (send-email uid "Someone has completed a review of your work." message)))

;; save part of a review.
;; called by codemirror autosave, and also triggered by changes
;; to review elements. There will be a bunch of these, and then a single
;; GET request made by a click on the "submit" button.
;; the result of this will not be seen as a page by the user.
(provide post->review)
(define (post->review session post-data rest)
  (let* ((r-hash (car rest))
         (review (try-select-by-hash r-hash)))
    (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
    (cond
      [(equal? (last rest) "save") (post->save-rubric session post-data review)]
      [(equal? (last rest) "load") (post->load-rubric session review)]
      [else (raise-404-not-found "Bad path")])))

;; given session, post data for either rubric or textual comments,
;; and hash of review, save data.
;; NOTE: silently discards data if the review has already been
;; finalized. I'm guessing this is because observing this
;; would require more front-end code to handle the AJAX response.
(define (post->save-rubric session post-data review)
  (let ((data (jsexpr->string (bytes->jsexpr post-data)))
        (class (ct-session-class session))
        (assignment (review:Record-assignment-id review))
        (stepName (review:Record-step-id review))
        (reviewee (review:Record-reviewee-id review))
        (reviewer (ct-session-uid session))
        (review-id (review:Record-review-id review)))
    (when (not (review:Record-completed review))
      ;; FIXME no validation of rubric form... I'm guessing
      ;; this will mess up the viewing later.
      (save-rubric class assignment stepName review-id reviewer reviewee data))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 "Success")))))
  
(define (post->load-rubric session review)
  (let* ((class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         (reviewer (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (retrieve-rubric class assignment stepName review-id reviewer reviewee)))
    (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

;; load or save review comments
(provide push->file-container)
(define (push->file-container session post-data rest)
  (define r-hash (car rest))
  ;; FIXME icky path generation
  (define path (string-join (take (cdr rest) (- (length rest) 2))  "/"))
  (define review (try-select-by-hash r-hash))
  (when (not (validate review session))
    (raise-403-not-authorized "You are not authorized to see this page."))
  (cond 
    [(equal? (last rest) "save") (push->save session post-data path review)]
    [(equal? (last rest) "load") (push->load session path review)]
    [else (raise-404-not-found)]))

;; save a json object representing review comments. silently discard
;; data if review is already complete.
(define (push->save session post-data path review)
  (let ((data (jsexpr->string (bytes->jsexpr post-data)))
        (class (ct-session-class session))
        (assignment (review:Record-assignment-id review))
        (stepName (review:Record-step-id review))
        (reviewee (review:Record-reviewee-id review))
        (reviewer (ct-session-uid session))
        (review-id (review:Record-review-id review)))
    (when (not (review:Record-completed review))
      (save-review-comments class assignment stepName review-id reviewer reviewee path data))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 "Success")))))

(define (push->load session path review)
  (let* ((class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         (reviewer (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee path)))
    (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (push->download session path review)
  (when (not (validate review session))
    (raise-403-not-authorized "You are not authorized to see this page."))
  (define class (ct-session-class session))
  (define assignment (review:Record-assignment-id review))
  (define stepName (review:Record-step-id review))
  (define reviewee (review:Record-reviewee-id review))
  (define data (maybe-get-file-bytes class assignment stepName reviewee path))
  (unless data
    (raise-403-not-authorized "You are not authorized to see this page."))
  (response/full
   200 #"Okay"
   (current-seconds) #"application/octet-stream; charset=utf-8"
   empty
   (list data)))
  
(provide (contract-out
          [file-container
           (-> ct-session? any/c (listof string?) any)]))
;; FIXME huge amount of shared code with 'do-file-container' in browse
(define (file-container session role rest)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define r-hash (car rest))
  (define review (try-select-by-hash r-hash))
  (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
  (let* ((class (ct-session-class session))
         [assignment (review:Record-assignment-id review)]
         [default-mode (determine-mode-from-filename (last rest))]
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         [save-url (string-append "'" start-url "save'")]
         [load-url (string-append "'" start-url "load'")]
         [step (to-step-link stepName (- (length rest) 2))]
         (last-path (last rest))
         [path (to-path-html (cdr rest))]
         (file (to-path (cdr rest)))
         (test-prime (newline))
         (file-path (submission-file-path class assignment reviewee stepName file)))
    (define is-dir (is-directory? file-path))
    (define contents (if (is-directory? file-path)
                         (render-directory file-path start-url)
                         (render-file file-path)))
    (define maybe-file-url
      (if is-dir #f (download-url start-url file #:dotdot-hack #t)))
    (file-container-page default-mode save-url load-url assignment step path contents
                         maybe-file-url)))

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

;; generate a textarea to be replaced by the codemirror instance
(define (render-file file-path)
  (unless (eq? (path-info file-path) 'file)
    (raise-403-not-authorized "You are not allowed to see this page."))
  '((textarea ((id "file") (class "file")) "")))

;; given a step-id and a depth, generate an xexpr for a link
;; with the name of the step and a path with the given number of
;; dotdots. CF test case.
(define (to-step-link step depth)
  (if (< depth 0) (xexpr->string step)
      ;; FIXME yucky paths
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) "./")))
        `(a ((href ,updepth)) ,step))))

(module+ test
  (require rackunit)
  ;; DERIVED FROM REGRESSION
  (check-equal? (to-step-link "argwarg" 4)
                '(a ((href "../../../.././")) "argwarg")))

(define (to-path ls)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let ((new-acc (cons "/" (cons head acc))))
                           
                                           
                                           (helper new-acc tail))]))))
    (helper '() ls)))

;; given a user-supplied hash, try to retrieve the review.
;; signal a 404 if nothing is found.
(define (try-select-by-hash hash)
  (define review (review:maybe-select-by-hash hash))
  (when (not review)
    (raise-403-not-authorized "This page does not exist, or you are not authorized to see it."))
  review)
