#lang racket/base

(require racket/string
         racket/list
         racket/contract
         racket/match
         web-server/templates
         xml
         json)

(require "../storage/storage.rkt"
         "../base.rkt"
         "../email/email.rkt"
         "../util/file-extension-type.rkt"
         "responses.rkt"
         "templates.rkt"
         "../paths.rkt"
         "file-container-helpers.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

;; FIXME call download.rkt fn
;; perform a download
(provide check-download)
(define (check-download session role rest)
  ;; FIXME rest check needed
  (let* ((r-hash (car rest))
         (review (try-select-by-hash r-hash)))
    (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
    (define path (cdr rest))
    (define len (length path))
    ;; FIXME icky path manipulation to remove 'download' token.
    (define file-path (string-join (append (take path (- len 2)) (list (last path))) "/"))
    (push->download session file-path review))) 

;; display the review page
(provide (contract-out
          [do-load (-> ct-session? string? (listof string?) response?)]))
(define (do-load session r-hash rest)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define review (try-select-by-hash r-hash))
  (define step (review:Record-step-id review))
  (define completed (review:Record-completed review))
  (define updir (apply string-append (repeat "../" (+ (length rest) 2))))
  (define root-url updir)
  (define no-modifications
    (if completed
        `((p "This review has already been submitted. Modifications will not be saved."))
        `()))
  (define submit-url (if completed "#" (string-append start-url root-url "review/submit/" r-hash "/")))
  (define updir-rubric (apply string-append (repeat "../" (length rest))))
  (define file-container (string-append start-url updir "file-container/" (to-path (cons r-hash rest))))
  ;; FIXME strings
  (define save-url (xexpr->string (string-append start-url updir-rubric step "/save")))
  (define load-url (xexpr->string (string-append start-url updir-rubric step "/load")))
  (when (not (validate review session))
    (raise-403-not-authorized "You are not authorized to see this page."))
  (review-page save-url load-url file-container no-modifications completed submit-url))

;; FIXME duplicated in download.rkt
(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewer (review:Record-reviewer-id review)))
    (equal? uid reviewer)))

;; user clicked on the "submit" button for the review.
;; note that the data is already saved by this point; this
;; call just marks the review as submitted and prevents
;; later changes to the review.
(provide (contract-out
          [do-submit-review (-> ct-session? string? (listof string?) response?)]))
(define (do-submit-review session r-hash rest)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define review (try-select-by-hash r-hash))
  (let* ((assignment (review:Record-assignment-id review))
         (completed (review:Record-completed review)))
    (cond [completed
           (plain-page
            "Review Already Submitted"
            `((p "The review you were trying to submit has already been submitted. You may not submit it again.")))]
          [else
           (review:mark-complete r-hash)
           (send-review-ready-email review)
           (plain-page
            "Review Submitted"
            `((h1 "Review Submitted")
              (p (a ((href ,(url-path->url-string
                             (ct-url-path session "feedback" assignment))))
                    "Continue"))))])))

(define (send-review-ready-email review)
  (let* ((uid (review:Record-reviewee-id review))
         [assignment-id (review:Record-assignment-id review)]
         [step-id (review:Record-step-id review)]
         [access-url (ct-path->emailable-url (rel-ct-path "feedback" assignment-id))]
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
      (raise-403-not-authorized))
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
      (raise-403-not-authorized))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

;; load or save review comments (returns response)
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
    (bytes->json-response #"Success")))

;; returns response
(define (push->load session path review)
  (let* ((class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         (reviewer (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee path)))
    (when (not (validate review session))
      (raise-403-not-authorized))
    (bytes->json-response (string->bytes/utf-8 data))))

;; FIXME call download.rkt fn instead.
;; given a session, a list of strings representing a file path, and a review hash,
;; produce a response containing the file's bytes
(define (push->download session path review)
  (when (not (validate review session))
    (raise-403-not-authorized "You are not authorized to see this page."))
  (define class (ct-session-class session))
  (define assignment (review:Record-assignment-id review))
  (define stepName (review:Record-step-id review))
  (define reviewee (review:Record-reviewee-id review))
  (define data (maybe-get-file-bytes class assignment stepName reviewee path))
  (unless data
    (raise-403-not-authorized))
  (bytes->file-response data))
  
(provide (contract-out
          [file-container
           (-> ct-session? ct-id? (listof ct-id?) response?)]))
;; FIXME huge amount of shared code with 'do-file-container' in browse
(define (file-container session r-hash path)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define review (try-select-by-hash r-hash))
  (when (not (validate review session))
      (raise-403-not-authorized "You are not authorized to see this page."))
  (let* ((class (ct-session-class session))
         [assignment (review:Record-assignment-id review)]
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         ;; FIXME use ct-paths instead
         [save-url (string-append start-url "save")]
         [load-url (string-append start-url "load")]
         [step (to-step-link stepName (- (length path) 1))]
         [path-xexprs (to-path-html path)]
         [ct-path (apply rel-ct-path path)]
         (test-prime (newline)))
    (define file-path
      (submission-file-path class assignment reviewee stepName ct-path))
    (define (link-maker path)
      (ct-path-join* (ct-url-path session "file-container" r-hash)
                     ct-path
                     path))
    (define (download-link-maker path)
      (ct-path-join* (ct-url-path session "download" r-hash)
                     ct-path
                     path))
    ;; FIXME eliminate conversion when unnecessary
    (match (path-info (ct-path->path file-path))
      ['directory
       (define contents (render-directory link-maker download-link-maker
                                          file-path))
       (define maybe-file-url #f)
       (file-container-page "" save-url load-url assignment step path-xexprs contents
                            maybe-file-url)]
      ['file
       (when (null? path)
         (error "internal error, empty path treated as file"))
       (define default-mode (determine-mode-from-filename (last path)))
       (define contents render-file)
       (define maybe-file-url
         (download-link-maker ct-path))
       (file-container-page default-mode save-url load-url assignment step path-xexprs contents
                            maybe-file-url)]
      ['does-not-exist
       (raise-403-not-authorized)])))

(define (determine-mode-from-filename filename)
  (let* ((split (string-split filename "."))
         (ext (if (null? split) "" (last split))))
    (extension->file-type ext)))

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
