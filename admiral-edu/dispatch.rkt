#lang racket/base

(require web-server/servlet
         web-server/dispatch
         racket/date
         racket/string
         racket/match
         racket/list
         racket/path)

(require 
  "auth/google-openidc.rkt"
  "base.rkt"
  "paths.rkt"
  "logging.rkt"
  (only-in "pages/errors.rkt"
           error-xexprs->response))

(require "pages/index.rkt"
         (prefix-in review: "pages/review.rkt")
         (prefix-in error: "pages/errors.rkt")
         (prefix-in author: "pages/author.rkt")
         "pages/next.rkt"
         (prefix-in export: "pages/export.rkt")
         (prefix-in submit: "pages/submit.rkt")
         (prefix-in dep: "pages/dependencies.rkt")
         (prefix-in feedback: "pages/feedback.rkt")
         (prefix-in roster: "pages/roster.rkt")
         (prefix-in browse: "pages/browse.rkt")
         (prefix-in download: "pages/download.rkt")
         (prefix-in typed: "dispatch-typed.rkt"))

(define (any? x)
  #t)

;; Defines how to process incoming requests
;; FIXME this is totally not using dispatch rules in any useful way...
;; this would be a great way to actually tighten up the URL handling.
;; FIXME arrgh! this can handle URL generation as well...
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [((string-arg) ...) (handler #f)]
   [((string-arg) ...) #:method "post" (handler #t)]
   [else error:four-oh-four-response]))

;; initially using this only for interactive testing server...
;; I'm aware of the danger that the interactive server may not
;; share as much code as possible with the production one.
;; the goal is to eliminate the classname-trimming done by the
;; apache forwarding rules, to make the testing env more like
;; the real one.
(provide ct-rules-2)
(define-values (ct-rules-2 mk-url-2)
  (dispatch-rules
   [((string-arg) (string-arg) ...) (handler2 #f)]
   [((string-arg) (string-arg) ...) #:method "post" (handler2 #t)]
   ))

(define ((handler post?) req path)
  ((handler2 post?) req (class-name) path))

;; should replace handler eventually. Too much to fix!
;; given request, classname (ignored, but inserted by dispatch interface), and
;; list of path strings, handle a request
;; NB: currently classname will be #f for real server
(define ((handler2 post?) req classname path)
  (define method (request-method req))
  (log-ct-access-info/nomacro
   (format
    "[~a] ~a - ~a ~a" (date->string (current-date) #t)
    (class-name) method path))
  ;; fixme ditch this:
  (define start-rel-url (ensure-trailing-slash (string-append "/" (class-name) "/" (string-join path "/"))))
  (define session (construct-session req start-rel-url))
  (let* ((raw-bindings (request-bindings/raw req))
         (bindings (request-bindings req))
         (post-data (request-post-data/raw req))
         (clean-path (filter (lambda (x) (not (equal? "" x))) path)))
    (with-handlers ([any? error:server-error-response])
      (handlerPrime post? post-data session
                    bindings raw-bindings clean-path))))

;; given a request, construct a "session"
;; FIXME hoping to get rid of the start-rel-url piece
(define (construct-session req start-rel-url)
  (match (req->uid req)
    [(? string? uid-str)
     (let* ((raw-bindings (request-bindings/raw req))
            (bindings (request-bindings req))
            (session (ct-session (class-name) uid-str #f (make-table start-rel-url bindings))))
       session)]
    [else
     (raise-400-bad-request "Missing Authentication Headers")]))


(define (ensure-trailing-slash candidate)
  (let ((len (string-length candidate)))
    (cond [(= 0 len) "/"]
          [else (let ((last-char (string-ref candidate (- len 1))))
                  (cond [(eq? #\/ last-char) candidate]
                        [else (string-append candidate "/")]))])))

;; given whether this is a POST, the post data, a ct-session, the bindings (should go away),
;; the raw-bindings, and the path, do the stuff and return a response.
(provide handlerPrime)
(define (handlerPrime post? post-data session bindings raw-bindings path)
  (with-handlers
      ([exn:user-error?
        (λ (exn)
          (cond
            [(= (exn:user-error-code exn) 403)
             (error:not-authorized-response
              (exn-message exn))]
            [else
             (error-xexprs->response
              `((p ,(exn-message exn))
                (p "Try returning to "
                   ;; I'm giving this string-append a pass:
                   (a ((href ,(string-append "https://"
                                             (sub-domain) (server-name) "/" (class-name))))
                      "Class Home")
                   " and trying again."))
              (exn:user-error-code exn)
              (match (exn:user-error-code exn)
                [400 #"Bad Request"]
                [404 #"Not Found"]))]))])
    ;; makes patterns more readable:
    (define method (cond [post? #"post"]
                         [else #"get"]))
    (define user-role (session->role session))
    (if (not user-role)
        (error:not-registered-response session)
        (match (list method path)
          [(list _ (or '() '("")))
           ;; root page
           (response/xexpr (index session user-role))]
          
          [(list #"get" (list "download" hash path-str path-strs ...))
          ;; download the raw bytes of a file, based on hash and path.
          ;; used for reviewing and feedback. (N.B. must be at least one path-str)
           (download:do-download session hash (cons path-str path-strs))]
          [(list #"get" (list "browse-download" assignment step path-strs ...))
           ;; download raw bytes. used in browsing (i.e. not as part of a review,
           ;; with a hash)
           (download:do-browse-download session assignment step path-strs)]
          
          [(list #"post" (cons "review" rest))
           ;; used by codemirror autosave and review elements
           ;; to save review.
           (review:post->review session post-data rest)]
          [(list #"get" (list "review" "submit" (? basic-ct-id? hash) rest ...))
           ;; click on review submit button
           (review:do-submit-review session hash rest)]
          [(list #"get" (list "review" (? basic-ct-id? hash) rest ...))
           ;; presents review screen
           (review:do-load session hash rest)]
          
          [(list #"post" (cons "file-container" rest))
           ;; save or load contents of review. bit of an abuse of POST.
           (review:push->file-container session post-data rest)]
          ;; FIXME this clause is now completely dead, if the download hack is gone.
          [(list #"get" (list "file-container" (? ct-id? rest) ... "download" (? ct-id? last-str)))
           (review:check-download session user-role (append rest (list "download" last-str)))]
          [(list #"get" (list "file-container" (? basic-ct-id? r-hash) (? ct-id? path-strs) ...))
           (review:file-container session r-hash path-strs)]
          
          [(list _ (list-rest "su" uid rest))
           ;; interface for executing a command as another user
           (with-sudo post? post-data uid session user-role bindings raw-bindings rest)]
          
          [(list #"post" (list "author" _ ... "validate"))
           ;; add a new assignment
           ;; AJAX call, doesn't return html
           (author:validate session post-data #t)]
          [(list #"post" (list "author" _ ... "validate-save"))
           ;; AJAX call, doesn't return html
           ;; add a new assignment, replacing an old one
           (author:validate session post-data #f)]
          [(list #"get" (cons "author" rest))
           ;; interface for adding, editing assignments
           (author:load session user-role rest)]
          
          ;; "/next/..."
          [(list _ (list "next" (? basic-ct-id? assignment-id) ignored ...))
           (next session assignment-id)]
          
          ;; "/dependencies/..."
          [(list #"post" (cons "dependencies" rest))
           (dep:post session rest bindings raw-bindings)]
          [(list #"get" (list "dependencies" (? basic-ct-id? assignment-id)))
           (dep:assignment-dependencies session assignment-id '())]
          [(list #"get" (list "dependencies" (? basic-ct-id? assignment-id) "three-study"))
           (dep:three-study-form session assignment-id)]
          [(list #"get" (list "dependencies" (? basic-ct-id? assignment) (? basic-ct-id? step) (? basic-ct-id? review-id) rest ...))
           (dep:dependencies-form session assignment step review-id rest)]
          
          [(list #"post" (list "submit" (? basic-ct-id? assignment) (? basic-ct-id? step)))
           ;; used to submit files and to publish them (POST only)
           ;; FIXME I think the "action" binding should instead just be
           ;; implemented using a different URL.
           ;; FIXME get rid of this use of response/xexpr
           (response/xexpr
            (submit:submit session assignment step bindings raw-bindings))]

          ;; "/feedback/..."
          ;; viewing and submitting feedback? And other stuff? confused.
          ;; seems to be the main entry point to an assignment. More of
          ;; a dashboard?
          [(list #"post" (cons "feedback" rest))
           ;; POST /feedback/*/<hash>/... ((feedback . *) ...) : submit feedback
           ;; POST /feedback/file-container/<hash>/load : load or save rubric json data
           ;; POST /feedback/view/... (!feedback) ?
           (feedback:post session user-role rest bindings post-data)]
          [(list #"get" (list "feedback" "view" path-elts ...))
           ;; FIXME only a hash allowed?
           ;; view a review
           (feedback:do-view session path-elts '())]
          [(list #"get" (list "feedback" "file-container" hash path-elts ...))
           ;; a file-container page (goes in the iframe of a review)
           (feedback:do-file-container session user-role hash path-elts)]
          [(list #"get" (list "feedback" (? basic-ct-id? assignment)))
           ;; kind of an assignment dashboard:
           (feedback:do-default session assignment)]
          
          [(list #"get" (cons "export" rest))
           ;; return a file representing the status(?) of an assignment
           (export:load session user-role rest)]
          
          [(list _ (cons "exception" rest))
           ;; simulate throwing of a server exception.
           (error "Test an exception occurring.")]
          
          [(list #"post" (cons "roster" rest))
           ;; must include "action" binding:
           (roster:post post-data bindings session user-role rest)]
          [(list #"get" (cons "roster" rest))
           (roster:load session user-role rest)]
          
          ;; "/browse/..."
          [(list #"get" (list "browse" assignment step rest ...))
           ;; the iframe that allows the user to inspect just-submitted files
           (browse:do-file-container session assignment step rest)]
          
          ;; looks like a WIP moving all of dispatch to typed racket?
          [else (typed:handlerPrime post? post-data session user-role bindings raw-bindings path)]))))

;; for superusers: re-issue the request as though it came from user 'uid'
(define (with-sudo post post-data new-uid session user-role bindings raw-bindings path)
  (define can-sudo? (roles:Record-can-edit user-role))
  (when (not can-sudo?)
    (raise-403-not-authorized))
  (define new-session (ct-session (ct-session-class session)
                                  new-uid
                                  (ct-session-uid session)
                                  (ct-session-table session)))
  (handlerPrime post post-data new-session bindings raw-bindings path))

;; Returns #f if the session is not valid
;; otherwise returns a role-record
(define (session->role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session)))
    (cond [(role:exists? class uid) (role:select class uid)]
          [else #f])))

(module+ test
  (require rackunit
           racket/promise
           "testing/test-configuration.rkt")

  (current-configuration test-conf)

  ;; copied from docs.
  (define (url->request method u)
    (make-request method (string->url u) empty
                  (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

  #;(check-not-exn
   (λ () (ct-rules-2 (url->request #"GET" "http://example.com/"))))
  #;(check-not-exn
   (λ () (ct-rules-2 (url->request #"POST" "http://example.com/"))))

  (define (zz req args)
    args)

  (define-values (test-rules test-maker)
    (dispatch-rules
     [((string-arg) ...) zz]))
  (check-equal?
   (test-rules (url->request #"GET" "http://example.com/abc%20def/"))
   (list "abc def" "")))