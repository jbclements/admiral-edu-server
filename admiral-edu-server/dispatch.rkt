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
  "logging.rkt"
  (only-in "pages/errors.rkt"
           error-xexprs->response)
  "temporary-hacks.rkt")

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
  (handler2 req #f path))

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
  (with-handlers ([exn:user-error?
                   (λ (exn)
                     (cond
                       [(= (exn:user-error-code exn) 403)
                        (error:not-authorized-response
                         (exn-message exn))]
                       [else
                        (error-xexprs->response
                         `((p ,(exn-message exn))
                           (p "Try returning to "
                              (a ((href ,(string-append "https://"
                                                        (sub-domain) (server-name) "/" (class-name))))
                                 "Class Home")
                              " and trying again."))
                         (exn:user-error-code exn)
                         (match (exn:user-error-code exn)
                           [400 #"Bad Request"]
                           [404 #"Not Found"]))]))])
    (define user-role (session->role session))
    (if (not user-role)
        (error:not-registered-response session)
        (match path
          ;; "/"
          ;; FIXME are both of these two actually possible?
          [(or '() '("")) (response/xexpr (index session user-role))]
          ;; "/download/hash/path..."
          ;; download the raw bytes of a file, based on hash and path.
          ;; used for reviewing and feedback.
          [(list "download" hash path-strs ...)
           (download:do-download session hash path-strs)]
          ;; "/review/..."
          [(cons "review" rest)
           ;; POST:
           ;; used by codemirror autosave and review elements
           ;; to save review.
           (cond [post? (review:post->review session post-data rest)]
                 ;; GET
                 ;; presents review screen, also handles click on review submit button
                 [else (render-hack (review:load session user-role rest))])]
          ;; "/file-container/..."
          [(cons "file-container" rest)
           (cond [post?
                  ;; save or load contents of review. bit of an abuse of POST.
                  (review:push->file-container session post-data rest)]
                 ;; FIXME move 'download' token to end of path or beginning...
                 ;; "/file-container/<hash>/...*/download/..."
                 [(and (> (length rest) 1)
                       ;; FIXME icky path hacking
                       (string=? "download" (list-ref rest (- (length rest) 2))))
                  (render-hack
                   (review:check-download session user-role rest))]
                 [(render-hack
                   (review:file-container session user-role rest))])]
          ;; "/su/uid/..."
          ;; interface for executing a command as another user
          [(cons "su" (cons uid rest))
           (with-sudo post? post-data uid session user-role bindings raw-bindings rest)]
          ;; "/author/..."
          ;; interface for adding, editing assignments
          [(cons "author" rest)
           (if post?
               ;; POST
               (author:post->validate session post-data rest)
               ;; GET
               (author:load session user-role rest))]
          ;; "/next/..."
          [(cons "next" rest)
           (render-hack (next session user-role rest))]
          ;; "/dependencies/..."
          [(cons "dependencies" rest)
           (if post?
               (dep:post session rest bindings raw-bindings)
               (render-hack (dep:dependencies session user-role rest)))]
          ;; "/submit/..."
          ;; used to submit files and to publish them (POST only)
          ;; FIXME I think the "action" binding should instead just be
          ;; implemented using a different URL.
          [(cons "submit" rest)
           (if post?
               ;; POST
               (response/xexpr (submit:submit session rest bindings raw-bindings))
               (raise-400-bad-request "You've accessed this page in an invalid way."))]
          ;; "/feedback/..."
          ;; viewing and submitting feedback? And other stuff? confused.
          ;; seems to be the main entry point to an assignment. More of
          ;; a dashboard?
          [(cons "feedback" rest)
           (if post?
               ;; POST /feedback/*/<hash>/... ((feedback . *) ...) : submit feedback
               ;; POST /feedback/file-container/<hash>/load : load or save rubric json data
               ;; POST /feedback/view/... (!feedback) ?
               (feedback:post session user-role rest bindings post-data)
               ;; GET /feedback/ : assignment dashboard ?
               ;; GET /feedback/view/<hash> : viewing a review
               ;; GET /feedback/file-container/<hash>/[...*] : file-container page
               (render-hack (feedback:load session user-role rest)))]
          ;; "/export/..."
          [(cons "export" rest)
           ;; this one does not return a web page, but rather a file:
           (export:load session user-role rest)]
          ;; "/exception/..."
          ;; simulate throwing of a server exception.
          [(cons "exception" rest)
           (error "Test an exception occurring.")]
          ;; "/roster/..."
          [(cons "roster" rest)
           (if post?
               ;; must include "action" binding:
               (render-hack ((roster:post post-data bindings) session user-role rest))
               (render-hack (roster:load session user-role rest)))]
          ;; "/browse/..."
          [(cons "browse" rest)
           (cond [(and (> (length rest) 1)
                       (string=? "download" (list-ref rest (- (length rest) 2))))
                  (render-hack (browse:download session user-role rest))]
                 [else (render-hack (browse:load session user-role rest))])]
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