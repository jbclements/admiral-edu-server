#lang racket/base

(require web-server/servlet
         web-server/dispatch
         racket/date
         racket/string
         racket/match
         racket/list)

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
         (prefix-in assignments: "pages/assignments.rkt")
         (prefix-in export: "pages/export.rkt")
         (prefix-in submit: "pages/submit.rkt")
         (prefix-in dep: "pages/dependencies.rkt")
         (prefix-in feedback: "pages/feedback.rkt")
         (prefix-in roster: "pages/roster.rkt")
         (prefix-in browse: "pages/browse.rkt")
         (prefix-in typed: "dispatch-typed.rkt"))

(define (any? x)
  #t)

;; Defines how to process incoming requests
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [((string-arg) ...) (handler #f)]
   [((string-arg) ...) #:method "post" (handler #t)]
   [else error:four-oh-four-response]))

;; given whether this is a POST, handles an incoming request, returns a response
(define (handler post?)
  (lambda (req path)
    (log-ct-access-info/nomacro
     (format
      "[~a] ~a - ~a ~a" (date->string (current-date) #t)
      (class-name) (if post? "POST" "GET") path))
    (match (req->uid req)
      [(? string? uid-str)
       (let* ((raw-bindings (request-bindings/raw req))
              (bindings (request-bindings req))
              (post-data (request-post-data/raw req))
              (clean-path (filter (lambda (x) (not (equal? "" x))) path))
              (start-rel-url (ensure-trailing-slash (string-append "/" (class-name) "/" (string-join path "/"))))
              (session (ct-session (class-name) uid-str (make-table start-rel-url bindings))))
         (with-handlers ([any? error:server-error-response])
                        (handlerPrime post? post-data session
                                      bindings raw-bindings clean-path)))]
      [else
       (error:error-xexprs->response
        `((p "missing authentication headers."))
        400 #"Bad Request")])))


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
    ;; FIXME compute the role up here. make sure this doesn't break things...
  (match path
    ;; "/"
    ['() (X1render session index)]
    ;; also "/" ?
    [(list "")
     (X1render session index)]
    ;; "/review/..."
    [(cons "review" rest)
     (cond [post? (review:post->review session post-data rest)]                                
           [else (render session review:load rest)])]
    ;; "/file-container/..."
    [(cons "file-container" rest) (cond [post? (review:push->file-container session post-data rest)]
                                        [(and (> (length rest) 1)
                                              (string=? "download" (list-ref rest (- (length rest) 2)))) (render session review:check-download rest)]
                                        [(render session review:file-container rest)])]
    ;; "/su/uid/..."
    ;; interface for executing a command as another user
    [(cons "su" (cons uid rest))
     (with-sudo post? post-data uid session bindings raw-bindings rest)]
    ;; "/author/..."
    ;; interface for adding, editing assignments
    [(cons "author" rest)
     (if post?
         ;; POST
         (author:post->validate session post-data rest)
         ;; GET
         (render session author:load rest))]
    ;; "/next/..."
    [(cons "next" rest) (render session next rest)]
    ;; "/dependencies/..."
    [(cons "dependencies" rest)
     (if post?
         (dep:post session rest bindings raw-bindings)
         (render session dep:dependencies rest))]
    ;; "/submit/..."
    [(cons "submit" rest)
     (if post?
         (submit:submit session role rest bindings raw-bindings)
         (raise-400-bad-request "You've accessed this page in an invalid way."))]
    ;; "/feedback/..."
    [(cons "feedback" rest)
     (if post?
         (feedback:post session role rest bindings post-data)
         (render session feedback:load rest))]
    ;; "/export/..."
    [(cons "export" rest)
     ;; this one does not return a web page, but rather a file:
     (export:load session (role session) rest)]
    ;; "/exception/..."
    ;; simulate throwing of a server exception.
    [(cons "exception" rest) (error "Test an exception occurring.")]
    ;; "/roster/..."
    [(cons "roster" rest)
     (if post?
         ;; must include "action" binding:
         (render session (roster:post post-data bindings) rest)
         (render session roster:load rest))]
    ;; "/browse/..."
    [(cons "browse" rest)
     (cond [(and (> (length rest) 1)
                 (string=? "download" (list-ref rest (- (length rest) 2))))
            (render session browse:download rest)]
           [else (render session browse:load rest)])]
    ;; looks like a WIP moving all of dispatch to typed racket?
    [else (typed:handlerPrime post? post-data session bindings raw-bindings path)])))

(define (require-auth session f)
  (let* ((user-role (role session))
         (can-sudo (if user-role (roles:Record-can-edit user-role) #f)))
    (if can-sudo (f) (error:not-authorized-response))))

;; for superusers: re-issue the request as though it came from user 'uid'
(define (with-sudo post post-data uid session bindings raw-bindings path)
  (let* ((user-role (role session))
         (can-sudo (if user-role (roles:Record-can-edit user-role) #f))
         (new-session (ct-session (ct-session-class session) uid (ct-session-table session))))
    (if (not can-sudo) (error:four-oh-four-response)
        (handlerPrime post post-data new-session bindings raw-bindings path))))


(define (initialization session role [message '()])
  `(html
      (p "The service has been initialized to a fresh state.")))


;; Returns #f if the session is not valid
;; otherwise returns a role-record
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session)))
    (cond [(role:exists? class uid) (role:select class uid)]
          [else #f])))

;; If the session has a valid role, renders the specified page. Otherwise,
;; this displays an error message
(define (X1render session page)
  (match (role session)
    [#f (error:not-registered-response session)]
    [role (response/xexpr (page session role))]))

;; given a session and a page generation function
;; and the remainder of the path, check that the user
;; is registered and then call the given
;; function with the session, the role, and the path
;; remainder.
;; FIXME: should be two separate functions, I believe.
(define (render session page rest)
  (match (role session)
    [#f (error:not-registered-response session)]
    [role (render-hack (page session role rest))]))


;; If the session has a valid role, renders the specified page with
;; the specified bindings. 
;; Otherwise, this displays an error message
(define (post->render session page bindings)
  (match (role session)
    [#f (error:not-registered-response session)]
    [role (response/xexpr
           (page session role bindings))]))
