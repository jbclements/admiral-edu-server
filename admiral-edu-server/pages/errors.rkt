#lang typed/racket/base

(require typed/racket/date
         racket/list
         racket/string)
(require "../logging.rkt"
         "../ct-session.rkt"
         "typed-xml.rkt"
         "responses.rkt")
(require web-server/templates)

(require/typed "templates.rkt"
               [xexpr->error-page-html (XExpr -> String)]
               [xexprs->error-page-html ((Listof XExpr) -> String)])



;; TODO : Sam says submit PR to add this ...
;; very strange that this is necessary:
(require/typed typed/racket/base
               [srcloc->string (srcloc -> (U String False))])



(provide error-xexprs->response
         error-xexprs->400-response
         server-error-response
         not-authorized-response
         assignment-closed-response
         not-registered-response
         four-oh-four-response)

;; FIXME this module needs to define its role better; is it
;; internal, external... ? Some of these return error messages,
;; some return strings representing html pages containing error
;; messages, some return xexprs, some return responses. Yikes.

;; ... mostly cleaned up now, just a few more...


;; given a list of error xexpr (strings are fine too) and a code and
;; a status, return a response embedding that error. These aren't
;; necessarily server errors, they may be client errors, so no
;; logging is done here.

(: error-xexprs->response ((Listof XExpr) Natural Bytes -> Response))
(define (error-xexprs->response xexprs code status)
  (response/full
   code status (current-seconds) TEXT/HTML-MIME-TYPE
   '()
   (list (string->bytes/utf-8
          (xexprs->error-page-html xexprs)))))

;; convenience function to produce 400 errors
(: error-xexprs->400-response ((Listof XExpr) -> Response))
(define (error-xexprs->400-response xexprs)
  (error-xexprs->response xexprs 400 #"Bad Request"))

;; given the thing raised (maybe an exn, maybe not), log it and
;; return a response. This is for internal server errors.
(: server-error-response (Any -> Response))
(define (server-error-response exn)
  (log-ct-error-info
   "[~a] ERROR:"
   (date->string (current-date) #t))
  (if (exn? exn)
      (log-exception exn)
      (begin (log-ct-error-info "Caught non-exn: ~a\n" exn)))
  ;; TODO Recursively print out exception information
  ;; TODO Send email with exception output to self.
  (error-xexprs->response
    `(p "An error occurred while processing your request. "
        "This has been recorded. "
        "Please check with staff or try again later.")
    500
    #"Internal Server Error"))

;; log an exception to the error log
(: log-exception (exn -> Void))
(define (log-exception exn)
  (let* ((message (exn-message exn))
         (marks (exn-continuation-marks exn))
         (stack (if (continuation-mark-set? marks)
                    (continuation-mark-set->context marks) #f)))
    (log-ct-error-info "Caught Exception: ~e" exn)
    (log-ct-error-info "Message: ~a" message)
    (when stack (map print-stack-elem
                     ;; test this cast!
                     stack))
    (void)))

;; print stack elements to the error log
(: print-stack-elem ((Pairof (U False Symbol) Any) -> Void))
(define (print-stack-elem elem) 
  (let ((label (if (null? elem) "function-name???" (car elem)))
        (srcloc (if (and (not (null? elem)) (srcloc? (cdr elem)))
                    (srcloc->string (cdr elem)) "No Source Location")))
  (log-ct-error-info "~a - ~a" label srcloc)))

;; FIXME get rid of this function:
;; accepts an xexprs message. Returns a 403 response
(: not-authorized-response (String -> Response))
(define (not-authorized-response msg)
  (error-xexprs->response
   `((p ,msg)
     (p "You may need to "
       (a ((href "/authentication/redirect?logout=/logout.html"))
          "log out")
       " and log back in with a different account name."))
   403 #"Forbidden"))

;; FIXME get rid of this, replace with call to raise-400-bad-request
(: assignment-closed-response (-> Response))
(define (assignment-closed-response)
  (error-xexprs->400-response
   `((p "The assignment you were attempting to access is currently closed."))))

;; FIXME merge with existing exception model
(: not-registered-response (ct-session -> Response))
(define (not-registered-response session)
  (error-xexprs->response
   `((h1 "Error") 
     (p "You are not registered for this class.")
     (p " You may need to "
        (a ((href "/authentication/redirect?logout=/logout.html")) " logout")
        " and reauthenticate with the correct account. You are currently"
        " logged in with the information below. If this information is correct"
        " you should contact your instructor.")
     (p , (string-append "User ID: " (ct-session-uid session)))
     (p , (string-append "Class ID: " (ct-session-class session))))
   403 #"Forbidden"))

;; FIXME delete...
(provide error-invalid-session)
(define error-invalid-session
  `((h1 "An Error Occurred")
    (p "This session is not valid. Try to log out and then log in again.")))

;; FIXME delete...
(: four-oh-four-response (-> Response))
(define (four-oh-four-response)
  (error-xexprs->response
   '((h2 "404 - The resource does not exist"))
   404
   #"Not Found"))

