#lang typed/racket/base

;; FIXME: basic-types is no longer a good name for this file...

;; FIXME: delete the Failure and Success structs and switch to exceptions
(provide (struct-out Failure))
(struct Failure ([message : String]) #:transparent)

(provide Result)
(define-type (Result A) (U (Success A) Failure))

(provide (struct-out Success))
(struct: (A) Success ([result : A]) #:transparent)

(provide failure)
(: failure (String * -> Failure))
(define (failure . messages)
  (Failure (apply string-append messages)))

(provide (struct-out exn:user-error)
         raise-400-bad-request
         raise-404-not-found
         raise-403-not-authorized)

;; represents a user error (400, 403, 404)
(struct exn:user-error exn ([code : Natural]) #:transparent)

(: raise-400-bad-request (String -> Nothing))
(define (raise-400-bad-request message)
  (raise (exn:user-error message (current-continuation-marks) 400)))

(: raise-404-not-found (->* () (String) Nothing))
(define (raise-404-not-found
         [message "Page not found."])
  (raise (exn:user-error message (current-continuation-marks) 404)))

;; accepts an optional message, 
(: raise-403-not-authorized (->* () (String) Nothing))
(define (raise-403-not-authorized
         [message
          "You are not authorized to access this page."])
  (raise (exn:user-error message (current-continuation-marks) 403)))

;; really need the exception monad here...

;;; wait! why not just use actual racket exceptions? This whole success/fail thing seems silly.

(provide sdo)
(define-syntax sdo
  (syntax-rules (<-)
    [(_ [name <- rhs]) rhs]
    [(_ rhs) rhs]
    [(_ [name <- rhs] rest ...)
     (let ([temp rhs])
       (cond [(Success? temp)
              (let ([name (Success-result temp)])
                (sdo rest ...))]
             [else temp]))]
    [(_ rhs rest ...)
     (let ([temp rhs])
       (cond [(Success? temp)
              (sdo rest ...)]
             [else temp]))]))

(provide do-map)
(: do-map (All (V) ((V -> (Result Void)) (Listof V) -> (Result Void))))
(define (do-map fun args)
  (cond [(null? args) (Success (void))]
        [else (sdo (fun (car args))
                   (do-map fun (cdr args)))]))


(module+ test
  (require typed/rackunit)

  (: num-tester (Integer -> (Result Void)))
  (define (num-tester x)
    (cond [(< x 10) (Success (void))]
          [else (Failure (format "number too large: ~v" x))]))
  
  (check-equal? (do-map num-tester '(3 4 1 8)) (Success (void)))
  (check-equal? (do-map num-tester '(3 14 11 8)) (Failure "number too large: 14")))
