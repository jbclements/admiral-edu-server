#lang racket

(require "../base.rkt"
         "../pages/typed-xml.rkt"
         "../pages/errors.rkt")

(require (prefix-in error: "../pages/errors.rkt"))
               ;[error:not-authorized (-> Any)])


;(: role (ct-session -> roles:Record))
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))

;; if not authorized, signal error.
(provide check-can-edit)
;(: check-can-edit (ct-session -> Void))
(define (check-can-edit session)
  (when (not (can-edit? session))
    (raise-403-not-authorized)))

(provide can-edit?)
;(: can-edit? (ct-session -> Boolean))
(define (can-edit? session)
  (let ((session-role (role session)))
    (roles:Record-can-edit session-role)))