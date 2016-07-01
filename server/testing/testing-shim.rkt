#lang racket/base

;; this file exists to deal with the API differences between
;; old and new server.

(require "../configuration.rkt"
         "../pages/errors.rkt"
         "../base.rkt")

(provide (all-defined-out))

(define server-error-shim
  exception-occurred)

(define (master-user-shim)
  master-user)

(define (class-name-shim)
  class-name)

(define (init-shim)
  ;(current-configuration test-conf)

  ;; start with a fresh database
  (force-initialize))
 