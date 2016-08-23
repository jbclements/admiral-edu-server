#lang racket/base

;; this file exists to deal with the API differences between
;; old and new server.

(require "../pages/errors.rkt"
         "../storage/storage.rkt"
         "../base.rkt"
         racket/file)

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

(define (delete-local-files-shim)
  (when (directory-exists? (class-name-shim))
    (fprintf (current-error-port)
             "ALERT: DELETING EXISTING CLASS DIRECTORY.\n")
    (delete-directory/files (class-name-shim))))

(define (list-files-shim path)
  (list-files path))

(define (ct-session-shim a b c d)
  (ct-session a b d))

;; should we ignore the bad-in-original tests?
(define ignore-bad-in-original? #t)