#lang racket/base

;; this file exists to deal with the API differences between
;; old and new server.

(require "./test-configuration.rkt"
         "../pages/errors.rkt"
         "../base.rkt"
         racket/file)

(provide (all-defined-out))

(define server-error-shim
  server-error-response)

(define (master-user-shim)
  (master-user))

(define (class-name-shim)
  (class-name))

(define (init-shim)
  (current-configuration test-conf)

  ;; start with a fresh database
  (db-init))

(define (delete-local-files-shim)
  (when (directory-exists? (build-path (local-storage-path) (class-name-shim)))
    (fprintf (current-error-port)
             "ALERT: DELETING EXISTING CLASS DIRECTORY.\n")
    (delete-directory/files (build-path (local-storage-path) (class-name-shim)))))
 