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

;; loads the testing configuration and initializes the database
(define (init-shim)
  (current-configuration test-conf)
  ;; start with a fresh database
  (db-init))

;; deletes the "local storage" directory and everything in it.
;; FIXME just call this from init-shim.
(define (delete-local-files-shim)
  (when (directory-exists? (build-path (local-storage-path) (class-name-shim)))
    (printf "ALERT: DELETING EXISTING CLASS DIRECTORY.\n")
    (delete-directory/files (build-path (local-storage-path) (class-name-shim)))))
 