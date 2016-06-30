#lang racket/base

(require web-server/http/response-structs)

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in error: "errors.rkt"))

(provide load)
(define (load session role rest)
  (let ((assignment-id (car rest)))
    (if (not (roles:Record-can-edit role))
        (error:not-authorized-response)
        (let ((data (export-assignment (class-name) assignment-id)))
          (response/full
           200 #"Okay"
           (current-seconds) #"application/octet-stream; charset=ISO-8859-1"
           '()
           (list data))))))