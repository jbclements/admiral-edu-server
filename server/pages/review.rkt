#lang racket

(require web-server/http/bindings)
(require web-server/templates)
(require web-server/http/response-structs)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt")

(provide load)
(define (load session role [message '()])
  (include-template "html/review.html"))

(provide file-container)
(define (file-container session role [message '()])
  (include-template "html/file-container.html"))
