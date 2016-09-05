#lang racket/base

(require racket/string)

(provide lines)
(define (lines data)
  (string-split data "\n"))