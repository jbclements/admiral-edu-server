#lang typed/racket/base

(provide (struct-out Record))
(struct: Record ([id : Integer] [name : String] [can-edit : Boolean]) #:transparent)
