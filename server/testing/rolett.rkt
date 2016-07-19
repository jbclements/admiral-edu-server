#lang typed/racket/base

(require (prefix-in roles: "rolestt.rkt"))


(provide (struct-out Record))
(struct: Record ([f : roles:Record]) #:transparent)

