#lang info

(define name "Admiral Edu Server")

(define compile-omit-paths '("testing/splitting-zap-data.rkt"))
(define test-omit-paths '("testing/splitting-zap-data.rkt"))
#;(define scribblings '(("molis-hai.scrbl" () (tool))))

#;(define raco-commands
  (list
   (list "molis-hai"
         "cmd-line.rkt"
         "generate secure passwords using a source text"
         #f)))