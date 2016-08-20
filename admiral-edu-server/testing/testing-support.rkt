#lang racket

(require web-server/http/request-structs
         racket/contract)

(provide (contract-out
          [spec->raw-bindings
           (-> binding-spec/c (listof binding?))]
          [spec->bindings
           (-> binding-spec/c (listof (cons/c symbol?
                                              (or/c string?
                                                    bytes?))))]
          [append-binding-specs
           (-> binding-spec/c binding-spec/c binding-spec/c)])
         binding-spec/c)

;; there are some fairly complex invariants relating the raw bindings,
;; the bindings (though we shouldn't be using these at all), and the post
;; data. We use a binding-spec that can be mapped to both bindings and
;; raw-bindings (and post data, in the case of the json)

(define binding-spec/c
  (or/c '()
        'empty
        (list/c 'alist (listof (cons/c symbol? string?)))
        (list/c 'multipart (listof
                            (or/c (list/c 'nameandvalue bytes? bytes?)
                                  (list/c 'namefilevalue bytes? bytes? (listof header?) bytes?))))
        (list/c 'json bytes?)))

;; map a binding spec to the "raw bindings" format
(define (spec->raw-bindings binding-spec)
  (match binding-spec
    ['empty (list)]
    ['() (list)]
    [(list 'multipart file-bindings)
     (for/list ([b (in-list (filter (λ (x) x) file-bindings))])
       (match b
         [(list 'namefilevalue label filename headers content)
          (binding:file label
                        filename
                        headers
                        content)]
         [(list 'nameandvalue name value)
          (binding:form name value)]))]
    [(list 'alist alist)
     (for/list ([b (in-list alist)])
       (binding:form (string->bytes/utf-8 (symbol->string (car b)))
                     (string->bytes/utf-8 (cdr b))))]
    [(list 'json bytes)
     ;; bindings are ignored for these
     '()]
    [other
     (error 'unexpected-spec-shape
            "~e" other)]))


;; map a binding spec to the "bindings" form
;; SHOULDN'T USE ORDINARY BINDINGS AT ALL....
(define (spec->bindings binding-spec)
  (match binding-spec
    ['() '()]
    ['empty '()]
    [(list 'multipart file-bindings)
     (for/list ([b (in-list file-bindings)])
       (match b
         [(list 'namefilevalue label filename headers content)
          (cons (string->symbol (bytes->string/utf-8 label))
                content)]
         [(list 'nameandvalue name value)
          (cons (string->symbol (bytes->string/utf-8 name))
                (bytes->string/utf-8 value))]))]
    [(list 'alist alist)
     alist]
    [(list 'json bytes)
     ;; bindings are ignored for these
     '()]
    [other
     (error 'unexpected-spec-shape
            "~e" other)]))

;; combine two sets of binding specs
;; only has to work for first one being 'empty or 'alist and
;; second one 'alist or 'multipart
(define (append-binding-specs a b)
  (match a
    ['() b]
    ['empty b]
    [(list 'alist a-bindings)
     (match b
       [(list 'alist b-bindings)
        (list 'alist (append a-bindings b-bindings))]
       [(list 'multipart b-bindings)
        (list 'multipart (append (map alistpr->nameandvalue a-bindings)
                                 b-bindings))]
       [other (error 'append-binding-specs
                     "unexpected form for b bindings: ~e"
                     b)])]
    [other
     (error 'append-bindings
            "unexpected form for a bindings: ~e"
            a)]))

(define alistpr->nameandvalue
  (λ (pr)
    (list 'nameandvalue
          (string->bytes/utf-8 (symbol->string (car pr)))
          (string->bytes/utf-8 (cdr pr)))))

(module+ test
(require rackunit)
(check-equal?
   (spec->bindings '(multipart
                     ((namefilevalue #"file-1" #"file-1" () "abcd")
                      (namefilevalue #"file-2" #"grogra-2" () "efgh"))))
   ;; should probably be byte strings...
   '((file-1 . "abcd")
     (file-2 . "efgh")))
  (check-equal?
   (spec->raw-bindings
    '(multipart
                     ((namefilevalue #"file-1" #"file-1" () #"abcd")
                      (namefilevalue #"file-2" #"grogra-2" () #"efgh"))))
   (list
    (binding:file #"file-1"
                  #"file-1"
                  '()
                  #"abcd")
    (binding:file #"file-2"
                  #"grogra-2"
                  '()
                  #"efgh")))

  (check-equal?
   (spec->bindings '(alist
                     ((action . "create-student")
                      (uid . "footle@boo"))))
   '((action . "create-student")
                     (uid . "footle@boo")))
  (check-equal?
   (spec->raw-bindings '(alist ((action . "create-student")
                     (uid . "footle@boo"))))
   (list
    (binding:form #"action" #"create-student")
    (binding:form #"uid" #"footle@boo"))))