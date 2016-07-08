#lang racket/base

;; this file is ... it's not even really regression testing; it's
;; just about coverage. It uses a sequence of actions scraped
;; from a ZAP session.

(module+ test
  (require racket/string
           racket/list
           racket/match
           racket/runtime-path
           racket/file
           net/uri-codec
           web-server/http/response-structs
           web-server/http/request-structs
           rackunit
           rackunit/text-ui
           "../dispatch.rkt"
           "../base.rkt"
           "testing-shim.rkt"
           "testing-back-doors.rkt")

  (define-runtime-path here ".")

  (init-shim)

  (when (directory-exists? (class-name-shim))
    (fprintf (current-error-port)
             "ALERT: DELETING EXISTING CLASS DIRECTORY.\n")
    (delete-directory/files (class-name-shim)))
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))



  ;; translate a zap action into the spec expected
  ;; by the test engine
  (define ((zap->spec user assignment) zap-datum)
    (match zap-datum
      [(cons #f rest) #f]
      [(list response-code (list #"GET" url))
       (define spec-path (zap-path->pathlist url))
       (cond [(path-contains-hash? spec-path)
              (list response-code
                    (λ ()
                      (list user
                            (patch-path spec-path (cons assignment user)))))]
             [else
              `(,response-code (,user ,spec-path))])]
      [(list response-code (list #"POST" url) content-type bytes)
       (define spec-path (zap-path->pathlist url))
       (define binding-spec (bytes->binding-spec
                             content-type bytes))
       (cond [(path-contains-hash? spec-path)
              `(,response-code
                ,(λ ()
                   (list user
                         (patch-path spec-path (cons assignment user))
                         binding-spec
                         #t
                         bytes)))]
             [else
              `(,response-code (,user
                                ,spec-path
                                ,binding-spec
                                #t
                                ,bytes))])]))

  ;; does this path contain an element of the form <HASHn> ?
  (define (path-contains-hash? p)
    (not (not (ormap (λ (elt) (regexp-match #px"^<HASHV?[0-9]+>$" elt)) p))))

  (check-true (path-contains-hash? '("review" "<HASH1>")))

  
  
  ;; translate a URL into a list of strings
  (define (zap-path->pathlist url)
    (match (regexp-match
            #px#"^https://www\\.captainteach\\.org/2166-dev/(.*)$"
            url)
      [(list _ rel)
       (match (regexp-split #px"/" (bytes->string/utf-8 rel))
         [(list elts ... "") elts]
         [other other])]
      [#f (error 'bad-path "path: ~e\n" url)]))

  ;; convert a byte-string into a binding-spec. what a mess.
  (define (bytes->binding-spec content-type bytes)
    (match content-type
      [#"Content-Type: application/x-www-form-urlencoded"
       (list 'alist
             (form-urlencoded->alist (bytes->string/utf-8 bytes)))]
      [(regexp #px#"^Content-Type: multipart/form-data; boundary=(.*)$"
               (list m boundary))
       (define parts
         (regexp-split (byte-regexp (regexp-quote boundary))
                     bytes))
       (define specs
         (for/list ([p (in-list parts)])
           (define lines (regexp-split #px#"\r\n" p))
           (match lines
             [(list #""
                    (regexp #"^Content-Disposition: form-data; name=\"([^\"]+)\""
                            (list _ name))
                    #""
                    value
                    #"--")
              (list 'nameandvalue name value)]
             [(list #""
                    (regexp
                     #"^Content-Disposition: form-data; name=\"([^\"]+)\"; filename=\"([^\"]*)\""
                     (list _ name filename))
                    (regexp
                     #"^(Content-Type): (application/octet-stream|text/plain)"
                     (list _ ct-tag ct-val))
                    #""
                    content-strs ...
                    #"--")
              (list 'namefilevalue name filename
                    (list (header ct-tag ct-val))
                    (apply bytes-append
                           (add-between content-strs "\r\n")))]
             [(list #"--") #f]
             [(list #"--" #"") #f]
             [other (list 'giving-up lines)])))
       (list 'multipart
             specs)]
      [#"Content-Type: application/json; charset=UTF-8"
       (list 'json (bytes->string/utf-8 bytes))]))



  ;; copied from dispatch.rkt
  (define (ensure-trailing-slash candidate)
    (let ((len (string-length candidate)))
      (cond [(= 0 len) "/"]
            [else (let ((last-char (string-ref candidate (- len 1))))
                    (cond [(eq? #\/ last-char) candidate]
                          [else (string-append candidate "/")]))])))
  
  
  (define (explode-response r)
    (cond
      [(response? r)
       (list (response-code r)
             (response-message r)
             (response-seconds r)
             (response-mime r)
             (response-headers r)
             (let ([os (open-output-string)])
               ((response-output r) os)
               (get-output-string os)))]
      [else
       (list 'not-a-response-at-all r)]))
  
  
  (define (run-request user path [binding-spec 'empty]
                       [post? #f] [post-data #""])
    (let* ([bindings (spec->bindings binding-spec)]
           (raw-bindings (spec->raw-bindings binding-spec))
           (start-rel-url (ensure-trailing-slash (string-append "/" (class-name-shim) "/" (string-join path "/"))))
           (session (ct-session (class-name-shim) user (make-table start-rel-url bindings)))
           (result (with-handlers ([(λ (x) #t) server-error-shim])
                     (handlerPrime post? post-data session bindings raw-bindings path))))
      (explode-response result)))
  
  
  ;; there are some fairly complex invariants relating the raw bindings,
  ;; the bindings (though we shouldn't be using these at all), and the post
  ;; data. We use a binding-spec that can be mapped to both bindings and
  ;; raw-bindings. Also, we just give up on getting the post data right.
  ;; We'll implement that if we need it...
  
  (define (spec->raw-bindings binding-spec)
    (match binding-spec
      ['empty (list)]
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
  
  
  
  ;; SHOULDN'T USE ORDINARY BINDINGS AT ALL....
  (define (spec->bindings binding-spec)
    (match binding-spec
      ['empty (list)]
      [(list 'multipart file-bindings)
       (for/list ([b (in-list (filter (λ (x) x) file-bindings))])
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
  
  
  (define m (master-user-shim))
  (define stu1 "frogstar@example.com")
  (define stu2 "mf2@example.com")

  (define specs
    (for/list ([i (in-range 4)])
      (file->value (build-path here (string-append "zap-actions-"
                                                   (number->string (add1 i))
                                                   ".rktd")))))

  (define test-specs
    (filter
     (λ (x) x)
     (append
      (map (zap->spec m 'no-assignment) (list-ref specs 0))
      (map (zap->spec stu1 "a1-577be86f") (list-ref specs 1))
      (map (zap->spec stu2 "a1-577be86f") (list-ref specs 2))
      (map (zap->spec stu1 "a1-577be86f") (list-ref specs 3)))))

  
  
  (define REGRESSION-FILE-PATH
    (string-append "/tmp/zap-regression-results-"(number->string (current-seconds))".rktd"))
  
  (run-tests
   (test-suite
    "zap script coverage testing"
    (call-with-output-file REGRESSION-FILE-PATH
      (λ (r-port)
        (for ([test (in-list test-specs)]
              [i (in-naturals)])
          (match-define (list expected request-args-or-thunk) test)
          (define request-args
            ;; !@#$ request hashes... can't extract until earlier tests have been
            ;; run.
            (cond [(procedure? request-args-or-thunk) (request-args-or-thunk)]
                  [else request-args-or-thunk]))
          (define result (apply run-request request-args))
          (test-case
           (format "~s" (list i request-args))
           (match expected
             [(? number? code)
              (check-equal? (first result) code)]
             [(list (? number? code)
                    (? procedure? test-proc))
              (begin (check-equal? (first result) code)
                     (test-proc result))]))
          (define output-val (list i request-args result))
          (fprintf r-port "~s\n" output-val)
          (printf "~s\n" output-val))))))
  
  
  
    (sleep 1)
  )


