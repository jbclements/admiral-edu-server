#lang racket/base

(module+ main
  (require racket/string
           racket/struct
           racket/list
           web-server/servlet-dispatch
           web-server/http/response-structs
           web-server/web-server
           web-server/http/request-structs
           
           "../dispatch.rkt"
           (prefix-in error: "../pages/errors.rkt")
           "../base.rkt"
           "../storage/storage-basic.rkt"
           "../util/config-file-reader.rkt"
           net/url
           "test-configuration.rkt")
  
  (current-configuration test-conf)

  ;; start with a fresh database
  (db-init)
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))

  
  (define (ensure-trailing-slash candidate)
    (let ((len (string-length candidate)))
      (cond [(= 0 len) "/"]
            [else (let ((last-char (string-ref candidate (- len 1))))
                    (cond [(eq? #\/ last-char) candidate]
                          [else (string-append candidate "/")]))])))

 	 
  #;(struct response (code message seconds mime headers output))

  (define (explode-response r)
    (list (response-code r)
          (response-message r)
          (response-seconds r)
          (response-mime r)
          (response-headers r)
          (let ([os (open-output-string)])
            ((response-output r) os)
            (get-output-string os))))

  ;; SHOULDN'T USE ORDINARY BINDINGS AT ALL....
  (define (make-raw-bindings bindings)
    (for/list ([b (in-list bindings)])
      (binding:form (string->bytes/utf-8 (symbol->string (car b)))
                    (string->bytes/utf-8 (cdr b)))))
  
  (define (run-request user path [bindings '()] [post? #f] [post-data #""])
    (let* ((raw-bindings (make-raw-bindings bindings) #;(request-bindings/raw req))
           (start-rel-url (ensure-trailing-slash (string-append "/" (class-name) "/" (string-join path "/"))))
           (session (ct-session (class-name) (master-user) (make-table start-rel-url bindings)))
           (result (with-handlers ([(λ (x) #t) error:server-error-response])
                     (handlerPrime post? #"" session bindings raw-bindings path))))
      (explode-response result)))

  (define m (master-user))
  (define stu1 "frogstar@example.com")
  
  (define requests
    `((,m () ())
      (,m ("assignments") ())
      (,m ("roster") ())
      (,m ("roster" "new-student"))
      ;; should be a 400, not a 200:
      (,m ("roster" "new-student") () #t)
      (,m ("roster" "new-student") ((action . "create-student")
                                    (uid . ,stu1))
          #t)
      ;; create same student again? (shouldn't be 200 okay)
      (,m ("roster" "new-student") ((action . "create-student")
                                       (uid . ,stu1))
          #t)
      (,m ("author"))
      ;; ouch internal error!
      (,m ("author") () #t #"assignment-id : zzz1")
      ;; ouch! another internal error!
      (,m ("author" "bogwater") () #t #"assignment-id : zzz1")
      (,m ("author" "validate") () #t #"assignment-id : zzz1")))

  
  (for ([r (in-list requests)])
    (write (list r (apply run-request r)))
    (newline))

  (sleep 1)
  )


