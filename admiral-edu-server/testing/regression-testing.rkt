#lang racket/base

(module+ main
  (require racket/string
           racket/struct
           web-server/servlet-dispatch
           web-server/http/response-structs
           web-server/web-server
           
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
  
  (define (run-request user path)
    (let* ((raw-bindings '() #;(request-bindings/raw req))
           (bindings '() #;(request-bindings req))
           (post-data #"" #;(request-post-data/raw req))
           (start-rel-url (ensure-trailing-slash (string-append "/" (class-name) "/" (string-join path "/"))))
           (session (ct-session (class-name) (master-user) (make-table start-rel-url bindings)))
           (result (with-handlers ([(λ (x) #t) error:server-error-response])
                     (handlerPrime #f post-data session bindings raw-bindings path))))
      (list user path (explode-response result))))

  (define m (master-user))
  (define requests
    `((,m ())
      (,m ("assignments"))
      (,m ("roster"))))

  
  (for ([r (in-list requests)])
    (write (apply run-request r))
    (newline))
  )


