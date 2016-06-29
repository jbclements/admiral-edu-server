#lang racket/base

(module+ main
  (require web-server/servlet-dispatch
           web-server/web-server
           "../dispatch.rkt"
           "../base.rkt"
           "../storage/storage-basic.rkt"
           "../util/config-file-reader.rkt"
           net/url
           "test-configuration.rkt")
  
  (current-configuration test-conf)
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))
  
  (define stop
    (serve #:dispatch (dispatch/servlet ct-rules)
           #:port (ct-port)))

  (sleep 1)
  (printf "server is running.\n")
  
  (define-values (status headers result-port)
    (http-sendrecv/url
     (string->url "http://localhost:8080/roster")
     #:headers '("oidc_claim_email: masteruser@example.com")))
  (printf "~v\n~v\n~v"
          status
          headers
          (regexp-match #px".*" result-port))
  
  
  
  stop)
