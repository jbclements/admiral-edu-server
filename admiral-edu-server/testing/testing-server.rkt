#lang racket/base

(define ((port-matching? regexp) port)
    (regexp-match regexp port))

(module+ main
  (require web-server/servlet-dispatch
           web-server/web-server
           "../dispatch.rkt"
           "../base.rkt"
           "../storage/storage-basic.rkt"
           "../util/config-file-reader.rkt"
           net/url
           "test-configuration.rkt"
           rackunit
           rackunit/text-ui)

  
  (current-configuration test-conf)
  (db-init)
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))
  
  (define stop
    (serve #:dispatch (dispatch/servlet ct-rules)
           #:port (ct-port)))

  (sleep 1)

  (run-tests
   (test-suite
    "server tests"

    (test-case
     "missing uid"
     (check-match
      (call-with-values
       (λ () (http-sendrecv/url
              (string->url "http://localhost:8080/index.html")))
       list)
      (list (regexp #px#"200 Okay") _
            (? (port-matching? #px"a")))))))


  (define-values (status headers result-port)
    (http-sendrecv/url
     (string->url "http://localhost:8080/index.html")
     #:headers '("oidc_claim_email: brongo@example.com")))
  (printf "~v\n~v\n~v"
          status
          headers
          (regexp-match #px".*" result-port))

  (sleep 1)
  (stop))
