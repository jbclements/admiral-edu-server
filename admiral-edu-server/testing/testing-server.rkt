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
  
  (printf "server is running.\n")
  
  (display
   (http-sendrecv/url "http://localhost:8080/index.html"))
  (newline)
  
  stop)
