#lang racket/base

(module+ main
  (require web-server/servlet-dispatch
           web-server/web-server
           "dispatch.rkt"
           "base.rkt"
           "paths.rkt"
           "storage/storage-basic.rkt"
           "util/config-file-reader.rkt")

  (define DEFAULT-CONFIG-PATH "/conf/captain-teach.config")

  (current-configuration (read-conf DEFAULT-CONFIG-PATH))

  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))

  (define stop
    (serve #:dispatch (dispatch/servlet ct-rules)
           #:port (ct-port)
           #:host #f))

  (print "Server Started. Type `stop` to kill the server.")
  (newline)
  (flush-output)

  (define (block)
    (let ((input (read)))
      (if (equal? 'stop input) (stop) (block))))

  (block))
