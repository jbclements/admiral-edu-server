#lang typed/racket/base

(provide test-conf)

(define test-conf
  '#hash(("ct-port" . "8080")
         ("mail-port" . "2525")
         ("mail-server" . "smtp.sendgrid.net")
         ("server-name" . "yoursite.com")
         ("cloud-access-key-id" . "YOUR-ACCESS-ID")
         ("cloud-host" . "storage.googleapis.com")
         ("mail-username" . "username")
         ("bucket" . "some-bucket-name/")
         ("class-name" . "test-class")
         ("storage-mode" . "local")
         ("db-user-name" . "captain_teach")
         ("cloud-secret-key" . "YOUR-SECRET-KEY")
         ("db-password" . "captain_teach")
         ("sub-domain" . "www.")
         ("mail-password" . "password")
         ("db-name" . "captain_teach")
         ("master-user" . "masteruser@example.com")
         ("db-address" . "localhost")
         ("db-name" . "captain_teach_testing")))

(module+ test
  (require typed/rackunit
           "../configuration.rkt")
  
  ;; ensure that it's a legal configuration:
  (check-not-exn
   (λ () (parameterize ([current-configuration test-conf])
           1234))))