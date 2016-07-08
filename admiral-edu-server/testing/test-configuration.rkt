#lang typed/racket/base

(provide test-conf)

(define test-conf
  '#hash(("ct-port" . "8080")
         ("mail-enable" . "fAlSe")
         ("mail-port" . "2525")
         ("mail-server" . "smtp.sendgrid.net")
         ("server-name" . "yoursite.com")
         ("cloud-access-key-id" . "YOUR-ACCESS-ID")
         ("cloud-host" . "storage.googleapis.com")
         ("mail-username" . "username")
         ("bucket" . "some-bucket-name/")
         ("class-name" . "test-class")
         ("storage-mode" . "local")
         ("cloud-secret-key" . "YOUR-SECRET-KEY")
         ("sub-domain" . "www.")
         ("mail-password" . "password")
         ("db-name" . "captain_teach_testing")
         ("db-password" . "captain_teach")
         ("db-user-name" . "captain_teach")
         ("master-user" . "masteruser@example.com")
         ("db-address" . "localhost")))

(module+ test
  (require typed/rackunit
           "../configuration.rkt")
  
  ;; ensure that it's a legal configuration:
  (check-not-exn
   (λ () (parameterize ([current-configuration test-conf])
           1234))))