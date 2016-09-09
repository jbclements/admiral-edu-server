#lang typed/racket/base

(require/typed racket/hash
               [hash-union ((HashTable String String)
                            (HashTable String String)
                            [#:combine (String String -> String)]
                            ->
                            (HashTable String String))])

(provide test-conf
         modified-test-conf)

(define test-conf
  (make-immutable-hash
   '(("ct-port" . "8080")
     ("mail-enable" . "fAlSe")
     ("mail-port" . "2525")
     ("mail-server" . "smtp.sendgrid.net")
     ("mail-username" . "username")
     ("server-name" . "yoursite.com")
     ("cloud-access-key-id" . "YOUR-ACCESS-ID")
     ("cloud-host" . "storage.googleapis.com")
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
     ("db-address" . "localhost")
     ("local-storage-path" . "/tmp/ct-storage")
     ("zip-binary" . "/usr/bin/zip")
     ("unzip-binary" . "/usr/bin/unzip")
     ("tar-binary" . "/usr/bin/tar"))))

(: modified-test-conf ((HashTable String String) -> (HashTable String String)))
(define (modified-test-conf table)
  (define correct-key-names (hash-keys test-conf))
  (for ([k : String (in-hash-keys table)])
    (unless (member k correct-key-names)
      (raise-argument-error
       'modified-test-conf
       (format "table containing valid key names (failed on '~a')"
               k)
       0 table)))
  (hash-union test-conf
              table
              #:combine (λ ([a : String] [b : String]) b)))



(module+ test
  (require typed/rackunit
           "../configuration.rkt")

  (check-equal?
   (hash-ref
    (modified-test-conf (hash "db-name" "grobbath"
                              "zip-binary" "/opt/local/bin/zip"))
    "db-name")
   "grobbath")

  (check-exn
   #px"table containing valid key names"
   (λ () (modified-test-conf (hash "db-XXX-name" "grobbath"
                                   "zip-binary" "/opt/local/bin/zip"))))
  
  ;; ensure that it's a legal configuration:
  (check-not-exn
   (λ () (parameterize ([current-configuration test-conf])
           1234))))