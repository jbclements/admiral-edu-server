#lang typed/racket/base

;; all other provides generated automatically by macro...
(provide current-configuration)

;; a configuration is a hash table with the required fields
;; (specified below)
(: current-configuration (Parameterof (HashTable String String)))
(define current-configuration
  (make-parameter (ann (hash) (HashTable String String))
                  (λ ([t : (HashTable String String)])
                    (check-conf-hash t))))

;; given a spec (sadly must appear below macro definition),
;; define a structure containing the specified fields,
;; and a function to read those values from a given file.
(define-syntax define-configuration-file
  (syntax-rules ()
    [(_ checker [field ty] ...)
     (begin
       (begin
         (provide field)
         (: field (-> ty))
         (define (field)
           (maybe-convert
            (hash-ref (current-configuration)
                      (symbol->string (quote field)))
            ty)))
       ...
       (: checker ((HashTable String String) -> (HashTable String String)))
       (define (checker table)
         (let ([field-str (symbol->string (quote field))])
           (unless (hash-has-key? table field-str)
             (error 'check-conf-hash
                    "configuration missing required field: ~v"
                    field-str))
           (define field-val (hash-ref table field-str))
           (unless (check-ty field-val ty)
             (error 'check-configuration
                    "expected value for key ~v convertible to ~a, got: ~v"
                    field-str (quote ty) field-val)))
         ...
         ;; hacking in a path check here:
         (for ([pfield (in-list '(zip-binary unzip-binary tar-binary))])
           (define path (hash-ref table (symbol->string pfield)))
           (unless (file-exists? path)
             (error 'check-configuration
                    "expected path to existing file for field ~a, got: ~a"
                    pfield path))) 
         table))]))

(define-syntax maybe-convert
  (syntax-rules (String Natural Boolean)
    [(_ v String) v]
    [(_ v Natural) (cast (string->number v) Natural)]
    [(_ v Boolean) (cond [(member (string-downcase v)
                                  (list "true" "t")) #t]
                         [(member (string-downcase v)
                                  (list "false" "f")) #f]
                         [else (error 'maybe-convert
                                      "expected true/t/false/f, got: ~e"
                                      v)])]))

(define-syntax check-ty
  (syntax-rules (String Natural Boolean)
    [(_ v String) #t]
    [(_ v Natural) (exact-nonnegative-integer?
                    (string->number v))]
    [(_ v Boolean) (member (string-downcase v)
                           (list "true" "t" "false" "f"))]))

(define-configuration-file
  check-conf-hash
  [db-address String]
  [db-user-name String]
  [db-password String]
  [db-name String]
  [server-name String]
  [sub-domain String]
  [mail-enable Boolean]
  [mail-server String]
  [mail-port Natural]
  [mail-username String]
  [mail-password String]
  [storage-mode String]
  [bucket String]
  [cloud-access-key-id String]
  [cloud-secret-key String]
  [cloud-host String]
  [class-name String]
  [ct-port Natural]
  [local-storage-path String]
  [zip-binary String]
  [unzip-binary String]
  [tar-binary String]
  [master-user String])


(module+ test
  (require typed/rackunit
           racket/runtime-path)

  
  (define test-conf
    '#hash(("ct-port" . "8080")
           ("mail-port" . "2525")
           ("mail-server" . "smtp.sendgrid.net")
           ("server-name" . "yoursite.com")
           ("mail-username" . "username")
           ("cloud-access-key-id" . "YOUR-ACCESS-ID")
           ("cloud-host" . "storage.googleapis.com")
           ("bucket" . "some-bucket-name/")
           ("storage-mode" . "cloud-storage")
           ("cloud-secret-key" . "YOUR-SECRET-KEY")
           ("class-name" . "test-class")
           ("db-user-name" . "captain_teach")
           ("db-password" . "captain_teach")
           ("sub-domain" . "www.")
           ("mail-password" . "password")
           ("db-name" . "captain_teach")
           ("master-user" . "youremail@domain.com")
           ("db-address" . "localhost")
           ("mail-enable" . "fAlSe")
           ("local-storage-path" . "/tmp/ct-storage")
           ("zip-binary" . "/usr/bin/zip")
           ("unzip-binary" . "/usr/bin/unzip")
           ("tar-binary" . "/usr/bin/tar")))
  
  (check-equal? (check-conf-hash test-conf) test-conf)

  (check-exn #px"missing required field"
             (λ () (check-conf-hash
                    (hash-remove test-conf "db-name"))))

  (check-exn #px"convertible to Natural, got:"
             (λ () (check-conf-hash
                    (hash-set test-conf "mail-port" "22342a")))))

