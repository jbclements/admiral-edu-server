#lang typed/racket/base

(require racket/string
         racket/list
         aws/s3
         aws/keys
         "../configuration.rkt"
         (prefix-in local: "local-storage.rkt"))

(require/typed aws/s3
               [s3-host (Parameterof String)]
               [ls (String -> (Listof String))]
               [put/file (String Path-String -> Any)]
               [get/file (String Path-String -> Void)]
               [delete (String -> String)])

(require/typed aws/keys
               [public-key (Parameterof String)]
               [private-key (Parameterof String)])

(provide retrieve-file
         retrieve-file-bytes
         write-file
         delete-path
         path-info
         list-files
         list-dirs
         list-sub-files
         startup-check)

;; set the aws parameters before running:
(define-syntax run-with-aws-parameters
  (syntax-rules ()
    [(_ exp) (run-with-aws-parameters/proc (λ () exp))]))

(: run-with-aws-parameters/proc (All (T) ((-> T) -> T)))
(define (run-with-aws-parameters/proc proc)
  (parameterize ([s3-host (cloud-host)]
                 [public-key (cloud-access-key-id)]
                 [private-key (cloud-secret-key)])
    (proc)))

(: ls/aws (String -> (Listof String)))
(define (ls/aws path) (run-with-aws-parameters (ls path)))
(: put/file/aws (String Path-String -> Any))
(define (put/file/aws a b) (run-with-aws-parameters (put/file a b)))
(: get/file/aws (String Path-String -> Void))
(define (get/file/aws a b) (run-with-aws-parameters (get/file a b)))
(: delete/aws (String -> String))
(define (delete/aws a) (run-with-aws-parameters (delete a)))

;; If the file exists locally, it returns it. Otherwise, it
;; fetches it from the cloud and then returns it as a string
(: retrieve-file (Path-String -> String))
(define (retrieve-file path)
  (unless (relative-path? path)
    (raise-argument-error 'retrieve-file
                          "relative path"
                          0 path))
  (when (eq? (local:path-info path) 'does-not-exist)
    (fetch-file path))
  (local:retrieve-file path))

;; If the file exists locally, it returns it. Otherwise, it
;; fetches it from the cloud and then returns it as a byte-string
(: retrieve-file-bytes (Path-String -> Bytes))
(define (retrieve-file-bytes path)
  (unless (relative-path? path)
    (raise-argument-error 'retrieve-file-bytes
                          "relative path"
                          0 path))
  (when (eq? (local:path-info path) 'does-not-exist)
    (fetch-file path))
  (local:retrieve-file-bytes path))

;; given a path-string, fetches a file from AWS to the local file
;; cache.
(: fetch-file (Path-String -> Void))
(define (fetch-file path)
  (unless (relative-path? path)
    (raise-argument-error 'fetch-file
                          "relative path"
                          0 path))
  (define path-str (local:path-string->string path))
  (local:ensure-path-exists path)
  (printf "Syncing bucket ~a and path ~a \n"
          (bucket) path-str)
  (get/file/aws (string-append (bucket) path-str)
                (build-path (local-storage-path) path))
  (printf "Done.\n"))

;; FIXME the use of clean on the local file path as well freaks me out...
;; rebooting into cloud mode will yield different file names (though
;; presumably the old FS will have been trashed anyway)
; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
(: write-file (Path-String Bytes -> Void))
(define (write-file path contents)
  (let ((clean-path (clean (local:path-string->string path))))
    (local:write-file clean-path contents)
    (let ((bucket+path (string-append (bucket) clean-path))
          (pathname (string->path clean-path)))
      (put/file/aws bucket+path (build-path (local-storage-path)
                                            pathname)))
    (void)))

;; FIXME this will cause collisions! use uri-encoding instead...
;; Conversts all spaces to underscores. This is a hack but the (put/file API
;; dies when you pass in a path with a space. This bug has been reported here:
;; https://github.com/greghendershott/aws/issues/35
;; ... though it's very unclear whether this is actually still a bug,
;; and whether google cloud storage has this issue.
(: clean (String -> String))
(define (clean to-clean)
  (let* ((string-list (string->list to-clean))
         (to-safe (lambda ([c : Char]) (cond [(eq? #\space c) #\_]
                                    [else c]))))
    (list->string (map to-safe string-list))))
  
; Deletes the local copy and the remote copy
(: delete-path (Path-String -> Void))
(define (delete-path path)
  (let* ((files (ls/aws (string-append (bucket)
                                       (local:path-string->string
                                        path))))
         (delete-f (lambda ([p : String])
                     (delete/aws (string-append (bucket) p)))))
    (map delete-f files))
  (local:delete-path path))

; (path -> Either 'file 'directory 'does-not-exist)
; returns 'file if the path exists, otherwise
;; returns 'directory'. Baby Yikes.
(: path-info (Path-String -> (U 'file 'directory)))
(define (path-info path)
  (cond [(file-exists-in-cloud? path) 'file]
        [else 'directory]))

;; determines whether a path exists in the cloud storage.
;; FIXME I'm confused by this function. This function suggests
;; that the 'ls' function can accurately perform directory-like
;; listing operations, which makes it very strange that
;; the 'path-info' function doesn't distinguish between directories
;; and 'does-not-exist. Experimentation with AWS required here.
(: file-exists-in-cloud? (Path-String -> Boolean))
(define (file-exists-in-cloud? path)
  (define path-str (cond [(path? path) (path->string path)]
                         [else path]))
  (let* ((files (ls/aws (string-append (bucket) path-str)))
         (member? (filter (lambda (x) (equal? path-str x)) files)))
    (= (length member?) 1)))

; (path -> (listof path))
; Returns all files that are at the specified path.
(: list-files (Path-String -> (Listof String)))
(define (list-files path)
  (define path-str (cond [(path? path) (path->string path)]
                         [else path]))
  (printf "Listing files at ~a\n" path-str)
  ;; FIXME terrible string manipulation
  (let* ((files (ls/aws (string-append (bucket) path-str)))
         (split-path (string-split path-str "/"))
         (split : (String -> (Listof String))
                (lambda ([x : String]) (string-split x "/")))
         (split-files (map split files))
         (at-len (length split-path))
         (at-path (map (inst last String)
                       (filter
                        (lambda ([x : (Listof String)])
                          (= (length x) (+ at-len 1)))
                        split-files))))
    (printf "at-path:~a\n" at-path)
    at-path))

; (path -> (listof path))
; Returns all directories that are at the specified path.
(: list-dirs (Path-String -> (Listof String)))
(define (list-dirs path)
  (define path-str (local:path-string->string path))
  (let* ((len (string-length path-str))
         (lc (if (= len 0) "" (string-ref path-str (- len 1))))
         (pathPrime (if (eq? #\/ lc) path-str
                        (string-append path-str "/")))
         (result (filter (lambda ([p : String])
                           (is-directory? (string-append pathPrime p))) (list-path pathPrime))))
    result))

;; FIXME completely copied from list-files...
(: list-path (Path-String -> (Listof String)))
(define (list-path path)
  (define path-str (local:path-string->string
                    path))
  (let* ((files (ls/aws (string-append (bucket) path-str)))
         (split-path (string-split path-str "/"))
         (split (lambda ([x : String]) (string-split x "/")))
         (split-files (map split files))
         (at-len (length split-path))
         (at-path (map (inst last String)
                       (remove-duplicates
                        (map (lambda ([x : (Listof String)])
                               (take-up-to x (cast (+ at-len 1) Index)))
                             split-files)))))
    at-path))

(: take-up-to (All (T) ((Listof T) Index -> (Listof T))))
(define (take-up-to ls n)
  (cond [(> (- (length ls) n) 0) (take ls n)]
        [else ls]))

;; FIXME it appears to me that this will return
;; true for nonexistent paths
(: is-directory? (Path-String -> Boolean))
(define (is-directory? path)
  (eq? 'directory (path-info path)))

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
(: list-sub-files (Path-String -> (Listof String)))
(define (list-sub-files path)
  (unless (relative-path? path)
    (raise-argument-error 'list-sub-files
                          "relative path"
                          0 path))
  (ls/aws (string-append (bucket) (local:path-string->string path))))

;; check that it's possible to ls at least before starting up
(: startup-check (-> Void))
(define (startup-check)
  (list-dirs "/")
  (void))