#lang racket/base

(require racket/string
         racket/list
         racket/contract
         aws/s3
         aws/keys
         "../configuration.rkt"
         (prefix-in local: "local-storage.rkt"))

(provide (contract-out
          
          [retrieve-file
           (-> relative-path? string?)]
          [retrieve-file-bytes
           (-> relative-path? bytes?)])
         ;; FIXME attach contracts to all of these...
         write-file
         delete-path
         (contract-out
          [path-info
           (-> relative-path? (or/c 'file 'directory 'does-not-exist))]
          [list-files
           (-> relative-path? (listof string?))])
         list-dirs
         list-sub-files
         (contract-out
          [startup-check (-> void?)]))

;; set the aws parameters before running:
(define ((run-with-aws-parameters proc) . args)
  (parameterize ([s3-host (cloud-host)]
                 [public-key (cloud-access-key-id)]
                 [private-key (cloud-secret-key)])
    (apply proc args)))

(define ls/aws (run-with-aws-parameters ls))
(define put/file/aws (run-with-aws-parameters put/file))
(define get/file/aws (run-with-aws-parameters get/file))
(define delete/aws (run-with-aws-parameters delete))

;; If the file exists locally, it returns it. Otherwise, it
;; fetches it from the cloud and then returns it as a string
(define (retrieve-file path)
  (when (eq? (local:path-info path) 'does-not-exist)
    (fetch-file path))
  (local:retrieve-file path))

;; If the file exists locally, it returns it. Otherwise, it
;; fetches it from the cloud and then returns it as a byte-string
(define (retrieve-file-bytes path)
  (when (eq? (local:path-info path) 'does-not-exist)
    (fetch-file path))
  (local:retrieve-file-bytes path))

;; given a path-string, fetches a file from AWS
(define (fetch-file path)
  (define path-str (cond [(path? path) (path->string path)]
                         [else path]))
  (define path-path (cond [(path? path) path]
                          [else (string->path path)]))
  (local:ensure-path-exists path-path)
  (printf "Syncing bucket ~a and path ~a \n"
          (bucket) path-str)
  (get/file/aws (string-append (bucket) path-str)
                (build-path (local-storage-path) path-path))
  (printf "Done.\n"))


;; FIXME the use of clean on the local file path as well freaks me out...
;; rebooting into cloud mode will yield different file names (though
;; presumably the old FS will have been trashed anyway)
; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
(define (write-file path contents)
  (let ((clean-path (clean path)))
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
  (define (clean to-clean)
    (let* ((string-list (string->list to-clean))
           (to-safe (lambda (c) (cond [(eq? #\space c) #\_]
                                      [else c]))))
      (apply string (map to-safe string-list))))
  
  
  ; Deletes the local copy and the remote copy
  (define (delete-path path)
    (let* ((files (ls/aws (string-append (bucket) path)))
           (delete-f (lambda (p)
                       (delete/aws (string-append (bucket) p)))))
      (map delete-f files))
    (local:delete-path path))
  
; (path -> Either 'file 'directory 'does-not-exist)
; returns 'file if the path exists, otherwise
;; returns 'directory'. Baby Yikes.
(define (path-info path)
  (cond [(file-exists-in-cloud? path) 'file]
        [else 'directory]))

;; determines whether a path exists in the cloud storage.
;; FIXME I'm confused by this function. This function suggests
;; that the 'ls' function can accurately perform directory-like
;; listing operations, which makes it very strange that
;; the 'path-info' function doesn't distinguish between directories
;; and 'does-not-exist. Experimentation with AWS required here.
(define (file-exists-in-cloud? path)
  (define path-str (cond [(path? path) (path->string path)]
                         [else path]))
  (let* ((files (ls/aws (string-append (bucket) path-str)))
         (member? (filter (lambda (x) (equal? path-str x)) files)))
    (= (length member?) 1)))
  
  ; (path -> (listof path))
  ; Returns all files that are at the specified path.
  (define (list-files path)
    (define path-str (cond [(path? path) (path->string path)]
                           [else path]))
    (printf "Listing files at ~a\n" path-str)
    ;; FIXME terrible string manipulation
    (let* ((files (ls/aws (string-append (bucket) path-str)))
           (split-path (string-split path-str "/"))
           (split (lambda (x) (string-split x "/")))
           (split-files (map split files))
           (at-len (length split-path))
           (at-path (map last (filter (lambda (x) (= (length x) (+ at-len 1))) split-files))))
      (printf "at-path:~a\n" at-path)
      at-path))
  
  ; (path -> (listof path))
  ; Returns all directories that are at the specified path.
  (define (list-dirs path)
    (let* ((len (string-length path))
           (lc (if (= len 0) "" (string-ref path (- len 1))))
           (pathPrime (if (eq? #\/ lc) path (string-append path "/")))
           (result (filter (lambda (p) (is-directory? (string-append pathPrime p))) (list-path pathPrime))))
      result))
  
  (define (list-path path)
    (let* ((files (ls/aws (string-append (bucket) path)))
           (split-path (string-split path "/"))
           (split (lambda (x) (string-split x "/")))
           (split-files (map split files))
           (at-len (length split-path))
           (at-path (map last (remove-duplicates (map (lambda (x) (take-up-to x (+ at-len 1))) split-files)))))
      at-path))
  
  (define (take-up-to ls n)
    (cond [(> (- (length ls) n) 0) (take ls n)]
          [else ls]))

;; FIXME it appears to me that this will return
;; true for nonexistent paths
(define (is-directory? path)
  (eq? 'directory (path-info path)))
  
; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
(define (list-sub-files path)
  (ls/aws (string-append (bucket) path)))

;; check that it's possible to ls at least before starting up
(define (startup-check)
  (list-dirs "/")
  (void))