#lang typed/racket/base

(require racket/file
         racket/string
         racket/list
         racket/system
         "../configuration.rkt")

(require/typed racket/file
               [make-parent-directory* (Path-String -> Void)])

;;TODO: Once everything is typed, we should replace almost all instances of Path-String with Path

; (path -> string?)
(provide retrieve-file)
(: retrieve-file (Path-String -> String))
(define (retrieve-file path)
  (file->string
   (build-path (local-storage-path) path)))

(provide retrieve-file-bytes)
(: retrieve-file-bytes (Path-String -> Bytes))
(define (retrieve-file-bytes path)
  (file->bytes
   (build-path (local-storage-path) path)))


; (path -> contents -> ())
; Given a path and the contents to a file, writes that file (overwriting any existing file).
; Writes the local file (overwriting if necessary). Then, pushes the local file to the cloud.
(provide write-file)
(: write-file (Path-String Any -> Void))
(define (write-file path contents)
  (define local-path (build-path (local-storage-path) path))
  (make-parent-directory* local-path)
  (display-to-file contents local-path #:exists 'replace))

; Deletes the local copy
(provide delete-path)
(: delete-path (Path-String -> Void))
(define (delete-path path)
  (when (not (eq? 'does-not-exist (path-info path)))
      (delete-directory/files (build-path (local-storage-path) path) #:must-exist? #f)))


; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(provide path-info)
(: path-info (Path-String -> (U 'file 'directory 'does-not-exist)))
(define (path-info path)
  (define local-path (build-path (local-storage-path) path))
  (cond [(directory-exists? local-path) 'directory]
        [(file-exists? local-path) 'file]
        [else 'does-not-exist]))


; (path -> (listof Path-String))
; Returns all files that are at the specified path.
(provide list-files)
(: list-files (Path-String -> (Listof String)))
(define (list-files path)
  (define local-path (build-path (local-storage-path) path))
  (let ((f (lambda: ([p : Path]) (file-exists? (build-path local-path p)))))
    (map path->string (filter f (directory-list local-path)))))


(provide list-dirs)
(: list-dirs (Path-String -> (Listof String)))
(define (list-dirs path)
  (define local-path (build-path (local-storage-path) path))
  (let ((f (lambda: ([p : Path]) (directory-exists? (build-path local-path p)))))
    (map path->string (filter f (directory-list local-path)))))

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
(provide list-sub-files)
(: list-sub-files (Path-String -> (Listof String)))
(define (list-sub-files path)
  (parameterize ([current-directory (local-storage-path)])
    (for/list ([p (in-directory path)]
               #:when (file-exists? p))
      (path->string p))))

;; create the parent directory for a file, if necessary
(provide ensure-path-exists)
(: ensure-path-exists (Path-String -> Void))
(define (ensure-path-exists path)
  (make-parent-directory* (build-path (local-storage-path) path)))

;; TODO: Eventually we want to detect the archive type and choose the correct program to extract it.
;; This also needs to be able to handle invalid file types
(provide unarchive)
(: unarchive (Path-String Path-String -> Boolean))
(define (unarchive tgt-path archive-path)
  (define tgt-path-str (path-string->string (build-path
                                             (local-storage-path)
                                             tgt-path)))
  (define archive-path-str (path-string->string (build-path
                                                 (local-storage-path)
                                                 archive-path)))
  (cond [(is-zip? archive-path)
         (system* (unzip-binary)
                  archive-path-str
                  "-d"
                  tgt-path-str)]
        [(is-tar? archive-path)
         (system* (tar-binary)
                  "-xf"
                  archive-path-str
                  "-C"
                  tgt-path-str)]
        [else (error (format "Could not unarcharve ~a." archive-path))]))


(: path-string->string (Path-String -> String))
(define (path-string->string path)
  (cond [(path? path) (path->string path)]
        [else path]))


;; is this a path to a file ending in .zip?
;; (does not access file system)
(provide is-zip?)
(: is-zip? (Path-String -> Boolean))
(define (is-zip? file)
  (define-values (base name must-be-dir?) (split-path file))
  (and (not must-be-dir?)
       (not (symbol? name))
       (not (not (regexp-match "\\.[zZ][iI][pP]$" name)))))

;; is this a path to a file ending in .tar?
;; (does not access file system)
(provide is-tar?)
(: is-tar? (Path-String -> Boolean))
(define (is-tar? file)
  (define-values (base name must-be-dir?) (split-path file))
  (and (not must-be-dir?)
       (not (symbol? name))
       (not (not (regexp-match "\\.[tT][aA][rR]$" name)))))

;; 1) creates submissions subdirectory of assignment
;; 2) moves everything except default submissions and reviews into submissions directory
;; 3) archives assignment path into archive file in temp dir
(provide rearrange-and-archive)
(: rearrange-and-archive (Path-String String String String -> Void))
(define (rearrange-and-archive temp-dir class-id assignment-id archive-name)
  (define assignment-path (build-path (local-storage-path) temp-dir class-id assignment-id))
  (parameterize ([current-directory assignment-path])
    (make-directory "submissions")
    (for ([p (in-list (directory-list))]
          #:when (not (equal? (path->string p) "submissions"))
          #:when (not (regexp-match #px"^default-submission" p))
          #:when (not (equal? (path->string p) "reviews")))
      (rename-file-or-directory p (build-path "submissions" p))))
  (parameterize ([current-directory (build-path (local-storage-path) temp-dir)])
    (system* (zip-binary) "-r" archive-name assignment-path))
  (void))