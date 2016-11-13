#lang typed/racket/base

(require "../configuration.rkt"
         (prefix-in c: "cloud-storage.rkt")
         (prefix-in l: "local-storage.rkt"))

(define-syntax setup
  (syntax-rules ()
    [(_ [name ty c-version l-version arg ...] ...)
     (begin (provide name)
            ...
            (: name ty)
            ...
            (define (name arg ...)
              (cond
                [(string=? (storage-mode) "cloud-storage") (c-version arg ...)]
                [(string=? (storage-mode) "local") (l-version arg ...)]
                [else (error (format "Unrecognized storage mode: ~a" (storage-mode)))]))
            ...)]))

(setup 
       )
(setup ;[retrieve-file       c:retrieve-file       l:retrieve-file]
       [retrieve-file-bytes (Path-String -> Bytes)           c:retrieve-file-bytes l:retrieve-file-bytes a]
       [write-file          (Path-String Bytes -> Void)      c:write-file          l:write-file a b]
       [delete-path         (Path-String -> Void)            c:delete-path         l:delete-path a]
       [path-info           (Path-String -> (U 'file 'directory 'does-not-exist))
                                                             c:path-info           l:path-info a]
       [list-files          (Path-String -> (Listof String)) c:list-files          l:list-files a]
       [list-sub-files      (Path-String -> (Listof String)) c:list-sub-files      l:list-sub-files a]
       [list-dirs           (Path-String -> (Listof String)) c:list-dirs           l:list-dirs a]
       [startup-check       (-> Void)                        c:startup-check       l:startup-check]
       )

; retrieve-file: (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.

; write-file: (path string -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).

; delete-path: (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files

; path-info: (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist

; list-files: (path -> (listof path))
; Returns all files that are at the specified path.

; list-sub-files: (path -> (listof path))
; Returns all files that are at the specified path recursively adding all files in sub directories

; list-dirs: (path -> (listof path))
; Returns all directories that are at the specified path.








