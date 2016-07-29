#lang racket/base

(require racket/list
         racket/string
         racket/match
         racket/contract
         web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         web-server/http/bindings
         yaml)

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../util/file-extension-type.rkt"
         "../authoring/assignment.rkt"
         "templates.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))


;; FIXME totally loose contract tighten this up.
(provide (contract-out
          [load (-> ct-session? any/c (listof string?) any)]))
(define (load session role url)
  (do-file-container session (ct-session-uid session) (first url) (second url) (drop url 2)))

;; render a "file-container" page (used exclusively I believe in an iframe for listing
;; a directory or editing a file using CodeMirror
;; FIXME 'message' apparently unused?
;; path contains a list of strings representing the elements of the URL after
;; the assignment-id and stepName, which in this case represent a path in
;; the class-local filesystem
(define (do-file-container session user-id assignment-id stepName path)
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define class (ct-session-class session))
  [define assignment assignment-id]
  [define default-mode (determine-mode-from-filename path)]
  [define step (to-step-link stepName (length path))]
  [define path-html (to-path-html path)]
  ;; gee whiz... in the case of the file, we want to insert the 'download' label *before* the filename... use horrible dotdot?
  (define file (to-path path))
  (define file-path
    (submission-file-path class assignment user-id stepName file))
  (define is-dir (is-directory? file-path))
  (define contents (if is-dir
                       (render-directory file-path start-url)
                       '((textarea ((id "file") (class "file")) ""))))
  (define maybe-file-url
    (if is-dir #f (download-url start-url file #:dotdot-hack #t)))
  (browse-file-container-page assignment step path-html default-mode
                              contents maybe-file-url))


(provide download)
(define (download session role url)
  (let* ((class (ct-session-class session))
         (user (ct-session-uid session))
         (assignment (first url))
         (step (second url))
         (path (drop url 2))
         ;; FIXME don't use strings here
         (file-path (string-join (append (take path (- (length path) 2)) (list (last path))) "/"))
         (temp (printf "file-path: ~a\n" file-path))
         (data (maybe-get-file-bytes class assignment step user file-path)))
    (unless data
      (raise-403-not-authorized "You are not authorized to see this file."))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/octet-stream; charset=utf-8"
     empty
     (list data))))

(define (determine-mode-from-filename url)
  (cond [(empty? url) "directory"]
        [else
         (let* ((filename (last url))
                (split (string-split filename "."))
                (ext (if (null? split) "" (last split))))
           (extension->file-type ext))]))

;; given a list of strings corresponding to a local-file
;; path, construct a list of xexprs displaying that path,
;; where each element other than the last is a link to
;; a super-directory
(define (to-path-html input)
  (define input-len (length input))
  (define path-xexprs
    (for/list ([i (in-naturals)]
               [path-elt (in-list input)])
      (cond
        ;; last one gets no up-link
        [(= i (- input-len 1))
         path-elt]
        [else
         (define dotdots (for/list ([i (in-range (- input-len i 1))])
                           ".."))
         `(a ((href ,(path->string
                      ;; FIXME need general conversion convention
                      (apply build-path/convention-type
                             'unix
                             (append dotdots (list path-elt))))))
             ,path-elt)])))
  (add-between path-xexprs " / "))

(module+ test
  (require rackunit)
  (check-equal? (to-path-html '("a" "b" "c"))
                '((a ((href "../../a")) "a")
                  " / "
                  (a ((href "../b")) "b")
                  " / "
                  "c")))

(define (to-step-link step depth)
  (if (<= depth 0) (xexpr->string step)
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) (xexpr->string step))))
        (string-append "<a href=\"" updepth "\">" (xexpr->string step) "</a>"))))

(define (prepare-url word rest)
  (let* ((last-el (last rest))
         (prefix (if (equal? last-el "") "" (string-append last-el "/"))))
    (string-append "\"" prefix word "\"")))

(define (prepare-load-url rest)
  (prepare-url "load" rest))

(define (prepare-save-url rest)
  (prepare-url "save" rest))


;;TODO: Also in pages/review.rkt Should abstract to common function place
(define (to-path ls)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let ((new-acc (cons "/" (cons head acc))))
                           
                                           
                                           (helper new-acc tail))]))))
    (helper '() ls)))

;; generate the xexprs representing the directory
;; browser
(define (render-directory dir-path start-url)
  (let ((dirs (list-dirs dir-path))
        (files (list-files dir-path)))
    `((div ((id "directory") (class "browser"))
          (ul
           ,@(append (map (html-directory start-url) dirs)
                     (map (html-file start-url) files)))))))

;; returns an xexpr representing a subdirectory in the directory listing
(define (html-directory start-url)
  (lambda (dir)
    `(li ((class "directory"))
         (a ((href ,(string-append start-url dir)))
            ,dir))))

;; returns an xexpr representing a file in the directory listing
(define (html-file start-url)
  (lambda (file)
    `(li ((class "file"))
         (a ((href ,(string-append start-url file))) ,file)
         (span ((style "float: right"))
               (a ((href ,(download-url start-url file)))
                  "Download File")))))

;; the link for a file download. Oh, ugh, hack for
;; file paths. The real fix is
;; - improve path handling (start-url always ends with slash?), and
;; - don't put the download token in this weird position
(define (download-url start-url file #:dotdot-hack [add-dotdot? #f])
  (define dotdot-hack-path (cond [add-dotdot? '(up)]
                                 [else '()]))
  (define rel-path (apply build-path/convention-type
                          'unix
                          (append dotdot-hack-path
                                  (list "download"
                                        file))))
  (string-append start-url (path->string rel-path)))

(module+ test
  (check-equal? (download-url "http://example.com/foo/bar/baz/" "quux")
                "http://example.com/foo/bar/baz/download/quux")
  (check-equal? (download-url "http://example.com/foo/bar/baz/quux/" "quux"
                              #:dotdot-hack #t)
                "http://example.com/foo/bar/baz/quux/../download/quux"))


