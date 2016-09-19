#lang typed/racket/base

;; there's a lot of shared code between the various
;; "file container" pages. This file is an attempt to
;; collect some of it.

(require "typed-xml.rkt"
         racket/path
         racket/list
         "../storage/storage.rkt"
         "../ct-session.rkt"
         "../paths.rkt")

(provide render-directory
         download-url
         to-path-html
         to-step-link
         render-file)

;; a link-maker takes a relative Ct-Path
;; and returns a url Ct-Path
(define-type Link-Maker (Ct-Path -> Ct-Path))

;; generate the xexprs representing the directory
;; browser. In the case of feedback, we don't show the download
;; link. I don't know whether this is deliberate, but I'm preserving
;; this behavior.
(: render-directory (Link-Maker Link-Maker
                     Ct-Path [#:show-download Boolean]
                     -> (Listof XExpr)))
(define (render-directory link-maker
                          download-link-maker
                          dir-path
                          #:show-download [download? #t])
  (define path-path (ct-path->path dir-path))
  ;; FIXME remove path->string conversion when list-dirs accepts Path-String
  (let ((dirs (map path->ct-path (list-dirs (path->string path-path))))
        (files (map path->ct-path (list-files (path->string path-path)))))
    `((div ((id "directory") (class "browser"))
          (ul
           ,@(append (map (html-directory link-maker) dirs)
                     (map (html-file link-maker download-link-maker download?)
                          files)))))))

;; returns an xexpr representing a subdirectory in the directory listing
(: html-directory (Link-Maker -> (Ct-Path -> XExpr)))
(define (html-directory link-maker)
  (lambda (dir)
    `(li ((class "directory"))
         ;; FIXME path
         (a ((href ,(url-path->url-string (link-maker dir))))
            ,(path->string (ct-path->path dir))))))

;; returns an xexpr representing a file in the directory listing
(: html-file (Link-Maker Link-Maker Boolean -> (Ct-Path -> XExpr)))
(define (html-file link-maker download-link-maker download?)
  (lambda ([file : Ct-Path])
    (: maybe-download-link (Listof XExpr))
    (define maybe-download-link
      (cond [download?
             `((span ((style "float: right"))
                     (a ((href ,(url-path->url-string
                                 (download-link-maker file))))
                        "Download File")))]
            [else
             `()]))
    `(li ((class "file"))
         ;; FIXME path
         (a ((href ,(url-path->url-string (link-maker file))))
            ,(path->string (ct-path->path file)))
         ,@maybe-download-link)))


;; the link for a file download. Oh, ugh, hack for
;; file paths. The real fix is
;; - improve path handling (start-url always ends with slash?), and
;; - don't put the download token in this weird position
(: download-url (ct-session String String [#:dotdot-hack Boolean] -> String))
(define (download-url session start-url file #:dotdot-hack [add-dotdot? #f])
  (define dotdot-hack-path (cond [add-dotdot? '(up)]
                                 [else '()]))
  (define rel-path (apply build-path/convention-type
                          'unix
                          (cast
                           (append dotdot-hack-path
                                   (list "download"
                                         file))
                           (Pairof (U 'up Path-String) (Listof Path-String)))))
  (string-append start-url (some-system-path->string rel-path)))


;; FIXME: WIP: use this to replace prior function. must convert
;; paths to Ct-Paths.
(: new-download-url (ct-session String String Ct-Path
                                -> Ct-Path))
(define (new-download-url session assignment-id step-id path)
  (ct-path-join
   (ct-url-path session "browse-download" assignment-id step-id)
   path))

(: new-browse-url (ct-session String String Ct-Path
                              -> Ct-Path))
(define (new-browse-url session assignment-id step-id path)
  (ct-path-join
   (ct-url-path session "browse" assignment-id step-id)
   path))

;; given a list of strings corresponding to a local-file
;; path, construct a list of xexprs displaying that path,
;; where each element other than the last is a link to
;; a super-directory
(: to-path-html ((Listof String) -> (Listof XExpr)))
(define (to-path-html path-strs)
  (define input-len (length path-strs))
  (define path-xexprs
    (for/list : (Listof XExpr)
      ([i (in-naturals)]
       [path-elt (in-list path-strs)])
      (cond
        ;; last one gets no up-link
        [(= i (- input-len 1))
         path-elt]
        [else
         (define dotdots (for/list : (Listof 'up)
                           ([i (in-range (- input-len i 1))])
                           'up))
         `(a ((href ,(some-system-path->string
                      ;; FIXME need general conversion convention
                      (apply build-path/convention-type
                             'unix
                             ;; TR can't see that the list can't
                             ;; be empty, requiring cast:
                             (cast
                              (append dotdots (list path-elt))
                              (Pairof (U String 'up)
                                      (Listof (U String 'up))))))))
             ,path-elt)])))
  (add-between path-xexprs " / "))

;; given a step-id and a depth, generate an xexpr for a link
;; with the name of the step and a path with the given number of
;; dotdots. CF test case.
;; FIXME gets called with negative numbers. Is this a bug?
(: to-step-link (XExpr Integer -> XExpr))
(define (to-step-link step depth)
  (if (< depth 0) step
      ;; FIXME yucky paths
      (let ((updepth (string-append
                      (apply string-append
                             (for/list : (Listof String)
                               ([i (in-range depth)])
                               "../"))
                      "./")))
        `(a ((href ,updepth)) ,step))))

;; a textarea to be replaced by the codemirror instance
(define render-file
  '((textarea ((id "file") (class "file")) "")))

(module+ test
  (require typed/rackunit)

  (define session (ct-session "test-class"
                              "user1@example.com"
                              #f
                              (hash)))

  (define lm1 (λ ([p : Ct-Path])
                (ct-path-join (ct-url-path session "blobby" "gobby")
                              p)))
  (define lm2 (λ ([p : Ct-Path])
                (ct-path-join (ct-url-path session "blob-download" "gobby")
                              p)))
  
  ;; derived from a regression test:
  (check-equal? ((html-directory lm1) (rel-ct-path "bc"))
                '(li ((class "directory"))
                     (a ((href "/test-class/blobby/gobby/bc")) "bc")))

  (check-equal? ((html-directory lm1) (rel-ct-path "bc" "de"))
                '(li ((class "directory"))
                     (a ((href "/test-class/blobby/gobby/bc/de")) "bc/de")))

  (check-equal? ((html-file lm1 lm2 #t) (rel-ct-path "c"))
                '(li ((class "file"))
                     (a ((href "/test-class/blobby/gobby/c")) "c")
                     (span ((style "float: right"))
                           (a ((href "/test-class/blob-download/gobby/c"))
                              "Download File"))))

  (check-equal? ((html-file lm1 lm2 #f) (rel-ct-path "c"))
                '(li ((class "file"))
                     (a ((href "/test-class/blobby/gobby/c")) "c")))
  
  (check-equal? (download-url session "http://example.com/foo/bar/baz/" "quux")
                "http://example.com/foo/bar/baz/download/quux")
  (check-equal? (download-url session "http://example.com/foo/bar/baz/quux/" "quux"
                              #:dotdot-hack #t)
                "http://example.com/foo/bar/baz/quux/../download/quux")

  (check-equal? (to-path-html '("a" "b" "c"))
              '((a ((href "../../a")) "a")
                " / "
                (a ((href "../b")) "b")
                " / "
                "c"))

  
  ;; DERIVED FROM REGRESSION
  ;; FIXME what's the point of the trailing ./ ?
  (check-equal? (to-step-link "argwarg" 4)
                '(a ((href "../../../.././")) "argwarg"))
  (check-equal? (to-step-link "argwarg" 0)
                '(a ((href "./")) "argwarg")))