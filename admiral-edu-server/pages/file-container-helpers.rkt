#lang typed/racket/base

(require "typed-xml.rkt"
         racket/path
         racket/list
         "../storage/storage.rkt")

(provide render-directory
         download-url
         to-path-html
         to-step-link
         render-file)

;; generate the xexprs representing the directory
;; browser. In the case of feedback, we don't show the download
;; link. I don't know whether this is deliberate, but I'm preserving
;; this behavior.
(: render-directory (String String [#:show-download Boolean]-> (Listof XExpr)))
(define (render-directory dir-path start-url
                          #:show-download [download? #t])
  (let ((dirs (list-dirs dir-path))
        (files (list-files dir-path)))
    `((div ((id "directory") (class "browser"))
          (ul
           ,@(append (map (html-directory start-url) dirs)
                     (map (html-file start-url download?) files)))))))

;; returns an xexpr representing a subdirectory in the directory listing
(: html-directory (String -> (String -> XExpr)))
(define (html-directory start-url)
  (lambda (dir)
    `(li ((class "directory"))
         ;; FIXME path
         (a ((href ,(string-append start-url dir)))
            ,dir))))

;; returns an xexpr representing a file in the directory listing
(: html-file (String Boolean -> (String -> XExpr)))
(define (html-file start-url download?)
  (lambda (file)
    (define maybe-download-link
      (cond [download?
             `((span ((style "float: right"))
                     (a ((href ,(download-url start-url file)))
                        "Download File")))]
            [else
             `()]))
    `(li ((class "file"))
         ;; FIXME path
         (a ((href ,(string-append start-url file))) ,file)
         ,@maybe-download-link)))


;; the link for a file download. Oh, ugh, hack for
;; file paths. The real fix is
;; - improve path handling (start-url always ends with slash?), and
;; - don't put the download token in this weird position
(: download-url (String String [#:dotdot-hack Boolean] -> String))
(define (download-url start-url file #:dotdot-hack [add-dotdot? #f])
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

  ;; derived from a regression test:
  (check-equal? ((html-directory "http://www.example.com/a/") "bc")
                '(li ((class "directory"))
                     (a ((href "http://www.example.com/a/bc")) "bc")))

  ;; derived from a regression test:
  (check-equal? ((html-file "http://www.example.com/a/b/" #t) "c")
                '(li ((class "file"))
                     (a ((href "http://www.example.com/a/b/c")) "c")
                     (span ((style "float: right"))
                           (a ((href "http://www.example.com/a/b/download/c"))
                              "Download File"))))

  (check-equal? ((html-file "http://www.example.com/a/b/" #f) "c")
                '(li ((class "file"))
                     (a ((href "http://www.example.com/a/b/c")) "c")))
  
  (check-equal? (download-url "http://example.com/foo/bar/baz/" "quux")
                "http://example.com/foo/bar/baz/download/quux")
  (check-equal? (download-url "http://example.com/foo/bar/baz/quux/" "quux"
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