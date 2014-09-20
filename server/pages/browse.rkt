#lang racket
(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         web-server/http/bindings
         (planet esilkensen/yaml:3:1))

(require "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../util/file-extension-type.rkt"
         "../authoring/assignment.rkt"
         (prefix-in authorized: "../util/authorized.rkt"))

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load session role rest [message '()])
  (authorized:can-edit session do-file-container session rest message))

(define (do-file-container session rest [message '()])
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (class (ct-session-class session))
         [assignment (second rest)]
         (stepName (third rest))
         (user-id (first rest))
         [default-mode (determine-mode-from-filename (last rest))]
         [load-url (string-append "'" start-url "load" "'")]
         [step (to-step-link stepName (- (length rest) 2))]
         [path (to-path-html (cdddr rest))]
         (file (to-path (cdddr rest)))
         (file-path (submission-file-path class assignment user-id stepName file))
         (contents (if (is-directory? file-path) (render-directory file-path start-url) (render-file file-path))))
        (string-append (include-template "html/browse-file-container-header.html")
                       contents
                       (include-template "html/file-container-footer.html"))))

(define (determine-mode-from-filename filename)
  (let* ((split (string-split filename "."))
         (ext (if (null? split) "" (last split))))
    (extension->file-type ext)))

(define (to-path-html input)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let* ((url (string-append (apply string-append (repeat "../" (- (length input) (+ (length acc) 1)))) (xexpr->string head)))
                                                (link (string-append " <a href=\"" url "\">" (xexpr->string head) "</a> / "))
                                                (new-acc (cons link acc)))
                                           (helper new-acc tail))]))))
    (helper '() input)))

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

(define (render-directory dir-path start-url)
  (let ((dirs (list-dirs dir-path))
        (files (list-files dir-path)))
    (string-append
     "<div id=\"directory\" class=\"browser\">"
     "<ul>"
     (apply string-append (map (html-directory start-url) dirs))
     (apply string-append (map (html-file start-url) files))
     "</ul>"
     "</div>")))

(define (html-directory start-url)
  (lambda (dir)
    (string-append "<li class=\"directory\"><a href=\"" start-url dir "\">" dir "</a></li>")))

(define (html-file start-url)
  (lambda (file)
    (string-append "<li class=\"file\"><a href=\"" start-url file "\">" file "</a></li>")))

(define (render-file file-path)
  (string-append "<textarea id=\"file\" class=\"file\">" (retrieve-file file-path) "</textarea>"))