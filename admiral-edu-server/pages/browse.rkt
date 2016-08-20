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
         "../paths.rkt"
         (prefix-in error: "errors.rkt")
         "../util/file-extension-type.rkt"
         "../authoring/assignment.rkt"
         "templates.rkt"
         "file-container-helpers.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))


(provide (contract-out
          [do-file-container
           (-> ct-session? legal-path-elt? legal-path-elt? (listof legal-path-elt?)
               any)]))

;; render a "file-container" page (used exclusively in an iframe for listing
;; a directory or editing a file using CodeMirror
;; path contains a list of strings representing the elements of the URL after
;; the assignment-id and stepName, which in this case represent a path in
;; the class-local filesystem
(define (do-file-container session assignment-id step-id path)
  (define user-id (ct-session-uid session))
  ;; FIXME eliminate this...
  (define start-url (hash-ref (ct-session-table session) 'start-url))
  (define class (ct-session-class session))
  [define default-mode (determine-mode-from-filename path)]
  [define step-link (to-step-link step-id (length path))]
  [define path-html (to-path-html path)]
  [define rel-ct-path (strs->rel-ct-path path)]
  (define file-path
    (submission-file-path class assignment-id user-id step-id
                          rel-ct-path))
  (match (path-info file-path)
    ['directory
     (define contents (render-directory file-path start-url))
     (define maybe-file-url #f)
     (browse-file-container-page assignment-id step-link path-html default-mode
                                 contents maybe-file-url)]
    ['file
     (define contents render-file)
     (define maybe-file-url
       (ct-path->url-path
        session
        (strs->rel-ct-path `("browse-download" ,assignment-id ,step-id ,@path))))
     (browse-file-container-page assignment-id step-link path-html default-mode
                                 contents maybe-file-url)]
    ['does-not-exist
     (raise-403-not-authorized)]))

(define (determine-mode-from-filename url)
  (cond [(empty? url) "directory"]
        [else
         (let* ((filename (last url))
                (split (string-split filename "."))
                (ext (if (null? split) "" (last split))))
           (extension->file-type ext))]))

(define (to-step-link step depth)
  (if (<= depth 0) step
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) (xexpr->string step))))
        `(a ((href ,updepth)) step))))

