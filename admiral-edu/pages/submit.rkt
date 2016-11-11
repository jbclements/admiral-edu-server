#lang racket/base

(require racket/list
         racket/contract
         web-server/http/bindings
         web-server/http/request-structs
         web-server/http/response-structs
         xml)

(require "../base.rkt"
         "errors.rkt"
         "../storage/storage.rkt"
         "../authoring/assignment.rkt"
         "../paths.rkt"
         "path-xexprs.rkt"
         "next.rkt")

;; handles an incoming submission.... or publication of a submission?
(provide (contract-out
          [submit (-> ct-session? basic-ct-id? basic-ct-id?
                      (listof (cons/c symbol? (or/c string? bytes?)))
                      (listof binding?) xexpr?)]))
(define (submit session assignment step bindings raw-bindings)
  (let ((table (ct-session-table session))
        (uid (ct-session-uid session)))
    (cond [(and (hash-has-key? table 'action)
                ;; the string "submit" should probably actually be "publish", as it
                ;; appears to indicate publication. 
                (string=? (hash-ref table 'action) "submit"))
           (handle-publish session uid assignment step)]
          ;; submit a file:
          [else
           (let* ((data (extract-binding/single 'file bindings))
                  ;; FIXME won't work correctly until DrRacket does RFC2388 Content-Disposition parsing.
                  (filename (bytes->string/utf-8 (binding:file-filename (car raw-bindings)))))
             (unless (assignment:exists? assignment (class-name))
               (raise-404-not-found (format "assignment ~e not found for class" assignment)))
             (if (check-okay-to-submit uid assignment step)
                 (preview-upload session uid assignment step filename data)
                 (raise-400-bad-request "Could not submit to the specified step.")))])))

(define (check-okay-to-submit uid assignment step)
  (let ((do-next (next-step assignment uid)))
    (cond 
      [(MustSubmitNext? do-next) (equal? (Step-id (MustSubmitNext-step do-next)) step)]
      [else #f])))


(define (preview-upload session uid assignment step filename data)
  (let ((result  (upload-submission (class-name) uid assignment step filename data)))
    (cond [(Success? result) 
           ;; We have uploaded the file successfully, we now have a browser and a confirm submission button
           `(html
             (title "Captain Teach - Submission Uploaded")
             (body
              (p "Submission uploaded successfully. " (b "Note:") " Your submission has not yet been published.")
              (p ,(cta `((href ,(ct-url-path-/ session "next" assignment)))
                       "View Submission"))))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             `(html
               (title "Captain Teach - Submission Failed")
               (body
                (p ,message)
                (p ,(cta `((href ,(ct-url-path-/ session "next" assignment)))
                       "Back")))))])))

;; publishes an existing submission
(define (handle-publish session uid assignment step)
  ;; FIXME submit-step does some ... interesting things.
  (let ((result (submit-step assignment step uid))
        (start-url (hash-ref (ct-session-table session) 'start-url)))
    (cond [(Success? result) 
           (let ((message (Success-result result)))
             `(html
               ;; FIXME should this be Submission Successful? Submission published?
               (title "Captain Teach - Action Succeeded")
               (body
                (p ,message)
                (p (a ((href ,(string-append start-url "../../../feedback/" assignment "/")))
                      "Continue")))))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             `(html
               ;; FIXME should be submission failed? publication failed?
               (title "Captain Teach - Action Failed")
               (p ,message)
               (p (a ((href ,(string-append start-url "../../../next/" assignment "/")))
                     "Back"))))])))
