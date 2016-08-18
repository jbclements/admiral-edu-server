#lang racket/base

;; this file is about regression testing, with the lightest possible
;; layer of actual correctness testing. Specifically, it makes a bunch
;; of requests and prints out the results, but it also checks that the
;; status codes are what they should be.

;; there are also a few tests that look for specific XSS attacks, by
;; checking to see whether the given tag can be made to appear in
;; the output.

(module+ test
  (require racket/string
           racket/list
           racket/match
           web-server/http/response-structs
           rackunit
           rackunit/text-ui
           "../dispatch.rkt"
           "../base.rkt"
           "testing-shim.rkt"
           "testing-support.rkt"
           "testing-back-doors.rkt")

  ;; this one is persistent
  (define REGRESSION-FILE-PATH-PERSISTENT
    (string-append "/tmp/regression-results-"(number->string (current-seconds))".rktd"))
  ;; this one gets overwritten every time
  (define REGRESSION-FILE-PATH-TEMP
    (string-append "/tmp/regression-results-tmp.rktd"))

  ;; delete everything in the database
  (init-shim)

  ;; delete local files
  (delete-local-files-shim)
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))

  
  (define (ensure-trailing-slash candidate)
    (let ((len (string-length candidate)))
      (cond [(= 0 len) "/"]
            [else (let ((last-char (string-ref candidate (- len 1))))
                    (cond [(eq? #\/ last-char) candidate]
                          [else (string-append candidate "/")]))])))

 	 
  #;(struct response (code message seconds mime headers output))

  (define (explode-response r)
    (cond
      [(response? r)
       (list (response-code r)
             (response-message r)
             (response-seconds r)
             (response-mime r)
             (response-headers r)
             (let ([os (open-output-string)])
               ((response-output r) os)
               (get-output-string os)))]
      ;; need to special-case this, because #<void> can't be
      ;; read by 'read'
      [(void? r)
       (list 'web-server-returned-void 'void-value)]
      [else
       (list 'not-a-response-at-all r)]))

  ;; we probably only care about the bytes in the case
  ;; of JSON arguments
  (define (spec->bytes binding-spec)
    (match binding-spec
      [(list 'json (? bytes? s))
       s]
      [other #""]))
  
  (define (run-request user path [binding-spec '()] [post? #f] [post-data-given #""])
    ;; a shortcut to avoid having to write 'alist everywhere
    (define spec (match binding-spec
                   [(cons (or 'multipart 'json) _) binding-spec]
                   [other (list 'alist other)]))
    [define bindings (spec->bindings spec)]
    (define raw-bindings (spec->raw-bindings spec))
    (define post-data-from-bindings (spec->bytes spec))
    ;; one or the other but not both...
    (define post-data
      (cond [(equal? post-data-given #"") post-data-from-bindings]
            [(equal? post-data-from-bindings #"") post-data-given]
            [else (error 'run-request "post data from bindings and optional arg: ~e and ~e"
                         post-data-from-bindings post-data-given)]))
    (define start-rel-url (ensure-trailing-slash (string-append "/" (class-name-shim) "/" (string-join path "/"))))
    (define session (ct-session (class-name-shim) user #f (make-table start-rel-url bindings)))
    (define result (with-handlers ([(λ (x) #t) server-error-shim])
              (handlerPrime post? post-data session bindings raw-bindings path)))
    (explode-response result))

  ;; quick hack to speed test case entry: replace slashes with spaces, turn into list:
  (define (path2list p)
    (regexp-split #px"/" p))


  (define m (master-user-shim))
  (define stu1 "frogstar@example.com")
  (define stu2 "mf2@example.com")
  ;; not in the class, ever:
  (define stu9 "stu9@example.com")

  (define assignment-yaml #"name: Assignment 1 Captain Teach
id: a1-ct
description: Problem 3.3.3 Solution
steps:
  - id: tests
    instructions: \"Submit your solution to problem 3.3.3\"
    reviews:
        - student-submission:
            id: student-reviews
            amount: 2
            rubric:
              - instruction: Click on the line number to add inline comments to the code to indicate missing tests, \
or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You \
must add a summative comment at the end.
              - likert:
                  id: correctness
                  text: These tests are complete, correct, and easy to read.
                  min-label: Disagree
                  max-label: Agree
                  granularity: 9
")

  ;; FIXME what's legal in an ID name?
  
  (define yaml-with-html #"name: Assignment 1 <i>Captain</i> Teach
id: test-with-html
description: Problem <i>3.3.3</i> Solution
steps:
  - id: tests
    instructions: \"Submit your <i>solution</i> to problem 3.3.3\"
    reviews:
        - student-submission:
            id: student-reviews
            amount: 2
            rubric:
              - instruction: Click on the line </i>number<i> to add inline comm\
ents to the code to indicate missing tests, or unclear or poorly organized code\
. Also, use comments to indicate particularly well-organized or clear tests. Yo\
u must add a summative comment at the end.
              - likert:
                  id: correctness
                  text: These tests are <i>complete</i>, correct, and easy to read.
                  min-label: Di<i>sa</i>gree
                  max-label: A<i>gre</i>e
                  granularity: 9
")

  (define (no-italics result)
    (match-define (list _ _ _ _ _ content) result)
    (check (compose not string-contains?) content "<i>"))

  ;; a test (currently) consists of a list
  ;; containing the expected status code and the
  ;; arguments to pass to run-request.
  ;; GRR! In order to allow hashes to be extracted
  ;; from earlier requests, must allow request args
  ;; to be thunked. 
  (define tests
    `((200 (,m ()))
      ;; REGRESSION: changed title
      (200 (,m ("assignments")))
      (200 (,m ("roster")))
      (200 (,m ("roster" "new-student")))
      ;; REGRESSION: error feedback less useful than old
      ;; should be a 400, not a 200:
      (400 (,m ("roster" "new-student") () #t))
      (200 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu1))
               #t))
      ;; create same student again?
      (200 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu1))
               #t))
      (200 (,m ("author")))
      ;; NON-REGRESSION: new version better than old
      (404 (,m ("author") () #t #"assignment-id : zzz1"))
      ;; ouch! another internal error!
      (404 (,m ("author" "bogwater") () #t #"assignment-id : zzz1"))
      ;; bad YAML
      ;; NON-REGRESSION: new version better than old
      (400 (,m ("author" "validate") () #t #"ziggy stardust")) ;; 10
      ;; bogus path piece... actually, the API just ignores
      ;; everything until the last one. For now, this is just okay.
      ;; holding off on fixing this until we have a handle on paths...
      (200 (,m ("author" "boguspath" "validate") () #t ,assignment-yaml)
           boguspath-validate)
      ;; this one is now invalid because the assignment already exists
      (400 (,m ("author" "validate") () #t ,assignment-yaml)
           existing-assignment)
      (200 (,m ("author" "validate") () #t ,yaml-with-html))
      ;; REGRESSION: missing title
      (200 (,m ("assignments")))
      ;; REGRESSION: missing title
      (200 (,m ("assignments" "dashboard" "test-with-html"))) ;; 15
      (200 (,m ("dependencies" "test-with-html")))
      (200 (,m ("dependencies" "test-with-html" "tests" "student-reviews")))
      ;; NON-REGRESSION: fixed bug
      (400 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload") () #t #""))
      (200 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload")
               (multipart
                ((namefilevalue #"file-1" #"file-1" () #"abcd")
                 (namefilevalue #"file-2" #"grogra-2" () #"efgh")))
               #t))
      (200 (,m ("assignments")))
      (200 (,m ("assignments" "dashboard" "test-with-html")))
      ;; not open yet:
      (400 (,stu1 ("next" "test-with-html")))
      ;; open the assignment
      (200 (,m ("assignments" "open" "test-with-html")))
      ;; student navigation:
      (200 (,stu1 ()))
      (200 (,stu1 ("assignments")))
      (403 (,stu9 ("feedback" "test-with-html"))
           stranger-feedback)
      (200 (,stu1 ("feedback" "test-with-html")))
      ;; XSS attack: html in assignment description:
      ((200 ,no-italics) (,stu1 ("next" "test-with-html")))
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue #"file" #"my-file" ()
                                      #"oh.... \n two lines!\n")))
                  #t))
      ((200 ,no-italics) (,stu1 ("next" "test-with-html"))) ;; 30
      ;; re-submit
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue
                     #"file" #"my-file" () #"oops... \n two different lines\n")))
                  #t))
      ;; re-submit with different file name
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue #"file"
                                      #"my-different-file"
                                      ()
                                      #"oops... \n two different lines\n")))
                  #t)
           stu1-resubmits)
      ;; content of the iframe:
      (200 (,stu1 ("browse" "test-with-html" "tests")))
      ;; the file 
      (200 (,stu1 ("browse" "test-with-html" "tests" "my-different-file")))
      ;; ouch, what about this:
      (403 (,stu1
            ("browse" "test-with-html" "tests" "my-different-file" "download" "test-class"
                      "test-with-html" ,stu1 "tests" "my-different-file"))
           accidental-trainwreck)
      ;; let's see what the download content looks like
      
      (200 (,stu1 ("browse" "test-with-html" "tests" "download" "my-different-file"))
           download)
      (200 (,stu1 ("browse-download" "test-with-html" "tests" "my-different-file"))
           new-download)
      
      ;; wait... random strangers can submit???
      (403 (,stu9 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue #"file"
                                      #"file-from-stranger" ()
                                      #"anotuh\n1234\n3")))
                  #t)
           stranger-submit)
      ;; create another student
      (200 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu2))
               #t))
      
      ;; that student submits:
      (200 (,stu2 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue
                     #"file" #"a-third-file" () #"zzz\n\nzzz\nzzz\n")))
                  #t))
      ;; can stu2 read stu1's file? No. Good.
      (403 (,stu2 ("browse" "test-with-html" "tests" "my-different-file")))
      ;; stu1 publishes:
      (200 (,stu1 ,(path2list "submit/test-with-html/tests")
                  ((action . "submit"))
                  #t)
           stu1-publishes)
      (200 (,stu1 ,(path2list "feedback/test-with-html")))
      ;; bogus hash:
      (403 (,stu1 ,(path2list "review/598109a435c52dc6ae10c616bcae407a")))
      ;; viewing a bogus feedback
      (403 (,stu1 ("feedback" "file-container" "BOGUSSS" "ALSOBAD" "load")))
      ;; thunk to delay extraction of hash:
      (200 ,(λ () (list stu1 (list "review" (lastreview stu1)))))
      ;; the iframe...
      (200 ,(λ () (list stu1 (list "file-container" (lastreview stu1)))))
      ;; stu2 logs in:
      (200 (,stu2 ()))
      ;; clicks on assignments
      (200 (,stu2 ("assignments")))
      ;; stu2 publishes:
      (200 (,stu2 ,(path2list "submit/test-with-html/tests")
                  ((action . "submit"))
                  #t)
           stu2-publishes)
      (200 (,stu2 ("feedback" "test-with-html")))
      ;; stu2 clicks on last review
      (200 ,(λ () (list stu2 (list "review" (lastreview-of stu2 stu1))))
           review)
      ;; load review file-container for directory
      (200 ,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1))))
           review-iframe-dir)
      ;; file-container for file
      (200 ,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1)
                                   "my-different-file")))
           review-iframe-file)
      ;; actual text of file
      (200 ,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1) "download"
                                   "my-different-file")))
           review-iframe-file-content)
      ;; actual text of file using new endpoint:
      (200 ,(λ () (list stu2 (list "download" (lastreview-of stu2 stu1) "my-different-file")))
           review-iframe-file-content-new)
      ;; should it be an error to submit bogus rubric json?
      (200 ,(λ () (list stu2 (list "review" (lastreview-of stu2 stu1) "tests" "save")
                        (list 'json #"\"abcd\"")
                        #t)))
      (200 ,(λ () (list stu2 (list "review" "submit" (lastreview-of stu2 stu1))))
           stu2-submits-review1)
      ;; do the other review too
      (200 ,(λ () (list stu2 (list "review" (lastreview stu2) "tests" "save")
                        (list 'json #"\"abcde\"")
                        #t)))
      (200 ,(λ () (list stu2 (list "review" "submit" (lastreview stu2))))
           stu2-submits-review2)
      ;; stu1 now views it
      (200 ,(λ () `(,stu1 ("feedback" "view" ,(firstfeedback stu1))))
           stu1-views-review)
      (200 ,(λ () `(, stu1 ("feedback" "file-container" ,(firstfeedback stu1))))
           stu1-views-review-fc-dir)
      (200 ,(λ () `(, stu1 ("feedback" "file-container" ,(firstfeedback stu1) "my-different-file")))
           stu1-views-review-fc-file)
      (200 ,(λ () `(, stu1 ("download" ,(firstfeedback stu1) "my-different-file")))
           stu1-views-review-fc-file-raw)
      ((200 ,no-italics)
       ,(λ () `(,stu1 ("feedback" "view" ,(firstfeedback stu1))
                      ((feedback . "feedback with <i>italics</i>.")
                       (flag . "goronsky"))
                      #t))
           stu1-submits-feedback)
      (200 (,stu2 ("feedback" "test-with-html")))))

  ;; return the last pending review for given student on "test-with-html"
  (define (lastreview uid)
    (last (pending-review-hashes (cons "test-with-html" uid))))

  ;; return the first pending review for the given student on "test-with-html"
  ;; where the reviewee is the given one
  (define (lastreview-of reviewer reviewee)
    (last (pending-review-hashes/reviewee (cons "test-with-html" reviewer)
                                           reviewee)))

  ;; return the feedback for given student on "test-with-html"
  (define (firstfeedback uid)
    (first (feedback-hashes (cons "test-with-html" uid))))



  (run-tests
   (test-suite
    "generate regression & a few tests"
    (call-with-output-file REGRESSION-FILE-PATH-PERSISTENT
      (λ (r-port)
        (call-with-output-file REGRESSION-FILE-PATH-TEMP
          #:exists 'truncate
          (λ (rt-port)
            (for ([test (in-list tests)]
                  [i (in-naturals)])
              (define-values (expected request-args-or-thunk testname)
                (match test
                  [(list a b) (values a b "")]
                  [(list a b c) (values a b (symbol->string c))]))
              (define request-args
                ;; !@#$ request hashes... can't extract until earlier tests have been
                ;; run.
                (cond [(procedure? request-args-or-thunk) (request-args-or-thunk)]
                      [else request-args-or-thunk]))
              (define result (apply run-request request-args))
              (test-case
               (format "~s" (list i testname request-args))
               (match expected
                 [(? number? code)
                  (check-equal? (first result) code)]
                 [(list (? number? code)
                        (? procedure? test-proc))
                  (begin (check-equal? (first result) code)
                         (test-proc result))]))
              (define output-val (list i testname request-args result))
              (fprintf r-port "~s\n" output-val)
              (fprintf rt-port "~s\n" output-val)
              #;(printf "~s\n" output-val))))))))

  
  (sleep 1)
  )


