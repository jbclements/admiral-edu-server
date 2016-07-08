#lang racket/base

;; this file is about regression testing, with the lightest possible
;; layer of actual correctness testing. Specifically, it makes a bunch
;; of requests and prints out the results, but it also checks that the
;; status codes are what they should be.

(module+ test
  (require racket/string
           racket/struct
           racket/list
           racket/match
           racket/file
           web-server/servlet-dispatch
           web-server/http/response-structs
           web-server/web-server
           web-server/http/request-structs
           xml
           rackunit
           rackunit/text-ui
           "../dispatch.rkt"
           (prefix-in error: "../pages/errors.rkt")
           "../base.rkt"
           "../storage/storage-basic.rkt"
           "../util/config-file-reader.rkt"
           net/url
           "testing-shim.rkt"
           "extract-links.rkt"
           "testing-back-doors.rkt")
  
  (init-shim)

  (when (directory-exists? (class-name-shim))
    (fprintf (current-error-port)
             "ALERT: DELETING EXISTING CLASS DIRECTORY.\n")
    (delete-directory/files (class-name-shim)))
  
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
      [else
       (list 'not-a-response-at-all r)]))

  ;; there are some fairly complex invariants relating the raw bindings,
  ;; the bindings (though we shouldn't be using these at all), and the post
  ;; data. We use a binding-spec that can be mapped to both bindings and
  ;; raw-bindings. Also, we just give up on getting the post data right.
  ;; We'll implement that if we need it...
  
  (define (spec->raw-bindings binding-spec)
    (match binding-spec
      [(list 'multipart/file file-bindings)
       (for/list ([b (in-list file-bindings)])
         (match-define (list label filename content) b)
         (binding:file (string->bytes/utf-8 (symbol->string label))
                       (string->bytes/utf-8 filename)
                       '()
                       (string->bytes/utf-8 content)))]
      [else
       (for/list ([b (in-list binding-spec)])
         (binding:form (string->bytes/utf-8 (symbol->string (car b)))
                       (string->bytes/utf-8 (cdr b))))]))

  ;; SHOULDN'T USE ORDINARY BINDINGS AT ALL....
  (define (spec->bindings binding-spec)
    (match binding-spec
      [(list 'multipart/file file-bindings)
       (for/list ([b (in-list file-bindings)])
         (match-define (list label filename content) b)
         (cons label content))]
      [else
       binding-spec]))
  
  (define (run-request user path [binding-spec '()] [post? #f] [post-data #""])
    (let* ([bindings (spec->bindings binding-spec)]
           (raw-bindings (spec->raw-bindings binding-spec))
           (start-rel-url (ensure-trailing-slash (string-append "/" (class-name-shim) "/" (string-join path "/"))))
           (session (ct-session (class-name-shim) user (make-table start-rel-url bindings)))
           (result (with-handlers ([(λ (x) #t) server-error-shim])
                     (handlerPrime post? post-data session bindings raw-bindings path))))
      (explode-response result)))

  ;; quick hack to speed test case entry: replace slashes with spaces, turn into list:
  (define (path2list p)
    (regexp-split #px"/" p))


  (define m (master-user-shim))
  (define stu1 "frogstar@example.com")
  (define stu2 "mf2@example.com")

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
      (200 (,m ("assignments")))
      (200 (,m ("roster")))
      (200 (,m ("roster" "new-student")))
      ;; should be a 400, not a 200:
      (400 (,m ("roster" "new-student") () #t))
      (400 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu1))
               #t))
      ;; create same student again? (shouldn't be 200 okay)
      (400 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu1))
               #t))
      (200 (,m ("author")))
      ;; ouch internal error!
      (404 (,m ("author") () #t #"assignment-id : zzz1"))
      ;; ouch! another internal error!
      (404 (,m ("author" "bogwater") () #t #"assignment-id : zzz1"))
      ;; bad YAML
      (400 (,m ("author" "validate") () #t #"ziggy stardust"))
      ;; bogus path piece
      (404 (,m ("author" "boguspath" "validate") () #t ,assignment-yaml))
      (200 (,m ("author" "validate") () #t ,assignment-yaml))
      (200 (,m ("author" "validate") () #t ,yaml-with-html))
      (200 (,m ("assignments")))
      (200 (,m ("assignments" "dashboard" "test-with-html")))
      (200 (,m ("dependencies" "test-with-html")))
      (200 (,m ("dependencies" "test-with-html" "tests" "student-reviews")))
      ;; fix this bug by rewriting to use raw bindings everywhere and fail nicely
      ;; on line 197 of authoring/next-action.rkt
      (400 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload") () #t #""))
      (200 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload")
               (multipart/file
                ((file-1 "file-1" "abcd")
                 (file-2 "grogra-2" "efgh"))) #t))
      (200 (,m ("assignments")))
      (200 (,m ("assignments" "dashboard" "test-with-html")))
      ;; not open yet:
      (400 (,stu1 ("next" "test-with-html")))
      ;; open the assignment
      (200 (,m ("assignments" "open" "test-with-html")))
      ;; student navigation:
      (200 (,stu1 ()))
      (200 (,stu1 ("assignments")))
      (200 (,stu1 ("feedback" "test-with-html")))
      ((200 ,no-italics) (,stu1 ("next" "test-with-html")))
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart/file
                   ((file "my-file" "oh.... \n two lines!\n")))
                  #t))
      ((200 ,no-italics) (,stu1 ("next" "test-with-html")))
      ;; re-submit
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart/file
                   ((file "my-file" "oops... \n two different lines\n")))
                  #t))
      ;; re-submit with different file name
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart/file
                   ((file "my-different-file" "oops... \n two different lines\n")))
                  #t))
      ;; content of the iframe:
      (200 (,stu1 ("browse" "test-with-html" "tests")))
      ;; the file (gosh I hope you can't see others' submissions...
      (200 (,stu1 ("browse" "test-with-html" "tests" "my-different-file")))
      ;; create another student
      (200 (,m ("roster" "new-student") ((action . "create-student")
                                         (uid . ,stu1))
               #t))
      ;; that student submits:
      (200 (,stu2 ("submit" "test-with-html" "tests")
                  (multipart/file
                   ((file "a-third-file" "zzz\n\nzzz\nzzz\n")))
                  #t))
      ;; can stu2 read stu1's file? No. Good.
      (403 (,stu2 ("browse" "test-with-html" "tests" "my-different-file")))
      ;; stu1 completes submit:
      (200 (,stu1 ,(path2list "submit/test-with-html/tests")
                  ((action . "submit"))
                  #t))
      (200 (,stu1 ,(path2list "feedback/test-with-html")))
      ;; bogus hash:
      (403 (,stu1 ,(path2list "review/598109a435c52dc6ae10c616bcae407a")))
      ;; thunk to delay extraction of saved html:
      (200 ,(λ () (list stu1 (list "review" (lastreview)))))
      ;; the iframe...
      (200 ,(λ () (list stu1 (list "file-container" (lastreview)))))
      ;; viewing a bogus feedback
      (403 (,stu1 ("feedback" "file-container" "BOGUSSS" "ALSOBAD" "load")))))

  ;; return the last pending review for student 1 on "test-with-html"
  (define (lastreview)
    (last (pending-review-hashes (cons "test-with-html" stu1))))

  (define REGRESSION-FILE-PATH
    (string-append "/tmp/regression-results-"(number->string (current-seconds))".rktd"))

  (run-tests
   (test-suite
    "generate regression & a few tests"
    (call-with-output-file REGRESSION-FILE-PATH
      (λ (r-port)
        (for ([test (in-list tests)]
              [i (in-naturals)])
          (match-define (list expected request-args-or-thunk) test)
          (define request-args
            ;; !@#$ request hashes... can't extract until earlier tests have been
            ;; run.
            (cond [(procedure? request-args-or-thunk) (request-args-or-thunk)]
                  [else request-args-or-thunk]))
          (define result (apply run-request request-args))
          (test-case
           (format "~s" (list i request-args))
           (match expected
            [(? number? code)
             (check-equal? (first result) code)]
            [(list (? number? code)
                   (? procedure? test-proc))
             (begin (check-equal? (first result) code)
                    (test-proc result))]))
          (define output-val (list i request-args result))
          (fprintf r-port "~s\n" output-val)
          (printf "~s\n" output-val))))))

  
  (sleep 1)
  )


