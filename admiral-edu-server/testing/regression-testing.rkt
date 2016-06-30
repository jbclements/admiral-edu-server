#lang racket/base

;; this file is about regression testing, with the lightest possible
;; layer of actual correctness testing. Specifically, it makes a bunch
;; of requests and prints out the results, but it also checks that the
;; status codes are what they should be.

(module+ main
  (require racket/string
           racket/struct
           racket/list
           racket/match
           web-server/servlet-dispatch
           web-server/http/response-structs
           web-server/web-server
           web-server/http/request-structs
           rackunit
           "../dispatch.rkt"
           (prefix-in error: "../pages/errors.rkt")
           "../base.rkt"
           "../storage/storage-basic.rkt"
           "../util/config-file-reader.rkt"
           net/url
           "test-configuration.rkt")
  
  
  (current-configuration test-conf)

  ;; start with a fresh database
  (db-init)
  
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
    (list (response-code r)
          (response-message r)
          (response-seconds r)
          (response-mime r)
          (response-headers r)
          (let ([os (open-output-string)])
            ((response-output r) os)
            (get-output-string os))))

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
           (start-rel-url (ensure-trailing-slash (string-append "/" (class-name) "/" (string-join path "/"))))
           (session (ct-session (class-name) user (make-table start-rel-url bindings)))
           (result (with-handlers ([(λ (x) #t) error:server-error-response])
                     (handlerPrime post? post-data session bindings raw-bindings path))))
      (explode-response result)))

  (define m (master-user))
  (define stu1 "frogstar@example.com")

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
              - instruction: Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.
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
              - instruction: Click on the line </i>number<i> to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.
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
      (400 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload") () #t #""))
      (200 (,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload")
               (multipart/file
                ((file-1 "file-1" "abcd")
                 (file-2 "grogra-2" "efgh"))) #t))
      (200 (,m ("assignments")))
      (200 (,m ("assignments" "dashboard" "test-with-html")))
      (200 (,m ("assignments" "open" "test-with-html")))
      (200 (,stu1 ()))
      (200 (,stu1 ("assignments")))
      (200 (,stu1 ("feedback" "test-with-html")))
      ((200 ,no-italics) (,stu1 ("next" "test-with-html")))
      (200 (,stu1 ("submit" "test-with-html" "tests")
                  (multipart/file
                   ((file "my-file" "oh.... \n two lines!\n")))
                  #t))
      ((200 ,no-italics) (,stu1 ("next" "test-with-html")))))

  (define REGRESSION-FILE-PATH
    (string-append "/tmp/regression-results-"(number->string (current-seconds))".rktd"))

  (call-with-output-file REGRESSION-FILE-PATH
    (λ (r-port)
      (for ([test (in-list tests)])
        (match-define (list expected request-args) test)
        (define result (apply run-request request-args))
        (match expected
          [(? number? code) (check-equal? (first result) code)]
          [(list (? number? code)
                 (? procedure? test-proc))
           (begin (check-equal? (first result) code)
                  (test-proc result))])
        (define output-val (list request-args result))
        (fprintf r-port "~s\n" output-val)
        (printf "~s\n" output-val))))

  (sleep 1)
  )


