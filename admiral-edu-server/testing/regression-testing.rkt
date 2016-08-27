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
           racket/contract
           web-server/http/response-structs
           rackunit
           rackunit/text-ui
           html-parsing
           "../dispatch.rkt"
           "../base.rkt"
           "testing-shim.rkt"
           "testing-support.rkt"
           "testing-back-doors.rkt"
           "html-testing-support.rkt")

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
                   [(cons (or 'multipart 'json 'alist) _) binding-spec]
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
    ;; really, this should be happening inside the tested code, not out here....
    (define start-rel-url (ensure-trailing-slash (string-append "/" (class-name-shim) "/" (string-join path "/"))))
    (define session (ct-session-shim (class-name-shim) user #f (make-table start-rel-url bindings)))
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


  ;; what is a test? These contracts are essentially documentation.
  
  ;; call-params: (user path-strs [bindings] [post?] [raw-bytes?])
  (define ct-call-params/c
    (or/c
     ;; this procedure must return a call-params/c, but I want this to
     ;; be a flat contract.
     procedure?
     (cons/c string?
             (cons/c (listof string?)
                     (or/c null?
                           (cons/c binding-spec/c
                                   (or/c null?
                                         (cons/c boolean?
                                                 (or/c null?
                                                       (list/c bytes?))))))))))
  (define response-code? (or/c 200 403 400 404 500))
  (define ct-expected?
    (or/c response-code?
          (list/c response-code? procedure?)))
  (define ct-test/c
    (or/c (list/c ct-call-params/c ct-expected?)
          (list/c ct-call-params/c ct-expected? symbol?)))

  (define ct-test? (flat-contract-predicate ct-test/c))
  
  #;(check-equal? ((flat-contract-predicate ct-call-params/c)
                 '("masteruser@example.com" ("roster" "new-student") ((action . "create-student") (uid . "frogstar@example.com")) #t))
                #t)

  ;; some tests are known not to pass on the old version. Run the code and
  ;; log the output, but don't signal an error on stderr
  (define known-bad-in-original
    '(bad-new-student
      bad-author-post
      bad-author-path
      bad-yaml
      existing-assignment
      boguspath-validate
      bad-review-upload
      not-open-yet
      stranger-feedback
      assignment-description-xss
      assignment-description-xss-2
      stranger-submit
      see-others-file
      bogus-review
      bogus-file-container
      stu1-submits-feedback-xss
      accidental-trainwreck))
  
  ;; a test contains three parts: expected, call, and (optionally) a name.
  ;; a test (currently) consists of a list
  ;; containing the expected status code and the
  ;; arguments to pass to run-request.
  ;; GRR! In order to allow hashes to be extracted
  ;; from earlier requests, must allow request args
  ;; to be thunked. 
  (define tests
    `(((,m ())
       (200 ,(has-anchor-links
              '("/test-class/assignments/" "/test-class/roster/"))))
      ;; REGRESSION: changed title
      ((,m ("assignments"))
       (200 ,(has-anchor-links
              '("/test-class/author/"))))
      ((,m ("roster"))
       (200 ,(has-anchor-links
              '("/test-class/roster/upload-roster/"
                "/test-class/roster/new-student/"
                "/test-class/roster/edit/masteruser@example.com/"))))
      ((,m ("roster" "new-student")) 200)
      ;; REGRESSION: error feedback less useful than old
      ;; should be a 400, not a 200:
      ((,m ("roster" "new-student") () #t)
       400
       bad-new-student)
      ((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu1)))
               #t) 200)
      ;; create same student again? yes, that's legal
      ((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu1)))
               #t)
       200)
      ((,m ("author"))
       (200 ,(has-anchor-links '("javascript:validate()"))))
      ;; NON-REGRESSION: new version better than old
      ((,m ("author") () #t #"assignment-id : zzz1")
       404
       bad-author-post)
      ;; ouch! another internal error!
      ((,m ("author" "bogwater") () #t #"assignment-id : zzz1")
       404
       bad-author-path)
      ;; bad YAML
      ;; NON-REGRESSION: new version better than old
      ((,m ("author" "validate") () #t #"ziggy stardust")
       400
       bad-yaml) ;; 10
      ;; bogus path piece... actually, the API just ignores
      ;; everything until the last one. For now, this is just okay.
      ;; holding off on fixing this until we have a handle on paths...
      ((,m ("author" "boguspath" "validate") () #t ,assignment-yaml)
       200
       boguspath-validate)
      ;; this one is now invalid because the assignment already exists
      ((,m ("author" "validate") () #t ,assignment-yaml) 400
           existing-assignment)
      ((,m ("author" "validate") () #t ,yaml-with-html) 200)
      ;; REGRESSION: missing title
      ((,m ("assignments"))
       (200 ,(has-anchor-links
              '("/test-class/author/"
                "/test-class/assignments/dashboard/a1-ct/"
                "/test-class/assignments/dashboard/test-with-html/"))))
      ;; REGRESSION: missing title
      ((,m ("assignments" "dashboard" "test-with-html"))
       (200 ,(has-anchor-links
              '("/test-class/assignments/"
                "/test-class/assignments/status/test-with-html/"
                "/test-class/dependencies/test-with-html/"
                "/test-class/author/edit/test-with-html/"
                "/test-class/export/test-with-html/test-with-html.zip"
                "/test-class/assignments/delete/test-with-html/")))) ;; 15
      ((,m ("dependencies" "test-with-html"))
       (200 ,(has-anchor-links
              '("/test-class/assignments/"
                "/test-class/assignments/dashboard/test-with-html/"
                "/test-class/dependencies/test-with-html/tests/student-reviews/"))))
      ((,m ("dependencies" "test-with-html" "tests" "student-reviews")) 200)
      ;; NON-REGRESSION: fixed bug
      ((,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload") () #t #"")
       400
       bad-review-upload)
      ((,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload")
               (multipart
                ((namefilevalue #"file-1" #"file-1" () #"abcd")
                 (namefilevalue #"file-2" #"grogra-2" () #"efgh")))
               #t)
       (200 ,(has-anchor-links
              '("/test-class/assignments/"
                "/test-class/assignments/dashboard/test-with-html/"
                "/test-class/dependencies/test-with-html/tests/student-reviews/"))))
      ((,m ("assignments"))
       (200 ,(has-anchor-links
              '("/test-class/author/"
                "/test-class/assignments/dashboard/a1-ct/"
                "/test-class/assignments/dashboard/test-with-html/"))))
      ((,m ("assignments" "dashboard" "test-with-html"))
       (200 ,(has-anchor-links
              '("/test-class/assignments/"
                "/test-class/assignments/status/test-with-html/"
                "/test-class/assignments/open/test-with-html/"
                "/test-class/dependencies/test-with-html/"
                "/test-class/author/edit/test-with-html/"
                "/test-class/export/test-with-html/test-with-html.zip"
                "/test-class/assignments/delete/test-with-html/"))))
      ;; not open yet:
      ((,stu1 ("next" "test-with-html"))
       400
       not-open-yet)
      ;; open the assignment
      ((,m ("assignments" "open" "test-with-html"))
       (200 ,(has-anchor-links
              '("/test-class/assignments/"
                "/test-class/assignments/status/test-with-html/"
                "/test-class/assignments/close/test-with-html/"
                "/test-class/dependencies/test-with-html/"
                "/test-class/author/edit/test-with-html/"
                "/test-class/export/test-with-html/test-with-html.zip"
                "/test-class/assignments/delete/test-with-html/"))))
      ;; student navigation:
      ((,stu1 ())
       (200 ,(has-anchor-links
              '("/test-class/assignments/"))))
      ((,stu1 ("assignments"))
       (200 ,(has-anchor-links
              ;; FIXME YUCKY URL:
              '("/test-class/assignments/../feedback/test-with-html/"))))
      ((,stu9 ("feedback" "test-with-html"))
       403
       stranger-feedback)
      ((,stu1 ("feedback" "test-with-html"))
       (200 ,(has-anchor-links
              '("/test-class/feedback/test-with-html/../../next/test-with-html/"))))
      ;; XSS attack: html in assignment description:
      ((,stu1 ("next" "test-with-html"))
       (200 ,no-italics)
       assignment-description-xss)
      ((,stu1 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue #"file" #"my-file" ()
                                      #"oh.... \n two lines!\n")))
                  #t)
       (200 ,(has-anchor-links
              ;; FIXME YUCKY URL
              '("/test-class/submit/test-with-html/tests/../../../next/test-with-html/"))))
      ((,stu1 ("next" "test-with-html"))
       (200 ,no-italics)
       assignment-description-xss-2) ;; 30
      ;; re-submit
      ((,stu1 ("submit" "test-with-html" "tests")
                  (multipart
                   ((namefilevalue
                     #"file" #"my-file" () #"oops... \n two different lines\n")))
                  #t)
       (200 ,(has-anchor-links
              ;; FIXME YUCKY URL
              '("/test-class/submit/test-with-html/tests/../../../next/test-with-html/"))))
      ;; re-submit with different file name
      ((,stu1 ("submit" "test-with-html" "tests")
              (multipart
               ((namefilevalue #"file"
                               #"my-different-file"
                               ()
                               #"oops... \n two different lines\n")))
              #t)
       (200
        ,(has-anchor-links
          ;; FIXME YUCKY URL
          '("/test-class/submit/test-with-html/tests/../../../next/test-with-html/")))
       stu1-resubmits)
      ((,stu1 ("next" "test-with-html"))
       200
       stu1-not-yet-published)
      ;; content of the iframe:
      ((,stu1 ("browse" "test-with-html" "tests"))
       (200 ,(has-anchor-links
              '("/test-class/browse/test-with-html/tests/my-different-file"
                "/test-class/browse/test-with-html/tests/download/my-different-file"))))
      ;; the file 
      ((,stu1 ("browse" "test-with-html" "tests" "my-different-file"))
       (200 ,(has-anchor-links
              ;; FIXME how do we feel about this? first relative url path?
              '("../tests"))))
      ;; ouch, what about this:
      ((,stu1
        ("browse" "test-with-html" "tests" "my-different-file" "download" "test-class"
                  "test-with-html" ,stu1 "tests" "my-different-file"))
       403
       accidental-trainwreck)
      ;; let's see what the download content looks like
      ;; removed old-style download
      #;((,stu1 ("browse" "test-with-html" "tests" "download" "my-different-file"))
         200
         download)
      ;; trying the new-style browse download
      ((,stu1 ("browse-download" "test-with-html" "tests" "my-different-file"))
       200
       new-download)
      
      ;; wait... random strangers can submit???
      ((,stu9 ("submit" "test-with-html" "tests")
              (multipart
               ((namefilevalue #"file"
                               #"file-from-stranger" ()
                               #"anotuh\n1234\n3")))
              #t)
       403
       stranger-submit)
      ;; create another student
      ((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu2)))
           #t)
       200)
      
      ;; that student submits:
      ((,stu2 ("submit" "test-with-html" "tests")
              (multipart
               ((namefilevalue
                 #"file" #"a-third-file" () #"zzz\n\nzzz\nzzz\n")))
              #t)
       (200 ,(has-anchor-links
              ;; FIXME yucky url
              '("/test-class/submit/test-with-html/tests/../../../next/test-with-html/")))
       stu2-submits)
      ;; can stu2 read stu1's file? No. Good.
      ((,stu2 ("browse" "test-with-html" "tests" "my-different-file"))
       403
       see-others-file)
      ;; stu1 publishes:
      ((,stu1 ,(path2list "submit/test-with-html/tests")
              (alist ((action . "submit")))
              #t)
       (200 ,(has-anchor-links
              ;; FIXME yucky url
              '("/test-class/submit/test-with-html/tests/../../../feedback/test-with-html/")))
       stu1-publishes)
      ((,stu1 ,(path2list "feedback/test-with-html"))
       ;; FIXME yucky urls
       (200 ,(λ (x)
               (let ([hashes (pending-review-hashes (cons "test-with-html" stu1))])
                 ((has-anchor-links
                   (cons
                    "/test-class/feedback/test-with-html/../../browse/test-with-html/tests/"
                    (map (λ (hash)
                           (string-append
                            "/test-class/feedback/test-with-html/../../review/" hash "/"))
                         hashes)))
                  x)))))
      ;; bogus hash:
      ((,stu1 ,(path2list "review/598109a435c52dc6ae10c616bcae407a"))
       403
       bogus-review)
      ;; viewing a bogus feedback
      ((,stu1 ("feedback" "file-container" "BOGUSSS" "ALSOBAD" "load"))
       403
       bogus-file-container)
      ;; thunk to delay extraction of hash:
      (,(λ () (list stu1 (list "review" (lastreview stu1))))
       ;; FIXME there's a *space* in there? and in the iframe link too?
       (200 ,(λ (x)
               (begin
                 ((has-anchor-links
                   (list (string-append
                          "/test-class/review/" (lastreview stu1)
                          "/../../review/submit/" (lastreview stu1) "/ ")))
                  x)
                 ((has-iframe-link
                   (string-append
                    "/test-class/review/" (lastreview stu1)
                    "/../../file-container/" (lastreview stu1) " "))
                  x)))))
      ;; the iframe...
      (,(λ () (list stu1 (list "file-container" (lastreview stu1))))
       (200 ,(λ (r)
               ;; nasty hack here because of nondeterminism; don't know whether
               ;; file name will be file-1 or grogra-2.
               (define (make-links filename)
                 (list
                  (string-append "/test-class/file-container/" (lastreview stu1) "/" filename)
                  (string-append "/test-class/file-container/" (lastreview stu1) "/download/" filename)))
               (define links-1 (make-links "file-1"))
               (define links-2 (make-links "grogra-2"))
               (check-pred
                (λ (x) (or ((has-anchor-links/bool links-1) x)
                           ((has-anchor-links/bool links-2) x)))
                r))))
      ;; stu2 logs in:
      ((,stu2 ())
       (200 ,(has-anchor-links '("/test-class/assignments/"))))
      ;; clicks on assignments
      ((,stu2 ("assignments"))
       (200 ,(has-anchor-links '("/test-class/assignments/../feedback/test-with-html/"))))
      ;; stu2 publishes:
      ((,stu2 ,(path2list "submit/test-with-html/tests")
              (alist ((action . "submit")))
              #t)
       (200 ,(has-anchor-links
              ;; FIXME yucky urls
              '("/test-class/submit/test-with-html/tests/../../../feedback/test-with-html/")))
       stu2-publishes)
      ((,stu2 ("feedback" "test-with-html")) 200)
      ;; stu2 clicks on last review
      (,(λ () (list stu2 (list "review" (lastreview-of stu2 stu1))))
       200
       review)
      ;; load review file-container for directory
      (,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1))))
       200
       review-iframe-dir)
      ;; file-container for file
      (,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1)
                               "my-different-file")))
       (200 ,(has-anchor-links '("./")))
       review-iframe-file)
      ;; actual text of file
      (,(λ () (list stu2 (list "file-container" (lastreview-of stu2 stu1) "download"
                               "my-different-file")))
       200
       review-iframe-file-content)
      ;; actual text of file using new endpoint:
      (,(λ () (list stu2 (list "download" (lastreview-of stu2 stu1) "my-different-file")))
       200
       review-iframe-file-content-new)
      ;; should it be an error to submit bogus rubric json?
      (,(λ () (list stu2 (list "review" (lastreview-of stu2 stu1) "tests" "save")
                    (list 'json #"\"abcd\"")
                    #t))
       200)
      (,(λ () (list stu2 (list "review" "submit" (lastreview-of stu2 stu1))))
       200
       stu2-submits-review1)
      ;; do the other review too
      (,(λ () (list stu2 (list "review" (lastreview stu2) "tests" "save")
                    (list 'json #"\"abcde\"")
                    #t))
       200)
      (,(λ () (list stu2 (list "review" "submit" (lastreview stu2))))
       (200 ,(λ (r)
               ;; oog, this is awful:
               (check
                ormap-xexp
                (λ (e) (match e
                         [(list
                           'a (list
                               '@ _1 ...
                               (list 'href
                                     (regexp #px"feedback/test-with-html/$"))
                               _2 ...)
                           _3 ...)
                          #t]
                         [other #f]))
                (match r
                  [(list _1 ... response-str)
                   (html->xexp response-str)]))
               #;((has-anchor-links
                   ;; FIXME yucky url, hash not even necessary
                   (list (string-append
                          "/test-class/review/submit/" (lastreview stu2)
                          "/../../../feedback/test-with-html/")))
                  r)))
       stu2-submits-review2)
      ;; stu1 now views it
      (,(λ () `(,stu1 ("feedback" "view" ,(firstfeedback stu1))))
       200
       stu1-views-review)
      (,(λ () `(, stu1 ("feedback" "file-container" ,(firstfeedback stu1))))
       200
       stu1-views-review-fc-dir)
      (,(λ () `(, stu1 ("feedback" "file-container" ,(firstfeedback stu1) "my-different-file")))
       200
       stu1-views-review-fc-file)
      (,(λ () `(, stu1 ("download" ,(firstfeedback stu1) "my-different-file")))
       200
       stu1-views-review-fc-file-raw)
      (,(λ () `(,stu1 ("feedback" "view" ,(firstfeedback stu1))
                      ((feedback . "feedback with <i>italics</i>.")
                       (flag . "goronsky"))
                      #t))
       (200 ,no-italics)
       stu1-submits-feedback-xss)
      ((,stu2 ("feedback" "test-with-html")) 200)))

  ;; RIGHT HERE still working on adding these urls:
  '((61
   stu1-views-review
   ("frogstar@example.com"
    ("feedback" "view" "39b6494b028ae30c4e5a9a6829c3deec"))
   ())
  (62
   stu1-views-review-fc-dir
   ("frogstar@example.com"
    ("feedback" "file-container" "39b6494b028ae30c4e5a9a6829c3deec"))
   ("/test-class/feedback/file-container/39b6494b028ae30c4e5a9a6829c3deec/my-different-file"))
  (63
   stu1-views-review-fc-file
   ("frogstar@example.com"
    ("feedback"
     "file-container"
     "39b6494b028ae30c4e5a9a6829c3deec"
     "my-different-file"))
   ("./"))
  (64
   stu1-views-review-fc-file-raw
   ("frogstar@example.com"
    ("download" "39b6494b028ae30c4e5a9a6829c3deec" "my-different-file"))
   ())
  (65
   stu1-submits-feedback-xss
   ("frogstar@example.com"
    ("feedback" "view" "39b6494b028ae30c4e5a9a6829c3deec")
    ((feedback . "feedback with <i>italics</i>.") (flag . "goronsky"))
    #t)
   ())
  (66
   #f
   ("mf2@example.com" ("feedback" "test-with-html"))
   ("/test-class/feedback/test-with-html/../../browse/test-with-html/tests/"
    "/test-class/feedback/test-with-html/../../review/39b6494b028ae30c4e5a9a6829c3deec/"
    "/test-class/feedback/test-with-html/../../review/693d7b007951c71a53a6c7017bc96ee0/")))

  ;; check that no two tests have the same name
  (check-false
   (check-duplicates (apply
                      append
                      (map (λ (t) (match t
                                    [(list a b c) (list c)]
                                    [other '()]))
                           tests))))
  
  ;; return the last pending review for given student on "test-with-html"
  (define (lastreview uid)
    (last (pending-review-hashes (cons "test-with-html" uid))))

  ;; return the first pending review for given student on "test-with-html"
  (define (firstreview uid)
    (first (pending-review-hashes (cons "test-with-html" uid))))

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
              (unless (ct-test? test)
                (raise-argument-error 'testing
                                      "test specification matching contract"
                                      0 test))
              (define-values (expected request-args-or-thunk testname)
                (match test
                  [(list call expected) (values expected call #f)]
                  [(list call expected name) (values expected call name)]))
              (define request-args
                ;; !@#$ request hashes... can't extract until earlier tests have been
                ;; run.
                (cond [(procedure? request-args-or-thunk) (request-args-or-thunk)]
                      [else request-args-or-thunk]))
              (define result (apply run-request request-args))
              (unless (and ignore-bad-in-original?
                           (member testname known-bad-in-original))
                (test-case
                 (format "~s" (list i testname request-args))
                 (match expected
                   [(? number? code)
                    (check-equal? (first result) code)]
                   [(list (? number? code)
                          (? procedure? test-proc))
                    (begin (check-equal? (first result) code)
                           (test-proc result))])))
              (define output-val (list i testname request-args result))
              (fprintf r-port "~s\n" output-val)
              (fprintf rt-port "~s\n" output-val)
              #;(printf "~s\n" output-val))))))))

  
  (sleep 1)
  )


