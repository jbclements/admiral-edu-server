#lang racket/base

;; this file is just to allow interactive testing of the server
;; without apache or docker
(module+ main
  (require racket/match
           web-server/servlet-dispatch
           (prefix-in sequence:
                      web-server/dispatchers/dispatch-sequencer)
           (prefix-in file:
                      web-server/dispatchers/dispatch-files)
           web-server/dispatchers/filesystem-map
           web-server/web-server
           "../dispatch.rkt"
           "../base.rkt"
           "../auth/google-openidc.rkt"
           "testing-shim.rkt")

  (define STATIC-FILES-ROOT
    "/Users/clements/admiral-edu/html")
  
  ;; config and delete everything in the database
  (init-shim)
  
  ;; delete local files
  (delete-local-files-shim)

  ;; treat all requests as coming from the master user.
  ;; (Use 'su' requests to model requests from other users.)
  (fixed-user (master-user))
  
  (let ((result (initialize)))
    (when (Failure? result)
      (error (format "Could not initialize system: ~a\n"))))

  (define stop
    (serve #:dispatch
           (sequence:make (file:make
                           #:url->path
                           (make-url->path
                            STATIC-FILES-ROOT))
                          (dispatch/servlet ct-rules-2))
           #:port (ct-port)))
  
  (require "../util/roster.rkt")
  (require "../authoring/assignment.rkt")
  
  (require "../storage/storage.rkt")
  
  (require (only-in "../database/mysql/assignment.rkt"
                    open))
  (define (run-some-setup)
  (printf "Adding student to roster\n")

  (register-uid "user1@example.com")
  (register-uid "user2@example.com")

  (printf "Adding Assignment 1\n")
  (define ASSIGNMENT-ID "a1-ct")
  (define STEP-ID "tests")
  (define REVIEW-ID "student-reviews")
  (define yaml-data
    (bytes-append
    #"name: Assignment 1 Captain Teach
id: " (string->bytes/utf-8 ASSIGNMENT-ID) #"
description: Problem 3.3.3 Solution
steps:
  - id: " (string->bytes/utf-8 STEP-ID) #"
    instructions: \"Submit your solution to problem 3.3.3\"
    reviews:
        - student-submission:
            id: " (string->bytes/utf-8 REVIEW-ID) #"
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
"))
  (yaml-bytes->create-or-save-assignment yaml-data #t)
  (printf "Adding dependencies\n")
  (upload-dependency-solution
   (class-name)
   "default-submission-student-reviews-1"
   ASSIGNMENT-ID STEP-ID "barbar"
   #"abc\ndef334")
  (upload-dependency-solution
   (class-name)
   "default-submission-student-reviews-2"
   ASSIGNMENT-ID STEP-ID "zigzag"
   #"a\nb\nc\n\nd")
  (printf "Opening assignment.\n")
  (open "a1-ct" (class-name))

  (printf "Submitting text for user 1\n")
  (upload-submission "test-class" "user1@example.com" "a1-ct" "tests" "my-file"
                     #"foo </textarea> bar <textarea> baz")

  (printf "Submitting text for user 2\n")
  (upload-submission "test-class" "user2@example.com" "a1-ct" "tests" "my-file"
                     #"this is the file submitted by user 2")
  
  ;; stop here if you want to see the user 1 submission
  ;; in the codemirror window.
  (printf "Finished setup.\n"))
  
  
  (print "Server Started. Type `stop` to kill the server.")
  (newline)
  (flush-output)

  
  
  (define (block)
    (let ((input (read)))
      (if (equal? 'stop input) (stop) (block))))
  
  (block))
