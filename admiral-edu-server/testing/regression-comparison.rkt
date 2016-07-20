#lang racket

(require rackunit
         html-parsing
         sxml
         sexp-diff )

;; this is very ad-hoc right now... looking for a way to compare
;; the output of two regression test run outputs.

(define pre-change-tests
  (call-with-input-file "regression-results-pre.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))

(define post-change-tests
  (call-with-input-file "/tmp/regression-results-tmp.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))


;; how to compare trees? We're using sxml, but we want to ignore
;; all of the extraneous whitespace that might not affect the
;; equality (e.g. in the <head> element). Unfortunately, this is
;; hard. So, as a non-conservative element, we just eliminate all
;; strings containing only whitespace. This may accidentally make
;; not-equal things into equal things, BUT not in a way that's
;; likely to arise as the result of a bug in our code. We hope.

;; given a single sxml element, remove all substrings that consist
;; entirely of whitespace
(define (sxml-eliminate-ws elt)
  (match elt
    [(cons (? symbol? tag) (cons (cons '@ attrs) rest))
     (cons tag (cons (cons '@ attrs)
                     (map sxml-eliminate-ws
                          (filter (compose not ws-string?) rest))))]
    [(cons (? symbol? tag) rest)
     (cons tag
           (map sxml-eliminate-ws
                (filter (compose not ws-string?) rest)))]
    [other other]))


;; is this a string consisting only of whitespace?
(define (ws-string? s)
  (and (string? s) (regexp-match #px"^[[:space:]]*$" s)))

(check-equal?
 (sxml-eliminate-ws
  '(*TOP* (html
           "\n" "\n" "  "
           (head "\n" "    "
                 (title " Captain Teach - Assignments ")
                 "\n" "  ")
           "\n" "\n" "\n"
           (body "\n" "  "
                 (h1 "Assignments")
                 "\n"
                 (p (a (@ (href "/test-class/author/")) "New Assignment"))
                 "\n"
                 (h2 "Open Assignments")
                 "\n"
                 (ul)
                 "\n"
                 (h2 "Closed Assignments")
                 "\n" (ul) "\n") "\n")))
 '(*TOP* (html
          (head (title " Captain Teach - Assignments "))
          (body (h1 "Assignments")
                (p (a (@ (href "/test-class/author/")) "New Assignment"))
                (h2 "Open Assignments")
                (ul)
                (h2 "Closed Assignments")
                (ul)))))

(define TESTS-OF-INTEREST '(1))

(for ([test-pre (in-list pre-change-tests)]
      [test-post (in-list post-change-tests)])
  (with-handlers ([exn:fail?
                   (λ (exn)
                     (fprintf (current-error-port)
                              "test meltdown on test: ~e"
                              test-pre)
                     #f)])
  
  (match-define (list i n args (list code-a code-msg-a ts-a encoding-pre
                                     headers-a str-pre))
    test-pre)
  (match-define (list _ _ _ (list code-b code-msg-b ts-b encoding-post
                                  headers-b str-post))
    test-post)
  (unless (equal? (take test-pre 2)
                  (take test-post 2))
    (error 'test-comparison
           "expected first 3 elements to be the same, got ~e and ~e"
           (take test-pre 2) (take test-post 2)))

  
    
  (test-case
   (~v (list i n args))
   ;; ignore differences in codes; these are
   ;; the subject of correctness tests:
   #;(check-equal? code-b code-a)
   #;(check-equal? code-msg-b code-msg-a)
   ;; ignore timestamp...
   (check-equal? encoding-post encoding-pre)
   ;; ignore headers...
   #;(check-equal? headers-b headers-a)
   ;; not clear how to parse these...
   (define parsed-post (sxml-eliminate-ws (html->xexp str-post)))
   (define parsed-pre (sxml-eliminate-ws (html->xexp str-pre)))

   (when (member i TESTS-OF-INTEREST)
   (printf "diff on test ~v: ~v\n"
           i
           (sexp-diff parsed-pre parsed-post)))
   (check-equal? parsed-post parsed-pre))))