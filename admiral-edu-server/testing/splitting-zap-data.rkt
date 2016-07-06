#lang racket

(require math/statistics)

;; looks like it might be a good place to split input???
(define splitter
  (byte-regexp (regexp-quote (bytes #x01 #x55 #xbc))))

(define stuff
  (call-with-input-file "/tmp/data"
    (λ (port)
      (regexp-split splitter port))))

(define (find-first-http bytes)
  (regexp-match-positions #px"http[-:/.\\w]+" bytes))

(define-values (have-http bads)
  (partition find-first-http stuff))

(define bads-norepeat (remove-duplicates bads))

(map bytes-length bads-norepeat)


(define http-posns (map find-first-http have-http))

(unless (andmap (λ (n) (< n 50)) (map caar http-posns))
  (error 'late-http))

(samples->hash (map caar http-posns))

(length have-http)

(define 1002-regexp #px#"\1\0\0..(G|P)")

;; after about 720, they're all from the tester
(define CHOP 720)

(define 1002-posns
  (for/list ([chunk (in-list (take have-http CHOP))])
    (regexp-match-positions* 1002-regexp
                             chunk)))

(define missing-1002
  (filter (λ (chunk) (not (regexp-match-positions 1002-regexp chunk)))
          have-http))

(length 1002-posns)

(unless (= 0 (length (filter not 1002-posns)))
  (error 'some-missing-1002))

(remove-duplicates (map length 1002-posns))

;; we want the second hit, I believe:
(define right-match (map second 1002-posns))

(define segmenteds
  (for/list ([r (in-list right-match)]
             [c (in-list have-http)])
    (define request-and-rest (subbytes c (+ (car r) 5)))
    (define end-of-input-posn
      (regexp-match-positions #px#"\01"
                              request-and-rest))
    (define request (subbytes request-and-rest 0 (caar end-of-input-posn)))
    (define rest (subbytes request-and-rest (caar end-of-input-posn)))
    (define response-begin-posn (regexp-match-positions #px"HTTP/1\\.1 "
                                                        rest))
    (cond [response-begin-posn
           (list request
                 (subbytes rest 0 (caar response-begin-posn))
                 ;; not too crazy long, please...
                 (subbytes
                  (subbytes rest (caar response-begin-posn))
                  0 50))]
          [else
           (list request
                 #f
                 (subbytes rest 0 50))])))

(define qmatches
  (for/list ([s (in-list segmenteds)])
    (define query-lines
      (regexp-split #px#"\r\n" (first s)))
    (list
     (rest
      (regexp-match #px#"^(GET|POST) ([^ ]+) HTTP/1.1$"
                    (first query-lines)))
     (regexp-match #px"\r\nCookie: mod_auth_openidc_session=[^\r]+\r\n" (first s))
     query-lines
     (rest s))))

(define captain-teaches
  (filter (λ (s) (regexp-match #px#"^https://www\\.captainteach\\.org/2166-dev"
                               (second (first s))))
          qmatches))

(define grouped-by-cookie
  (let loop ([remaining captain-teaches]
             [prev-cookie 'not-a-cookie]
             [current-group '()])
    (cond [(empty? remaining) (list (reverse current-group))]
          [else
           (define f (first remaining))
           (cond [(equal? (second f) prev-cookie)
                  (loop (rest remaining)
                        prev-cookie
                        (cons f current-group))]
                 [else
                  (cons (reverse current-group)
                        (loop (rest remaining)
                              (second f)
                              (list f)))])])))

(map length grouped-by-cookie)

(define master-setup (third grouped-by-cookie))

(for/list ([request (in-list master-setup)])
  (define maybe-content-type
    (findf (λ (l) (regexp-match #px#"^Content-Type: " l)) (third request)))
  (define maybe-content-length
    (findf (λ (l) (regexp-match #px#"^Content-Length: " l)) (third request)))
  (match (caar request)
    [#"GET"
     (cond
       [(or maybe-content-type maybe-content-length)
        (error 'get-had-content "~e" request)]
       [else
        (first request)])]
    [#"POST"
     (cond [(or (not maybe-content-type) (not maybe-content-length))
            (error 'post-had-no-content "~e" request)]
           [else
            (define content-length
              (string->number
               (bytes->string/utf-8
                (second
                 (regexp-match #px"^Content-Length: ([0-9]+)$"
                               maybe-content-length)))))
            (list
             (first request)
             maybe-content-type
             (subbytes (first (fourth request))
                       5 (+ 5 content-length)))])
     ]))




#;((define first-https
  (for/list ([chunk (in-list have-http)])
    (regexp-match #px#"https://www\\.captainteach\\.org/2166-dev[-:/.\\w]+" chunk)))

(length (filter (λ (x) x) first-https))

(take (filter (λ (x) x) first-https) 10)

(call-with-output-file "/tmp/allpaths.txt"
  #:exists 'truncate
  (λ (port)
    (for ([p (in-list (map first (filter (λ (x) x) first-https)))])
      (fprintf port "~a\n" p)))))

