#lang typed/racket/base

(require racket/string
         racket/match
         "ct-session.rkt")

(require/typed net/uri-codec
               [uri-path-segment-encode (String -> String)])


;; captain teach uses HTML references that take the form of URI paths,
;; such as "/test-class/download/129384727482/foo/bar". Many of
;; these paths contain elements that will be used as file system
;; paths, in this case e.g. "foo" and "bar". It is therefore
;; incumbent on us to restrict these to strings that can be
;; encoded in both URI paths and used as UNIX filenames and used
;; as AWS filenames.

;; first off, empty strings are almost certainly a bad idea. Also,
;; #\nul characters are just asking for trouble. Both of these are
;; out.

;; Fortunately, URIs have percent-encoding. This means that pretty
;; much any string should be all right.

;; on the UNIX side, it seems wise to avoid forward slashes.
;; also, path names of "." and ".." are off-limits.

;; really, though, tabs and newlines are just going to make everyone's
;; life hell. So, currently, we're going to allow all the characters
;; in [:graph:] *except* for forward slash, and we're going to
;; alow spaces.

;; also, Windows is going to invalidate many of the assumptions
;; made in this code.

;; the general expected flow here is that you create relative
;; paths using rel-ct-path, optionally join them using
;; ct-path-join, and then *either* convert them to url paths
;; using ct-path->url-path and url-path->url-string (two-step
;; conversion because I don't want to pass the session into
;; the page-maker), *or* to a relative filesystem path with
;; ct-path->path.

;; ooh, I want to be able to model paths that end with slashes.
;; I think the nicest way to do this is just to add another
;; boolean field to the structure.  

(provide Ct-Path
         legal-path-elt?
         rel-ct-path
         ct-url-path
         ct-path-/
         ct-path-join
         ct-path->url-path
         url-path?
         url-path->url-string
         ct-path->path
         strs->abs-ct-path/testing)

;; in order to make this code work on Windows we'd have to think
;; harder about paths and how to construct them, and the mapping
;; between file system paths and AWS paths.

(match (system-type 'os)
  ['windows
   (error 'system-type
          "this code doesn't work on Windows. Sorry.")]
  ['macosx
   (printf
    (string-append
     "**WARNING: running captain teach on a case-insensitive file system"
     " is WRONG WRONG WRONG. Use only for testing, and even then be"
     " alert.\n"))]
  ['unix
   'everything-is-okay]
  [other
   (error 'system-type "unrecognized system type: ~v" other)])


;; for our purposes, a captain teach path is represented as a list of
;; strings that don't contain nuls and slashes, plus a boolean
;; indicating whether it's an absolute path
(struct Ct-Path ([elts : (Listof String)]
                 [abs? : Boolean]
                 [trailing-slash? : Boolean])
  #:transparent)

;; as an approximation, a URL path must be a ct-path and must
;; be absolute. A more restrictive definition would require that
;; it have been constructed using ct-path->url-path; I'm not
;; ready for that yet.
(define (url-path? p)
  (and (Ct-Path? p) (Ct-Path-abs? p)))

;; turn a list of strings into a relative captain teach path
(: rel-ct-path (String * -> Ct-Path))
(define (rel-ct-path . strs)
  (strs->ct-path strs #f))

;; turn a list of strings into an absolute url path
;; exported but only for use in test creation. 
(: strs->abs-ct-path/testing ((Listof String) -> Ct-Path))
(define (strs->abs-ct-path/testing strs)
  (strs->ct-path strs #t))

;; create a ct-path, checking that strings are legal path
;; elements first
(: strs->ct-path ((Listof String) Boolean -> Ct-Path))
(define (strs->ct-path strs absolute?)
  (unless (andmap legal-path-elt? strs)
    (raise-argument-error 'strs->ct-path
                          "list of path element strings"
                          0 strs))
  (Ct-Path strs absolute? #f))

;; is this a legal element in a url path?
;; FIXME check rfc
(: legal-path-elt? (String -> Boolean))
(define (legal-path-elt? str)
  (and (only-good-chars? str)
       (not (member str bad-path-elts))))

;; does this string consist of only good characters?
;; (only spaces and [:graph:] - #\/ )
(: only-good-chars? (String -> Boolean))
(define (only-good-chars? b)
  (and (regexp-match? #px"^[ [:graph:]]*$" b)
       (not (regexp-match? #px"/" b))))

;; these are not legal path elements.
(: bad-path-elts (Listof String))
(define bad-path-elts (list "" "." ".."))

;; add a trailing slash to a path
(: ct-path-/ (Ct-Path -> Ct-Path))
(define (ct-path-/ a)
  (Ct-Path (Ct-Path-elts a)
           (Ct-Path-abs? a)
           #t))

;; join two url paths. The second one can't be absolute
(: ct-path-join (Ct-Path Ct-Path -> Ct-Path))
(define (ct-path-join a b)
  (when (Ct-Path-abs? b)
    (raise-argument-error 'url-path-join
                          "relative URL path"
                          1 a b))
  (Ct-Path (append (Ct-Path-elts a) (Ct-Path-elts b))
           (Ct-Path-abs? a)
           (Ct-Path-trailing-slash? b)))

;; given a ct-session and a relative path, prepend
;; an absolute path based on the session to the relative
;; path to obtain an absolute path.
(: ct-path->url-path (ct-session Ct-Path -> Ct-Path))
(define (ct-path->url-path session path)
  (ct-path-join
   (session->base-url-path session)
   path))

;; translate an absolute path to a string for use in HTML
;; FIXME check that browsers always use decoding here
(: url-path->url-string (Ct-Path -> String))
(define (url-path->url-string a)
  (when (not (Ct-Path-abs? a))
    (raise-argument-error 'url-path->string
                          "absolute captain teach path"
                          0 a))
  (define maybe-final-slash (cond [(Ct-Path-trailing-slash? a) "/"]
                                  [else ""]))
  (string-append
   (string-join (map 
                 uri-path-segment-encode
                 (Ct-Path-elts a)) "/"
                                   #:before-first "/")
   maybe-final-slash))

;; translate a relative ct-path to a relative path
(: ct-path->path (Ct-Path -> Path))
(define (ct-path->path a)
  (when (Ct-Path-abs? a)
    (raise-argument-error 'ct-path->path
                          "relative captain teach path"
                          0 a))
  (cond [(null? (Ct-Path-elts a))
         (raise-argument-error
          'ct-path->path
          "captain teach path with nonempty list of strings"
          0 a)]
        [else
         (define p (apply build-path (Ct-Path-elts a)))
         (cond [(Ct-Path-trailing-slash? a)
                (path->directory-path p)]
               [else p])]))



;; construct an absolute url from a session
(: session->base-url-path (ct-session -> Ct-Path))
(define (session->base-url-path ct-session)
  (define maybe-su-uid (ct-session-su-from-uid ct-session))
  (define maybe-su-segment
    (cond [maybe-su-uid (list "su" (ct-session-uid ct-session))]
          [else (list)]))
  (Ct-Path (cons (ct-session-class ct-session) maybe-su-segment)
            #t
            #f))

;; given a session and a list of strings, construct a url-path
(: ct-url-path (ct-session String * -> Ct-Path))
(define (ct-url-path session . path-elts)
  (ct-path->url-path session (apply rel-ct-path path-elts)))


(module+ test
  (require typed/rackunit)

  (check-equal? (rel-ct-path "b" "ohu:t")
                (Ct-Path (list "b" "ohu:t") #f #f))

  (check-exn #px"path element strings" 
             (λ ()
               (rel-ct-path "b" "ohu/t")))

  (check-equal? (strs->abs-ct-path/testing (list "b" "ohu:t"))
                (Ct-Path (list "b" "ohu:t") #t #f))

  (check-equal? (ct-path-join (Ct-Path (list "a" "b") #t #f)
                               (Ct-Path (list "a1" "b2") #f #f))
                (Ct-Path (list "a" "b" "a1" "b2") #t #f))

  (check-equal? (ct-path-join (Ct-Path (list "a" "b") #f #f)
                               (Ct-Path (list "a1" "b2") #f #f))
                (Ct-Path (list "a" "b" "a1" "b2") #f #f))

  (check-exn #px"relative URL path"
             (λ ()
               (ct-path-join (Ct-Path (list "a" "b") #f #f)
                              (Ct-Path (list "a1" "b2") #t #f))))

  ;; trailing slash should be lost:
  (check-equal? (ct-path-join (Ct-Path (list "a" "b") #f #t)
                               (Ct-Path (list "a1" "b2") #f #f))
                (Ct-Path (list "a" "b" "a1" "b2") #f #f))

  ;; trailing slash should be preserved:
  (check-equal? (ct-path-join (Ct-Path (list "a" "b") #f #f)
                               (Ct-Path (list "a1" "b2") #f #t))
                (Ct-Path (list "a" "b" "a1" "b2") #f #t))

  (check-equal? (ct-path->path (Ct-Path (list "b" "ohu:t") #f #f))
                (build-path "b" "ohu:t"))

  (check-equal? (ct-path-/ (Ct-Path (list "b " "zzok") #f #f))
                (Ct-Path (list "b " "zzok") #f #t))

  (check-equal? (ct-path->path (Ct-Path (list "b" "ohu:t") #f #t))
                (path->directory-path (build-path "b" "ohu:t")))

  (check-exn #px"relative captain teach path"
             (λ () (ct-path->path (Ct-Path (list "b" "ohu:t") #t #f))))

  (check-equal? (url-path->url-string (Ct-Path (list "b" "ohu;t") #t #f))
                "/b/ohu%3Bt")

  (check-equal? (url-path->url-string (Ct-Path (list "b" "ohu;t") #t #t))
                "/b/ohu%3Bt/")

  (check-equal? (session->base-url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    #f
                                                    (hash)))
                (Ct-Path (list "bogusclass") #t #f))

  (check-equal? (session->base-url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    "user2@foo.com"
                                                    (hash)))
                (Ct-Path (list "bogusclass"
                          "su"
                          "user1@foo.com")
                          #t
                          #f))

  (check-equal? (ct-path->url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    #f
                                                    (hash))
                                   (Ct-Path (list "a" "b") #f #f))
                (Ct-Path (list "bogusclass" "a" "b") #t #f))

  (check-equal? (only-good-chars? "") #t)
  (check-equal? (only-good-chars? "abcha###3;; 14!") #t)
  (check-equal? (only-good-chars? "abc/ha###3;; 14!") #f)
  (check-equal? (only-good-chars? "abcha#\n##3;; 14!") #f))