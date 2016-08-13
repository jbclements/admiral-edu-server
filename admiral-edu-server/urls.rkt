#lang typed/racket/base

(require racket/string
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

;; also, Windows is going to invalidate many of the assumptions
;; made in this code.

(provide (struct-out Url-Path)
         strs->rel-url-path
         strs->abs-url-path
         url-path-join
         url-path->url-string
         url-path->string
         legal-path-elt?
         construct-url-path)

;; in order to make this code work on Windows we'd have to think
;; harder about paths and how to construct them, and the mapping
;; between file system paths and AWS paths.

;; FIXME honestly, 'macos has a case-insensitive FS by default,
;; which will mess things up completely.
(when (eq? (system-type 'os) 'windows)
  (error 'system-type
         "this code doesn't work on Windows. Sorry."))

;; for our purposes, a url path is represented as a list of
;; strings that don't contain nuls and slashes, plus a boolean
;; indicating whether it's an absolute path
(struct Url-Path ([elts : (Listof String)]
                  [abs? : Boolean])
  #:transparent)

;; turn a list of strings into a relative url path
(: strs->rel-url-path ((Listof String) -> Url-Path))
(define (strs->rel-url-path strs)
  (strs->url-path strs #f))

;; turn a list of strings into an absolute url path
(: strs->abs-url-path ((Listof String) -> Url-Path))
(define (strs->abs-url-path strs)
  (strs->url-path strs #t))

(: strs->url-path ((Listof String) Boolean -> Url-Path))
(define (strs->url-path strs absolute?)
  (unless (andmap legal-path-elt? strs)
    (raise-argument-error 'strs->url-path
                          "list of path element strings"
                          0 strs))
  (Url-Path strs absolute?))

;; is this a legal element in a url path?
;; FIXME check rfc
(: legal-path-elt? (String -> Boolean))
(define (legal-path-elt? str)
  (and (andmap (λ (ch)
                 (not (member ch bad-chars)))
               (string->list str))
       (not (string=? str ""))))

;; nuls are not legal in most strings because of C.
;; slashes are not legal because they will be interpreted
;;  as path dividers
(: bad-chars (Listof Char))
(define bad-chars (list #\nul #\/))

;; join two url paths. The second one can't be absolute
(: url-path-join (Url-Path Url-Path -> Url-Path))
(define (url-path-join a b)
  (when (Url-Path-abs? b)
    (raise-argument-error 'url-path-join
                          "relative URL path"
                          1 a b))
  (Url-Path (append (Url-Path-elts a) (Url-Path-elts b))
            (Url-Path-abs? a)))

;; translate an absolute path to a string for use in HTML
;; FIXME check that browsers always use decoding here
(: url-path->url-string (Url-Path -> String))
(define (url-path->url-string a)
  (when (not (Url-Path-abs? a))
    (raise-argument-error 'url-path->string
                          "absolute URL path"
                          0 a))
  (string-join (map 
                uri-path-segment-encode
                (Url-Path-elts a)) "/"
               #:before-first "/"))

;; translate an absolute path to a string for use as a filesystem
;; path
(: url-path->string (Url-Path -> String))
(define (url-path->string a)
  (when (not (Url-Path-abs? a))
    (raise-argument-error 'url-path->string
                          "absolute URL path"
                          0 a))
  (path->string
   (apply build-path "/" (Url-Path-elts a))))



;; construct an absolute url from a session
(: session->base-url-path (ct-session -> Url-Path))
(define (session->base-url-path ct-session)
  (define maybe-su-uid (ct-session-su-from-uid ct-session))
  (define maybe-su-segment
    (cond [maybe-su-uid (list "su" (ct-session-uid ct-session))]
          [else (list)]))
  (Url-Path (cons (ct-session-class ct-session) maybe-su-segment)
            #t))

;; given a session and a list of strings, construct a url-path
(: construct-url-path (ct-session (Listof String) -> Url-Path))
(define (construct-url-path session path-elts)
  (url-path-join
   (session->base-url-path session)
   (strs->rel-url-path path-elts)))


(module+ test
  (require typed/rackunit)

  (check-equal? (strs->rel-url-path (list "b" "ohu:t"))
                (Url-Path (list "b" "ohu:t") #f))

  (check-exn #px"path element strings" 
             (λ ()
               (strs->rel-url-path (list "b" "ohu/t"))))

  (check-equal? (strs->abs-url-path (list "b" "ohu:t"))
                (Url-Path (list "b" "ohu:t") #t))

  (check-equal? (url-path-join (Url-Path (list "a" "b") #t)
                               (Url-Path (list "a1" "b2") #f))
                (Url-Path (list "a" "b" "a1" "b2") #t))

  (check-equal? (url-path-join (Url-Path (list "a" "b") #f)
                               (Url-Path (list "a1" "b2") #f))
                (Url-Path (list "a" "b" "a1" "b2") #f))

  (check-exn #px"relative URL path"
             (λ ()
               (url-path-join (Url-Path (list "a" "b") #f)
                              (Url-Path (list "a1" "b2") #t))))

  (check-equal? (url-path->string (Url-Path (list "b" "ohu:t") #t))
                "/b/ohu:t")

  (check-equal? (url-path->url-string (Url-Path (list "b" "ohu;t") #t))
                "/b/ohu%3Bt")

  (check-exn #px"absolute URL path"
             (λ () (url-path->string (Url-Path (list "b" "ohu:t") #f))))

  (check-equal? (session->base-url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    #f
                                                    (hash)))
                (Url-Path (list "bogusclass") #t))

  (check-equal? (session->base-url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    "user2@foo.com"
                                                    (hash)))
                (Url-Path (list "bogusclass"
                          "su"
                          "user1@foo.com")
                          #t)))