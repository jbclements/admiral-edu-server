#lang typed/racket/base

(require racket/string
         racket/match
         "ct-session.rkt"
         "configuration.rkt")

(require/typed net/uri-codec
               [uri-path-segment-encode (String -> String)])


;; captain teach uses HTML references that take the form of URI paths,
;; such as "/test-class/download/129384727482/foo/bar". Many of
;; these paths contain elements that will be used as file system
;; paths, in this case e.g. "foo" and "bar". It is therefore
;; incumbent on us to restrict these to strings that can be
;; encoded in both URI paths and used as UNIX filenames and used
;; as AWS filenames.

;; unfortunately, many of these are provided either by the instructor
;; or by the student, in the form of assignment identifiers or
;; in the form of file names.

;; for the assignment and step names, we can lean on the instructors
;; to conform to a fairly narrow scheme: we allow numbers, digits,
;; hyphens, and that's it. For file names, though, we can't be so
;; restrictive. Specifically, we'd like to allow spaces, and once
;; you've allowed spaces, you're sort of into the soup.

;; As a compromise, we're allowing spaces and all printing characters
;; except for forward and back slashes, and that's it. And no ending
;; with a space. And "." and ".." (see below).

;; Well, it turns out that's just the first part of the puzzle.
;; After deciding what tokens users can use, you have to figure
;; out how to encode them in three ways.

;; first, as paths. There we're doing all right, because as long
;; as your path elements don't include slashes or whitespace,
;; you're on pretty solid ground, EXCEPT that the ids "." and
;; ".." have special meanings for paths, so we'll just rule
;; them out. Erm, except that paths that end with a space are
;; probably a bad idea. those are out.

;; second, as S3 paths. Here again I think we're fine, because
;; S3 paths are (AFAICT) just strings. So, staying away from
;; slashes should allow us to concatenate them using slashes
;; and Bob's your uncle.

;; third, in HTML. This one is hellish.  Specifically, there
;; are two totally separate structural mechanisms that you have
;; to respect within HTML. First, your path elements must make
;; up a legal URL. This means that putting a question mark in
;; a path name is a big no-no, because it will cause a URL
;; parser to believe that we've ended the "path" part of the
;; URL and entered the "query" part of the URL. Second, the
;; encoded url can't have things like double-quotes, because
;; the resulting string is getting embedded into an HTML context
;; where a double-quote is interpreted as the end of the
;; string. Embedding them in Javascript raises the same issues.

;; Fortunately, it will turn out that our solution to problem
;; one will also solve problem two; specifically, we perform
;; 'uri-path-segment-encode'ing to change e.g. space characters
;; into %20. This same encoding also changes '<' into %3C and
;; '"' into %22, so the second form of encoding should always
;; be the identity.

;; Note an interesting thing about this process: the decoding
;; is *not* injective. Specifically, "abc%20def" and "abc def"
;; both decode to the same string. This should raise danger
;; flags all over the case. The saving grace, IIUC, is that
;; decode composed with encode *is* the identity.

;; The browser plays an interesting role here. Specifically,
;; the browser *does* peform HTML decoding, to turn (e.g.)
;; &quot; back into double-quote. However, whether the browser
;; performs uri-path-segment decoding is more or less a moot
;; point, because of course it has to send the URL back to
;; the server using a TCP connection, which means that no matter
;; how it represents it internally, it must perform uri-path-
;; segment encoding in order to get the URL back out on the
;; wire. In fact, sending a browser a URL with a &quot; in it
;; will result in a request that contains %22 instead.

;; In order to stay out of danger, then, we need to make sure
;; that our URLs are properly encoded. As long as this is
;; true, it appears that the browser will send back the
;; URL in the same form.

;; what about JavaScript? We need to make sure that AJAX
;; calls, $.get() and $.post(), work correctly. My guess...
;; well, let's check. <goes and checks>. Well, I was surprised.
;; it appears that JS does the same thing that the browser
;; does: it does a decode and then an encode. This means
;; that as long as your strings are properly encoded to
;; begin with, you're fine.

;; we also define a ct-basic-id?, which is basically just
;; a-z,A-Z,0-9, hyphens, and underscores. These are pretty safe to put in
;; URLs anywhere, without worrying about encoding.

;; FIXME: are these (ct-id's) safely encoded in DB conversion?
;; specifically: are hyphens and underscores both mapped to underscores?

;; OKAY

;; so what do we need to do in the server?

;; First, it turns out that the URL parsing mechanism built
;; into the Racket libraries will *already* perform the
;; uri-path-segment decoding. (This is probably already
;; the source of existing XSS attacks, yikes.) This means
;; that we don't need to do decoding. In fact, performing
;; decoding again would open us up to a well-known source
;; of XSS attacks known as "double-decode" attacks.

;; Instead, all we need to do is to be careful to perform
;; uri-path-segment encoding (exactly once) before embedding
;; a url in the output.

;; For those that are already Ct-Path's, this is pretty easy;
;; we just need to perform the encoding in the url-path->url-string
;; function.

;; However, for those that are still strings... we have a problem.
;; I'm torn as to whether to try to set up a temporary gross hack
;; or to tackle the problem head-on and just get rid of paths
;; as strings.






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
         Ct-Path?
         ct-id?
         basic-ct-id?
         rel-ct-path
         ct-url-path
         ct-url-path-/
         ct-path-/
         ct-path-join
         ct-path->url-path
         url-path?
         url-path->url-string
         ct-path->path
         path->ct-path
         abs-path->ct-path
         ct-path->emailable-url
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
  (unless (andmap ct-id? strs)
    (raise-argument-error 'strs->ct-path
                          "list of path element strings"
                          0 strs))
  (Ct-Path strs absolute? #f))

;; is this a legal element in a url path?
;; FIXME check rfc
(: ct-id? (Any -> Boolean))
(define (ct-id? str)
  (and (string? str)
       (only-good-chars? str)
       (not (regexp-match? #px" $" str))
       (not (member str bad-path-elts))))

;; does this string consist of only good characters?
;; (only spaces and [:graph:] - #\/ , #\', #\" #\\)
(: only-good-chars? (String -> Boolean))
(define (only-good-chars? b)
  (and (regexp-match? #px"^[ [:graph:]]*$" b)
       (not (regexp-match? #px"[/'\"\\\\]" b))))

;; these are not legal path elements.
(: bad-path-elts (Listof String))
(define bad-path-elts (list "" "." ".."))

;; is this string safe to use without encoding?
(: basic-ct-id? (Any -> Boolean))
(define (basic-ct-id? str)
  (and (string? str)
       (regexp-match? #px"^[-_A-Za-z0-9]+$" str)))

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

;; FIXME should be unneeded after completing conversion
;; to ct-paths
;; translate a relative path to a ct-path
(: path->ct-path (Path-String -> Ct-Path))
(define (path->ct-path p)
  (when (equal? p "")
    (raise-argument-error
     'path->ct-path
     "non-empty string or path"
     0 p))
  (define path-path
    (cond [(path? p) p]
          [else (string->path p)]))
  (cond [(not (relative-path? path-path))
         (raise-argument-error
          'path->ct-path
          "relative path" 0 p)]
        [else
         ;; first split-path is just to see if
         ;; the whole thing ends with a slash
         (define-values (_1 _2 ends-with-slash?) (split-path path-path))
         (define elements
           (reverse
            (let loop : (Listof String) ([path path-path])
              (define-values (base this _3) (split-path path))
              (when (or (eq? this 'up)
                        (eq? this 'same))
                (raise-argument-error
                 'path->ct-path
                 "path not containing . or .. "
                 0 p))
              (cons (path->string this)
                    (cond [(eq? base #f)
                           (error
                            'path->ct-path
                            "internal error: relative path wound up being a base path? ~v"
                            path-path)]
                          [(eq? base 'relative)
                           '()]
                          [else
                           (loop base)])))))
         (define rel-path
           (apply rel-ct-path elements))
         (cond [ends-with-slash?
                (ct-path-/ rel-path)]
               [else rel-path])]))

;; FIXME should be unneccessary after completing
;; transition to ct-paths
;; converts an absolute path-string to an absolute
;; Ct-path. (Note: it turns out to be surprisingly
;; difficult to turn this into a call to the previous
;; function.)
(: abs-path->ct-path (Path-String -> Ct-Path))
(define (abs-path->ct-path p)
  (when (equal? p "")
    (raise-argument-error
     'abs-path->ct-path
     "non-empty string or path"
     0 p))
  (define path-path
    (cond [(path? p) p]
          [else (string->path p)]))
  (cond [(relative-path? path-path)
         (raise-argument-error
          'path->ct-path
          "absolute path" 0 p)]
        [else
         ;; first split-path is just to see if
         ;; the whole thing ends with a slash
         (define-values (_1 _2 ends-with-slash?) (split-path path-path))
         (define elements
           (reverse
            (let loop : (Listof String) ([path path-path])
              (define-values (base this _3) (split-path path))
              (when (or (eq? this 'up)
                        (eq? this 'same))
                (raise-argument-error
                 'path->ct-path
                 "path not containing . or .. "
                 0 p))
              (cond
                [(and (eq? base #f) (equal? (path->string this) "/"))
                 '()]
                [(eq? base #f)
                 (error 'abs-path->ct-path
                        "unexpected return value from path-split")]
                [(eq? base 'relative)
                 (error 'abs-path->ct-path
                        "absolute path wound up being relative: ~e"
                        path-path)]
                [else
                 (cons (path->string this) (loop base))]))))
         (Ct-Path elements #t ends-with-slash?)]))

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

;; given a session and a list of strings, construct a url-path ending
;; with a slash
(: ct-url-path-/ (ct-session String * -> Ct-Path))
(define (ct-url-path-/ session . path-elts)
  (ct-path-/ (apply ct-url-path session path-elts)))

;; given a relative ct-path, generate a url string for use in
;; an email
(: ct-path->emailable-url (Ct-Path -> String))
(define (ct-path->emailable-url path)
  (string-append
   "https://"
   (sub-domain)
   (server-name)
   (url-path->url-string
    (ct-path-join
     (Ct-Path (list (class-name)) #t #t)
     path))))


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

  (check-equal? (path->ct-path (build-path "b" "ohu:t"))
                (Ct-Path (list "b" "ohu:t") #f #f ))

  (check-equal? (path->ct-path "b/ohu:t")
                (Ct-Path (list "b" "ohu:t") #f #f))

  (check-equal? (path->ct-path "b/ohu:t/")
                (Ct-Path (list "b" "ohu:t") #f #t))

  (check-exn #px"relative path"
             (λ () (path->ct-path "/b/ohu:t/")))

  (check-equal? (abs-path->ct-path "/b/ohu:t/")
                (Ct-Path (list "b" "ohu:t") #t #t))

  (check-exn #px"absolute path"
             (λ () (abs-path->ct-path "b/ohu:t/")))
  
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

  (check-equal? (ct-url-path (ct-session "bogusclass"
                                                    "user1@foo.com"
                                                    #f
                                                    (hash))
                             "a" "b")
                (Ct-Path (list "bogusclass" "a" "b") #t #f))

  (check-equal? (ct-url-path-/ (ct-session "bogusclass"
                                           "user1@foo.com"
                                           #f
                                           (hash))
                               "a" "b")
                (Ct-Path (list "bogusclass" "a" "b") #t #t))

  (check-equal? (only-good-chars? "") #t)
  (check-equal? (only-good-chars? "abcha###3;; 14!") #t)
  (check-equal? (only-good-chars? "abcha#%3f##3;; 14!") #t)
  (check-equal? (only-good-chars? "abcha#&3f##3;; 14!") #t)
  (check-equal? (only-good-chars? "ab\\cha###3;; 14!") #f)
  (check-equal? (only-good-chars? "abcha#'##3;; 14!") #f)
  (check-equal? (only-good-chars? "abcha#\"##3;; 14!") #f)
  (check-equal? (only-good-chars? "abc/ha###3;; 14!") #f)
  (check-equal? (only-good-chars? "abcha#\n##3;; 14!") #f)
  (check-equal? (ct-id? "abcha#\n##3;; 14!") #f)
  (check-equal? (ct-id? "abcha###3;; 14!") #t)
  (check-equal? (ct-id? "abcha###3;; 14! ") #f)
  (check-equal? (ct-id? ".") #f)
  (check-equal? (ct-id? "..") #f)

  (check-equal? (basic-ct-id? ".") #f)
  (check-equal? (basic-ct-id? "abcd-3h_th") #t)
  (check-equal? (basic-ct-id? "abc d-3hth") #f)
  (check-equal? (basic-ct-id? "") #f)

  (require "tests/test-configuration.rkt")

  (parameterize ([current-configuration
                  (modified-test-conf
                   (hash "sub-domain" "www."
                         "server-name" "zigbar.org"
                         "class-name" "zoop-dedoop"))])
    (define assignment-id "ass9")
    (check-equal?
     (ct-path->emailable-url (rel-ct-path "feed back" assignment-id))
     (string-append "https://" (sub-domain) (server-name) "/" (class-name) "/feed%20back/" assignment-id)))

  )