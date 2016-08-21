#lang typed/racket/base

;; this file provides functions to construct responses, and
;; provides the Response type

;; FIXME there's a certain amount of tension between this file
;; and templates; it appears that some pages use templates, and
;; some just go straight from xexprs to responses.

(require "typed-xml.rkt")

(require/typed web-server/servlet
               [#:opaque Response response?]
               ; (number? bytes? number?
               ; (or false bytes?) (listof header?) (listof bytes)
               ;  -> response?
               [response/full (Number Bytes Number (U False Bytes)
                                      (Listof Any) (Listof Bytes)
                                      -> Response)]
               [response/xexpr (XExpr
                                [#:code Natural]
                                [#:message Bytes]
                                -> Response)]
               
               [TEXT/HTML-MIME-TYPE Bytes])

(provide response/full ;; <- bad, don't use this
         response/xexpr
         bytes->file-response
         bytes->json-response
         xexprs->response
         string->response ;; <- bad, don't use this
         Response
         response?
         TEXT/HTML-MIME-TYPE)

;; wrap the given xexpr in html and body tags, map to
;; 200-okay response
(: xexprs->response ((Listof XExpr) -> Response))
(define (xexprs->response xexprs)
  (response/xexpr `(html (body ,@xexprs))))

;; FIXME ELIMINATE USES OF THIS FUNCTION (replace with xexprs)
(: string->response (String -> Response))
(define (string->response str)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   '()
   (list (string->bytes/utf-8 str))))

;; use to send a file back to the user
(: bytes->file-response (Bytes -> Response))
(define (bytes->file-response content)
  (response/full
   200 #"Okay"
   (current-seconds)
   #"application/octet-stream; charset=utf-8"
   '()
   (list content)))

;; use to send a JSON-encoded file back to the user
(: bytes->json-response (Bytes -> Response))
(define (bytes->json-response content)
  (response/full
   200 #"Okay"
   (current-seconds)
   #"application/json; charset=utf-8"
   '()
   (list content)))

