#lang typed/racket/base

;; this file provides functions to construct responses, and
;; provides the Response type

(require "typed-xml.rkt")

(require/typed web-server/servlet
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
               [#:opaque Response response?]
               [TEXT/HTML-MIME-TYPE Bytes])

(provide response/full
         response/xexpr
         xexprs->response
         string->response
         Response
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
