#lang typed/racket/base

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
