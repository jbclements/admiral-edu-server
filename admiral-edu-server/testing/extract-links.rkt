#lang racket/base

(require xml
         racket/match
         racket/list
         rackunit)

(provide extract-html-links
         extract-xexpr-links)

;; extract links from html, needed for testing


;; given a string representing an html page, return a list of the
;; "href" targets embedded in 'a' tags
(define (extract-html-links html)
  (extract-xexpr-links
   ;; top tag wrapping just ensures that multiple xexprs are okay.
   (string->xexpr (string-append "<top>" html
                                 "</top>"))))

;; given an xexpr, return the hrefs associated with the 'a' tags.
;; would have been near-trivial with sxpath, but then I would have
;; had to import sxml...
(define (extract-xexpr-links xexpr)
  (match xexpr
    [(list-rest 'a (? attr-list? attrs) rest)
     (append (extract-attr-list-hrefs attrs)
             (apply append (map extract-xexpr-links rest)))]
    [(list-rest (? symbol? _) (? attr-list? _) rest)
     (apply append (map extract-xexpr-links rest))]
    [(list-rest (? symbol? _) rest)
     (apply append (map extract-xexpr-links rest))]
    [other (list)]))

;; given an attr list, return the links associated with the hrefs.
;; there shouldn't be more than one, but this function doesn't enforce
;; that.
(define (extract-attr-list-hrefs attrs)
  (map second (filter (λ (pr) (eq? (first pr) 'href)) attrs)))

;; is this an attribute list? In an xexpr, this just means a list that is empty or whose
;; first element is a list.
(define (attr-list? l)
  (and (list? l) (or (empty? l)
                     (list? (first l)))))

(check-equal?
 (extract-html-links
  (string-append
   "<h1>test-with-html</h1><h2>Next Required Action</h2><p>You must complete "
   "pending reviews before you can proceed to the next step.</p><h2>Browse "
   "Submissions</h2> <p>The links below are to the submissions you've made for "
   "this assignment.</p> <li><a href="
   "\"/test-class/feedback/test-with-html/../../browse/test-with-html/tests/\">"
   "tests</a></li><h2>Pending Reviews</h2> <p>The links below are to reviews "
   "that you have not yet completed. As you work on them, they automatically save. "
   "If you want, you may work on them and come back later. Once you are satisfied "
   "with your review, press submit to send it to the author. Once you have "
   "submitted, you may not make additional changes.</p> <li><a href="
   "\"/test-class/feedback/test-with-html/../../review/44abc1ddc5644699e73450958033d54b/\">"
   "Pending Review for 'tests'</a></li> <li><a href="
   "\"/test-class/feedback/test-with-html/../../review/893776e42ea47c100cd04a9842b42382/\">"
   "Pending Review for 'tests'</a></li><h2>Completed Reviews</h2> <p>You have "
   "not completed any reviews.</p><h2>Review Feedback</h2><p>You have not "
   "received any feedback for this assignment.</p>"))
 (list "/test-class/feedback/test-with-html/../../browse/test-with-html/tests/" 
       "/test-class/feedback/test-with-html/../../review/44abc1ddc5644699e73450958033d54b/"
       "/test-class/feedback/test-with-html/../../review/893776e42ea47c100cd04a9842b42382/"))