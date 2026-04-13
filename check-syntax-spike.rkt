#lang racket/base

;; Spike: Can we get check-syntax annotations in-process?
;; Run: racket check-syntax-spike.rkt
;; Expected: binding arrows, mouse-over tooltips, text types, etc.

(require drracket/check-syntax
         racket/class
         racket/pretty
         racket/list
         racket/file)

(define sample-code
  #<<CODE
#lang racket/base
(require racket/list)

(define (greet name)
  (string-append "hello, " name))

(define xs '(1 2 3))
(define ys (map add1 xs))
(displayln (greet "rktlens"))
(displayln (filter odd? ys))
CODE
  )

(define sample-file
  (let ([p (make-temporary-file "rktlens-spike-~a.rkt")])
    (call-with-output-file p #:exists 'truncate
      (lambda (out) (display sample-code out)))
    p))

(printf "analyzing: ~a~n~n" sample-file)
(define annotations (show-content sample-file))
(delete-file sample-file)

(define grouped (group-by (lambda (v) (vector-ref v 0)) annotations))

(for ([g (in-list grouped)])
  (define tag (vector-ref (car g) 0))
  (printf "── ~a  (~a)~n" tag (length g))
  (for ([v (in-list (take g (min 5 (length g))))])
    (printf "   ~a~n" v))
  (when (> (length g) 5)
    (printf "   ... ~a more~n" (- (length g) 5)))
  (newline))

(printf "total annotations: ~a~n" (length annotations))
