#lang racket/base

(require (for-syntax racket/base syntax/parse)
         ffi/unsafe/objc
         "ide.rkt"
         "repl.rkt"
         "project.rkt"
         "status-bar.rkt"
         racket/set
         racket/string
         syntax-color/racket-lexer)

(provide define-application window split-view outline-view project-view
         highlight-for-file
         text-view repl-view status-bar monospace racket-syntax)

;; ============================================================================
;; DSL macros — these all expand to plain function calls into ide.rkt.
;; No FFI happens here; the macros are just syntactic sugar.
;; ============================================================================

(define-syntax (define-application stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(begin
         (define (name) body)
         (run-app name))]))

(define-syntax (window stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:title title:expr))
              (~optional (~seq #:size (w:expr h:expr))))
        ...
        child:expr)
     #'(make-window (~? title #f) (~? w 800) (~? h 600) child)]))

(define-syntax (split-view stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:divider-style _style:expr))
              (~optional (~seq #:orientation orient:expr)))
        ...
        child:expr ...)
     #'(make-split-view (list child ...)
                        (not (equal? (~? orient 'horizontal) 'vertical)))]))

(define-syntax (outline-view stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:data data:expr))
              (~optional (~seq #:on-select on-select:expr))
              (~optional (~seq #:width width:expr)))
        ...)
     #'(make-table-view (~? data '()) (~? on-select #f) (~? width 250))]))

(define-syntax (project-view stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:root root:expr))
              (~optional (~seq #:on-select on-select:expr))
              (~optional (~seq #:width width:expr)))
        ...)
     #'(make-project-tree-view (~? root ".") (~? on-select #f) (~? width 250))]))

(define-syntax (status-bar stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:width width:expr))) ...)
     #'(make-status-bar (~? width 1400))]))

(define-syntax (repl-view stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:font font:expr))) ...)
     #'(make-repl-view (~? font #f))]))

(define-syntax (text-view stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:storage storage:expr))
              (~optional (~seq #:font font:expr))
              (~optional (~seq #:highlights highlights:expr)))
        ...)
     #'(make-text-view (~? storage #f) (~? font #f) (~? highlights #f))]))

;; ============================================================================
;; racket-syntax: a highlighter using Racket's own lexer.
;; Helix "beans" palette (see ide.rkt for the editor bg/fg).
;; ============================================================================

(define racket-keyword-strs
  (list->set
   '("define" "define-values" "define-syntax" "define-syntax-rule"
     "define-struct" "define-for-syntax" "define-syntaxes"
     "lambda" "λ" "case-lambda"
     "let" "let*" "letrec" "let-values" "let*-values" "letrec-values"
     "if" "when" "unless" "cond" "case" "else"
     "and" "or" "not" "begin" "begin0" "set!"
     "quote" "quasiquote" "unquote" "unquote-splicing"
     "require" "provide" "module" "module+" "module*" "#%module-begin"
     "syntax-rules" "syntax-case" "syntax-parse" "syntax-parser"
     "for" "for/list" "for/fold" "for/and" "for/or" "for/sum" "for/vector"
     "match" "match-lambda" "struct" "class" "new" "send"
     "parameterize" "with-handlers" "raise" "error")))

(define (coerce-lexeme x)
  (cond [(string? x) x]
        [(symbol? x) (symbol->string x)]
        [else #f]))

;; Islands Dark color scheme
(define c-comment (tell (hex->NSColor "#7a7e85") retain))
(define c-string  (tell (hex->NSColor "#6aab73") retain))
(define c-number  (tell (hex->NSColor "#2aacb8") retain))
(define c-keyword (tell (hex->NSColor "#cf8e6d") retain))
(define c-hashkw  (tell (hex->NSColor "#bf8b56") retain))

(define rainbow-colors
  (vector (tell (hex->NSColor "#cf8e6d") retain)    ; orange
          (tell (hex->NSColor "#c77dbb") retain)    ; purple
          (tell (hex->NSColor "#56a8f5") retain)    ; blue
          (tell (hex->NSColor "#6aab73") retain)))  ; green

(define (racket-syntax src)
  (define in (open-input-string src))
  (port-count-lines! in)
  (let loop ([acc '()] [depth 0])
    (define-values (lexeme type paren start end)
      (with-handlers ([exn:fail? (lambda _ (values #f 'eof #f #f #f))])
        (racket-lexer in)))
    (cond
      [(or (eq? type 'eof) (not start) (not end)) (reverse acc)]
      [else
       (define loc (- start 1))
       (define len (- end start))
       (define-values (color new-depth)
         (case type
           [(comment sexp-comment) (values c-comment depth)]
           [(string) (values c-string depth)]
           [(constant) (values c-number depth)]
           [(hash-colon-keyword) (values c-hashkw depth)]
           [(parenthesis)
            (cond
              [(equal? paren '|(|)
               (values (vector-ref rainbow-colors
                                   (modulo depth (vector-length rainbow-colors)))
                       (add1 depth))]
              [(equal? paren '|)|)
               (define d (max 0 (sub1 depth)))
               (values (vector-ref rainbow-colors
                                   (modulo d (vector-length rainbow-colors)))
                       d)]
              [else (values #f depth)])]
           [(symbol)
            (define s (coerce-lexeme lexeme))
            (values (if (and s (set-member? racket-keyword-strs s)) c-keyword #f)
                    depth)]
           [else (values #f depth)]))
       (loop (if color (cons (list color loc len) acc) acc)
             new-depth)])))

;; ---- YAML highlighter (regex-based) -----------------------------------------
(define c-yaml-key    c-keyword)
(define c-yaml-value  c-string)
(define c-yaml-bool   c-number)
(define c-yaml-number c-number)
(define c-yaml-comment c-comment)

(define (yaml-syntax src)
  (define lines (string-split src "\n" #:trim? #f))
  (let loop ([lines lines] [pos 0] [acc '()])
    (cond
      [(null? lines) (reverse acc)]
      [else
       (define line (car lines))
       (define line-len (string-length line))
       (define new-acc
         (cond
           ;; Comment line
           [(regexp-match-positions #rx"^(\\s*)#" line)
            => (lambda (m)
                 (define start (cdar m))
                 (cons (list c-yaml-comment (+ pos start) (- line-len start)) acc))]
           ;; Key: value
           [(regexp-match-positions #rx"^(\\s*)([^:#\\s][^:]*):" line)
            => (lambda (m)
                 (define key-start (caadr (cdr m)))
                 (define key-end (cdadr (cdr m)))
                 (define after-colon (add1 key-end))
                 (define rest (if (< after-colon line-len)
                                  (substring line after-colon)
                                  ""))
                 (define val-acc
                   (cons (list c-yaml-key (+ pos key-start) (- key-end key-start)) acc))
                 (cond
                   [(regexp-match-positions #rx"^\\s+(true|false|yes|no|null)\\s*$" rest)
                    => (lambda (vm)
                         (define vs (+ pos after-colon (caar (cdr vm))))
                         (define vl (- (cdar (cdr vm)) (caar (cdr vm))))
                         (cons (list c-yaml-bool vs vl) val-acc))]
                   [(regexp-match-positions #rx"^\\s+(-?[0-9]+(\\.[0-9]+)?)\\s*$" rest)
                    => (lambda (vm)
                         (define vs (+ pos after-colon (caar (cdr vm))))
                         (define vl (- (cdar (cdr vm)) (caar (cdr vm))))
                         (cons (list c-yaml-number vs vl) val-acc))]
                   [(regexp-match-positions #rx"^\\s+(\"[^\"]*\"|'[^']*')" rest)
                    => (lambda (vm)
                         (define vs (+ pos after-colon (caar (cdr vm))))
                         (define vl (- (cdar (cdr vm)) (caar (cdr vm))))
                         (cons (list c-yaml-value vs vl) val-acc))]
                   [else val-acc]))]
           [else acc]))
       (loop (cdr lines) (+ pos line-len 1) new-acc)])))

;; ---- File-extension based highlighter dispatch ------------------------------
(define (highlight-for-file path)
  (define s (if (path? path) (path->string path) (or path "")))
  (cond
    [(regexp-match? #rx"\\.rkt$" s) racket-syntax]
    [(regexp-match? #rx"\\.ya?ml$" s) yaml-syntax]
    [else #f]))

(set-highlight-for-file! highlight-for-file)
