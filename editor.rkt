#lang racket/base

(require drracket/check-syntax
         ffi/unsafe/objc
         racket/file
         "ide.rkt")

(import-class NSNumber)

(provide editor-open-file!
         editor-save-file!
         editor-text-changed!
         analyze-file
         apply-semantic-highlights!
         get-annotations
         *current-file-path*
         mouse-over-at
         jump-target-at)

;; ---- Run check-syntax on a file, returning annotation vectors ---------------
(define (analyze-file path)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "check-syntax: ~a~n" (exn-message e))
                               '())])
    (show-content path)))

;; ---- Stored state -----------------------------------------------------------
(define *current-file-path* (box #f))
(define *current-annotations* (box '()))
(define (get-annotations) (unbox *current-annotations*))

;; ---- Lookup helpers (positional queries into stored annotations) ------------

(define (mouse-over-at pos)
  (for/first ([ann (in-list (unbox *current-annotations*))]
              #:when (eq? (vector-ref ann 0) 'syncheck:add-mouse-over-status)
              #:when (<= (vector-ref ann 1) pos)
              #:when (< pos (vector-ref ann 2)))
    (vector-ref ann 3)))

(define (jump-target-at pos)
  (for/first ([ann (in-list (unbox *current-annotations*))]
              #:when (eq? (vector-ref ann 0)
                          'syncheck:add-jump-to-definition/phase-level+space)
              #:when (<= (vector-ref ann 1) pos)
              #:when (< pos (vector-ref ann 2)))
    (list (vector-ref ann 4)     ; filename
          (vector-ref ann 3))))  ; identifier

;; ---- Semantic tokens from annotations → (list color start len) -------------
;; Layered on TOP of the lexer-based highlighting in apply-highlights!.
;; Beans palette:
;;   light_blue #48c6ff — imported identifiers (use sites)
;;   yellow     #fad07a — local definition names
;;   green      #ccff00 — function.macro / builtins
;;   red_error  #902020 — unused requires

;; Islands Dark semantic colors
(define c-import    (tell (hex->NSColor "#56a8f5") retain))
(define c-local-def (tell (hex->NSColor "#c77dbb") retain))
(define c-unused    (tell (hex->NSColor "#f75464") retain))

(define (safe-range? left right)
  (and (exact-nonnegative-integer? left)
       (exact-nonnegative-integer? right)
       (> right left)))

(define (semantic-tokens annotations)
  (define tokens '())
  (for ([ann (in-list annotations)])
    (when (>= (vector-length ann) 2)
      (define tag (vector-ref ann 0))
      (case tag
        [(syncheck:add-arrow/name-dup/pxpy)
         (when (>= (vector-length ann) 12)
           (define end-left  (vector-ref ann 5))
           (define end-right (vector-ref ann 6))
           (define require?  (vector-ref ann 11))
           (when (and (equal? require? 'module-lang)
                      (safe-range? end-left end-right))
             (set! tokens (cons (list c-import end-left (- end-right end-left))
                                tokens))))]
        [(syncheck:add-definition-target/phase-level+space)
         (when (>= (vector-length ann) 3)
           (define left  (vector-ref ann 1))
           (define right (vector-ref ann 2))
           (when (safe-range? left right)
             (set! tokens (cons (list c-local-def left (- right left)) tokens))))]
        [(syncheck:add-unused-require)
         (when (>= (vector-length ann) 3)
           (define left  (vector-ref ann 1))
           (define right (vector-ref ann 2))
           (when (safe-range? left right)
             (set! tokens (cons (list c-unused left (- right left)) tokens))))]
        [else (void)])))
  tokens)

;; ---- Apply semantic colors to NSTextStorage --------------------------------
(define NS-UNDERLINE-KEY "NSUnderline")
(define NS-UNDERLINE-NSSTR (tell (NSStr NS-UNDERLINE-KEY) retain))

(define (apply-semantic-highlights! tv annotations)
  (define storage (tell tv textStorage))
  (define total (tell #:type _NSUInteger storage length))
  (define tokens (semantic-tokens annotations))
  (define um (tell tv undoManager))
  (when um (tellv um disableUndoRegistration))
  (tellv storage beginEditing)
  (for ([tok (in-list tokens)])
    (define color (car tok))
    (define loc   (cadr tok))
    (define rlen  (caddr tok))
    (when (and color
               (exact-nonnegative-integer? loc)
               (exact-nonnegative-integer? rlen)
               (> rlen 0)
               (<= (+ loc rlen) total))
      (tellv storage addAttribute: NS-FG-COLOR-NSSTR
                     value: color
                     range: #:type _NSRange (make-NSRange loc rlen))))
  (for ([ann (in-list annotations)])
    (when (and (eq? (vector-ref ann 0) 'syncheck:add-unused-require)
               (>= (vector-length ann) 3))
      (define left  (vector-ref ann 1))
      (define right (vector-ref ann 2))
      (when (safe-range? left right)
        (define rlen (- right left))
        (when (<= (+ left rlen) total)
          (tellv storage addAttribute: NS-UNDERLINE-NSSTR
                         value: (tell NSNumber numberWithInteger: #:type _NSInteger 1)
                         range: #:type _NSRange (make-NSRange left rlen))))))
  (tellv storage endEditing)
  (when um (tellv um enableUndoRegistration)))

;; ---- Public: load file into editor with full highlighting ------------------
(define (editor-open-file! path)
  (set-box! *current-file-path* path)
  (define src (file->string path))
  (set-editor-text! src)
  (define annotations (analyze-file path))
  (set-box! *current-annotations* annotations)
  (define tv (unbox *editor-view*))
  (when tv
    (apply-semantic-highlights! tv annotations)
    (tellv tv setNeedsDisplay: #:type _BOOL #t))
  (printf "check-syntax: ~a annotations for ~a~n"
          (length annotations) path)
  (flush-output))

;; ---- Debounced re-analysis on edit -----------------------------------------
;; Each edit bumps the generation. After 500ms, if the generation hasn't
;; changed, we write the buffer to a temp file and run check-syntax.
(define *edit-generation* (box 0))

(define (editor-text-changed!)
  (define gen (add1 (unbox *edit-generation*)))
  (set-box! *edit-generation* gen)
  ;; Also re-run lexer highlights immediately (fast)
  (define tv (unbox *editor-view*))
  (when tv
    (define src (nsstring->string (tell tv string)))
    (apply-highlights! tv src)
    (tellv tv setNeedsDisplay: #:type _BOOL #t))
  ;; Schedule deferred semantic re-analysis after 500ms
  (thread
   (lambda ()
     (sleep 0.5)
     (when (= (unbox *edit-generation*) gen)
       (defer!
        (lambda ()
          (when (= (unbox *edit-generation*) gen)
            (define tv (unbox *editor-view*))
            (define path (unbox *current-file-path*))
            (when (and tv path)
              (define src (nsstring->string (tell tv string)))
              ;; Write to temp file for check-syntax (needs a real file)
              (define tmp (make-temporary-file "rktlens-~a.rkt"))
              (call-with-output-file tmp #:exists 'replace
                (lambda (out) (write-string src out)))
              (define annotations
                (with-handlers ([exn:fail? (lambda (e) '())])
                  (show-content tmp)))
              (delete-file tmp)
              (when (= (unbox *edit-generation*) gen)
                (set-box! *current-annotations* annotations)
                (apply-semantic-highlights! tv annotations)
                (tellv tv setNeedsDisplay: #:type _BOOL #t))))))))))

;; Register as the text change callback
(set-on-text-change! editor-text-changed!)

(define (editor-save-file!)
  (define path (unbox *current-file-path*))
  (define tv (unbox *editor-view*))
  (when (and path tv)
    (define src (nsstring->string (tell tv string)))
    (call-with-output-file path #:exists 'replace
      (lambda (out) (write-string src out)))
    (apply-highlights! tv src)
    (define annotations (analyze-file path))
    (set-box! *current-annotations* annotations)
    (apply-semantic-highlights! tv annotations)
    (printf "saved: ~a~n" path)))
