#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/pretty
         "ide.rkt"
         "editor.rkt")

(provide step-macros!)

(import-class NSWindow NSScrollView NSTextView NSFont NSView NSBox)

;; ---- Macro stepping via expand-once -----------------------------------------
(define (collect-steps src-text)
  (with-handlers ([exn:fail? (lambda (e) (list (format "error: ~a" (exn-message e))))])
    (define ns (make-base-namespace))
    ;; Try to load the current file's module for context
    (define path (unbox *current-file-path*))
    (when (and path (rkt-file? path))
      (with-handlers ([exn:fail? void])
        (parameterize ([current-namespace ns])
          (dynamic-require path (void)))))
    (parameterize ([current-namespace ns])
      (define stx (datum->syntax #f (read (open-input-string src-text))))
      (define steps
        (let loop ([s stx] [acc (list (format "~a" (syntax->datum stx)))] [i 0])
          (if (>= i 20)
              (reverse (cons "... (max steps reached)" acc))
              (let ([next (with-handlers ([exn:fail? (lambda _ #f)])
                            (expand-once s))])
                (if (or (not next) (equal? (syntax->datum next) (syntax->datum s)))
                    (reverse acc)
                    (loop next
                          (cons (format "~a" (syntax->datum next)) acc)
                          (add1 i)))))))
      steps)))

(define (rkt-file? path)
  (regexp-match? #rx"\\.rkt$" (if (string? path) path (path->string path))))

;; ---- Stepper panel ----------------------------------------------------------
(define *stepper-window* (box #f))

(define (show-stepper-window! steps)
  (define existing (unbox *stepper-window*))
  (when existing (tellv existing close))

  (define win
    (tell (tell NSWindow alloc)
          initWithContentRect: #:type _NSRect (NSMakeRect 200 200 600 500)
          styleMask: #:type _NSUInteger 15
          backing: #:type _NSUInteger 2
          defer: #:type _BOOL #f))
  (tellv win setTitle: (NSStr "Macro Stepper"))
  (tellv win setBackgroundColor: (hex->NSColor "#1A1B1D"))

  (define scroll
    (tell (tell NSScrollView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 600 500)))
  (tellv scroll setHasVerticalScroller: #:type _BOOL #t)

  (define tv
    (tell (tell NSTextView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 600 500)))
  (tellv tv setEditable: #:type _BOOL #f)
  (tellv tv setRichText: #:type _BOOL #f)
  (tellv tv setBackgroundColor: (hex->NSColor "#1A1B1D"))
  (tellv tv setTextColor: (hex->NSColor "#bcbec4"))
  (tellv tv setFont: (monospace "JetBrains Mono" 13))

  ;; Build the step display
  (define text
    (apply string-append
           (for/list ([step (in-list steps)] [i (in-naturals)])
             (if (zero? i)
                 (format "  Original:\n    ~a\n\n" step)
                 (format "  Step ~a:\n    ~a\n\n" i step)))))
  (tellv tv setString: (NSStr text))

  ;; Color the step headers
  (define storage (tell tv textStorage))
  (define header-color (tell (hex->NSColor "#cf8e6d") retain))
  (define arrow-color (tell (hex->NSColor "#7a7e85") retain))
  (define um (tell tv undoManager))
  (when um (tellv um disableUndoRegistration))
  (tellv storage beginEditing)
  (define full-text text)
  (let loop ([pos 0])
    (define idx (regexp-match-positions #rx"(Original:|Step [0-9]+:)" full-text pos))
    (when idx
      (define start (caar idx))
      (define end (cdar idx))
      (tellv storage addAttribute: NS-FG-COLOR-NSSTR
                     value: header-color
                     range: #:type _NSRange (make-NSRange start (- end start)))
      (loop end)))
  (tellv storage endEditing)
  (when um (tellv um enableUndoRegistration))

  (tellv scroll setDocumentView: tv)
  (tellv win setContentView: scroll)
  (tellv win makeKeyAndOrderFront: #f)
  (set-box! *stepper-window* win))

;; ---- Public: step the expression at cursor ----------------------------------
(define (step-macros!)
  (define tv (unbox *editor-view*))
  (when tv
    (define sel (tell #:type _NSRange tv selectedRange))
    (define storage (tell tv textStorage))
    (define str (tell storage string))
    (define text
      (if (> (NSRange-length sel) 0)
          (nsstring->string
           (tell str substringWithRange: #:type _NSRange sel))
          ;; No selection: try to read the s-expression at cursor
          (let ()
            (define src (nsstring->string str))
            (define pos (NSRange-location sel))
            ;; Find the enclosing paren
            (define start
              (let loop ([i pos])
                (cond
                  [(< i 0) 0]
                  [(char=? (string-ref src i) #\() i]
                  [else (loop (sub1 i))])))
            (with-handlers ([exn:fail? (lambda _ "")])
              (define in (open-input-string (substring src start)))
              (define datum (read in))
              (define end (+ start (file-position in)))
              (substring src start end)))))
    (when (> (string-length text) 0)
      (defer!
       (lambda ()
         (define steps (collect-steps text))
         (show-stepper-window! steps))))))
