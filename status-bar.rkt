#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         "ide.rkt"
         "editor.rkt")

(provide make-status-bar update-status-bar! wrap-with-status-bar)

(import-class NSTextField NSFont NSNotificationCenter)

(define bar-bg   (tell (hex->NSColor "#1c1c1c") retain))
(define bar-fg   (tell (hex->NSColor "#6d6d6d") retain))
(define bar-font (tell NSFont monospacedSystemFontOfSize: #:type _CGFloat 12.0
                              weight: #:type _CGFloat 0.0))

(define *status-label* (box #f))

(define (update-status-bar!)
  (define label (unbox *status-label*))
  (define tv (unbox *editor-view*))
  (when (and label tv)
    (define path (unbox *current-file-path*))
    (define sel (tell #:type _NSRange tv selectedRange))
    (define cursor (NSRange-location sel))
    (define storage (tell tv textStorage))
    (define total (tell #:type _NSUInteger storage length))
    (define src (if (> total 0) (nsstring->string (tell storage string)) ""))
    (define-values (line col)
      (if (> (string-length src) 0)
          (let loop ([i 0] [ln 1] [c 0])
            (cond
              [(>= i (min cursor (string-length src))) (values ln c)]
              [(char=? (string-ref src i) #\newline) (loop (add1 i) (add1 ln) 0)]
              [else (loop (add1 i) ln (add1 c))]))
          (values 1 0)))
    (define fname (if path
                      (let-values ([(b n d?) (split-path path)])
                        (path->string n))
                      "untitled"))
    (define text (format "  ~a  |  Ln ~a, Col ~a  |  UTF-8  |  Racket" fname line col))
    (tellv label setStringValue: (NSStr text))))

(define-objc-class RktStatusBarUpdater NSObject ()
  [- _void (cursorMoved: [_id notification])
     (update-status-bar!)])

(define status-updater (tell (tell RktStatusBarUpdater alloc) init))

(define (make-status-bar width)
  (define label
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 width 28)))
  (tellv label setEditable: #:type _BOOL #f)
  (tellv label setBordered: #:type _BOOL #f)
  (tellv label setDrawsBackground: #:type _BOOL #t)
  (tellv label setBackgroundColor: bar-bg)
  (tellv label setTextColor: bar-fg)
  (tellv label setFont: bar-font)
  (tellv label setStringValue: (NSStr "  rktlens"))
  (tellv label setAutoresizingMask: #:type _NSUInteger 2)
  (set-box! *status-label* label)

  (define nc (tell NSNotificationCenter defaultCenter))
  (tellv nc addObserver: status-updater
            selector: #:type _SEL (selector cursorMoved:)
            name: (NSStr "NSTextViewDidChangeSelectionNotification")
            object: #f)

  label)

(import-class NSView)

(define (wrap-with-status-bar content-vc)
  (define bar (make-status-bar 1400))
  (define bar-h 28.0)
  (define container
    (tell (tell NSView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 1400 900)))
  (tellv container setAutoresizesSubviews: #:type _BOOL #t)

  (define content-view (tell content-vc view))
  (tellv content-view setFrame: #:type _NSRect (NSMakeRect 0 bar-h 1400 (- 900 bar-h)))
  (tellv content-view setAutoresizingMask: #:type _NSUInteger 18)

  (tellv bar setFrame: #:type _NSRect (NSMakeRect 0 0 1400 bar-h))
  (tellv bar setAutoresizingMask: #:type _NSUInteger 2)

  (tellv container addSubview: content-view)
  (tellv container addSubview: bar)
  (wrap-view-in-vc container))
