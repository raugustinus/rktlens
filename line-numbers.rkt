#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         "ide.rkt")

(provide install-line-numbers! redraw-gutter!)

(import-class NSView NSString NSFont NSColor NSMutableDictionary
              NSScrollView NSNotificationCenter NSBezierPath)

(define gutter-width 40.0)
(define gutter-bg    (tell (hex->NSColor "#1A1B1D") retain))
(define gutter-fg    (tell (hex->NSColor "#535353") retain))
(define gutter-line  (tell (hex->NSColor "#2b2b2b") retain))

;; ---- Line number gutter view -----------------------------------------------
;; Sits as a subview of the scroll view's content view, positioned at
;; the left edge. The text view gets a left textContainerInset to make room.

(define *gutter-text-view* (box #f))

(define-objc-class RktGutterView NSView ()
  [- _BOOL (isFlipped) #t]

  [- _void (drawRect: [_NSRect dirty])
     (define tv (unbox *gutter-text-view*))
     (when tv
       ;; Fill background with rounded bottom-left corner
       (define bounds (tell #:type _NSRect self bounds))
       (define bw (NSSize-w (NSRect-size bounds)))
       (define bh (NSSize-h (NSRect-size bounds)))
       (define bx (NSPoint-x (NSRect-origin bounds)))
       (define by (NSPoint-y (NSRect-origin bounds)))
       (define radius 10.0)

       (define path (tell NSBezierPath bezierPath))
       ;; Start top-left (square corner)
       (tellv path moveToPoint: #:type _NSPoint (make-NSPoint bx by))
       ;; Top edge to top-right (square corner)
       (tellv path lineToPoint: #:type _NSPoint (make-NSPoint (+ bx bw) by))
       ;; Right edge down to bottom-right (square corner)
       (tellv path lineToPoint: #:type _NSPoint (make-NSPoint (+ bx bw) (+ by bh)))
       ;; Bottom edge to bottom-left corner (rounded)
       (tellv path lineToPoint: #:type _NSPoint (make-NSPoint (+ bx radius) (+ by bh)))
       (tellv path appendBezierPathWithArcFromPoint: #:type _NSPoint (make-NSPoint bx (+ by bh))
                   toPoint: #:type _NSPoint (make-NSPoint bx (+ by bh (- radius)))
                   radius: #:type _CGFloat radius)
       ;; Left edge back to top
       (tellv path lineToPoint: #:type _NSPoint (make-NSPoint bx by))
       (tellv path closePath)

       (tellv gutter-bg setFill)
       (tellv path fill)

       ;; 1px separator line at right edge
       (tellv gutter-line setFill)
       (define fill-fn
         (get-ffi-obj "NSRectFill"
                      (ffi-lib "/System/Library/Frameworks/AppKit.framework/AppKit")
                      (_fun _NSRect -> _void)))
       (fill-fn (NSMakeRect (- bw 1.0) by 1.0 bh))

       (define storage (tell tv textStorage))
       (define text-len (tell #:type _NSUInteger storage length))
       (when (> text-len 0)
         (define lm (tell tv layoutManager))
         (define tc (tell tv textContainer))
         (define visible-rect (tell #:type _NSRect tv visibleRect))
         (define inset-y (NSSize-h (tell #:type _NSSize tv textContainerInset)))

         ;; Match editor font size (slightly smaller for gutter)
         (define editor-font (tell tv font))
         (define editor-size (if editor-font (tell #:type _CGFloat editor-font pointSize) 13.0))
         (define font (tell NSFont monospacedSystemFontOfSize: #:type _CGFloat (- editor-size 1.0)
                                   weight: #:type _CGFloat 0.0))
         (define attrs (tell NSMutableDictionary dictionary))
         (tellv attrs setObject: font forKey: (NSStr "NSFont"))
         (tellv attrs setObject: gutter-fg forKey: (NSStr "NSColor"))

         ;; Visible glyph range — extend rect to ensure full coverage
         (define vis-origin (NSRect-origin visible-rect))
         (define vis-size (NSRect-size visible-rect))
         (define padded-rect
           (NSMakeRect (NSPoint-x vis-origin)
                       (NSPoint-y vis-origin)
                       (NSSize-w vis-size)
                       (+ (NSSize-h vis-size) 100.0)))
         (define glyph-range
           (tell #:type _NSRange lm
                 glyphRangeForBoundingRect: #:type _NSRect padded-rect
                 inTextContainer: tc))
         (define char-range
           (tell #:type _NSRange lm
                 characterRangeForGlyphRange: #:type _NSRange glyph-range
                 actualGlyphRange: #:type _pointer #f))

         ;; Count line number at start of visible range
         (define src (nsstring->string (tell storage string)))
         (define start-pos (NSRange-location char-range))
         (define line-num
           (let loop ([i 0] [n 1])
             (cond
               [(>= i start-pos) n]
               [(>= i (string-length src)) n]
               [(char=? (string-ref src i) #\newline) (loop (add1 i) (add1 n))]
               [else (loop (add1 i) n)])))

         ;; Draw each visible line
         (define end-pos (min (+ start-pos (NSRange-length char-range)) text-len))
         (let draw-loop ([pos start-pos] [ln line-num])
           (when (< pos end-pos)
             (define line-range
               (tell #:type _NSRange (tell storage string)
                     lineRangeForRange: #:type _NSRange (make-NSRange pos 0)))
             (define glyph-idx
               (tell #:type _NSUInteger lm
                     glyphIndexForCharacterAtIndex: #:type _NSUInteger pos))
             (define line-rect
               (tell #:type _NSRect lm
                     lineFragmentRectForGlyphAtIndex: #:type _NSUInteger glyph-idx
                     effectiveRange: #:type _pointer #f))
             (define y (+ (NSPoint-y (NSRect-origin line-rect)) inset-y))
             (define num-str (NSStr (number->string ln)))
             (define str-size (tell #:type _NSSize num-str sizeWithAttributes: attrs))
             (define x (- gutter-width (NSSize-w str-size) 10.0))
             (tellv num-str drawAtPoint: #:type _NSPoint (make-NSPoint x y)
                            withAttributes: attrs)
             (define next-pos (+ (NSRange-location line-range)
                                 (NSRange-length line-range)))
             (when (> next-pos pos)
               (draw-loop next-pos (add1 ln)))))))])

;; ---- Notification handler for scroll/edit -----------------------------------
(define-objc-class RktGutterUpdater NSObject ()
  [- _void (

textChanged: [_id notification])
     (define gutter (unbox *gutter-view-ref*))
     (when gutter (tellv gutter setNeedsDisplay: #:type _BOOL #t))]
  [- _void (boundsChanged: [_id notification])
     (define gutter (unbox *gutter-view-ref*))
     (when gutter (tellv gutter setNeedsDisplay: #:type _BOOL #t))])

(define *gutter-view-ref* (box #f))
(define gutter-updater (tell (tell RktGutterUpdater alloc) init))

;; ---- Install ----------------------------------------------------------------
(define (install-line-numbers! scroll-view text-view)
  (set-box! *gutter-text-view* text-view)

  ;; Add left inset to text container so text starts after gutter
  (tellv text-view setTextContainerInset:
         #:type _NSSize (make-NSSize gutter-width 0.0))

  ;; Create gutter view as subview of the scroll view's content view
  (define content-view (tell scroll-view contentView))
  (define bounds (tell #:type _NSRect content-view bounds))
  (define gutter
    (tell (tell RktGutterView alloc)
          initWithFrame: #:type _NSRect
          (NSMakeRect 0 0 gutter-width (NSSize-h (NSRect-size bounds)))))
  (tellv gutter setAutoresizingMask: #:type _NSUInteger 16)
  ;; Round bottom-left corner of gutter
  (tellv gutter setWantsLayer: #:type _BOOL #t)
  (define gutter-layer (tell gutter layer))
  (tellv gutter-layer setCornerRadius: #:type _CGFloat 10.0)
  (tellv gutter-layer setMaskedCorners: #:type _NSUInteger 4)
  (tellv gutter-layer setMasksToBounds: #:type _BOOL #t)
  ;; Round bottom-right corner of scroll view
  (tellv scroll-view setWantsLayer: #:type _BOOL #t)
  (define scroll-layer (tell scroll-view layer))
  (tellv scroll-layer setCornerRadius: #:type _CGFloat 10.0)
  (tellv scroll-layer setMaskedCorners: #:type _NSUInteger 8)
  (tellv scroll-layer setMasksToBounds: #:type _BOOL #t)
  (tellv content-view addSubview: gutter)
  (set-box! *gutter-view-ref* gutter)

  ;; Observe text changes and scroll to redraw
  (define nc (tell NSNotificationCenter defaultCenter))
  (tellv nc addObserver: gutter-updater
            selector: #:type _SEL (selector textChanged:)
            name: (NSStr "NSTextDidChangeNotification")
            object: text-view)
  (tellv nc addObserver: gutter-updater
            selector: #:type _SEL (selector boundsChanged:)
            name: (NSStr "NSViewBoundsDidChangeNotification")
            object: content-view)
  (tellv content-view setPostsBoundsChangedNotifications: #:type _BOOL #t))

(define (redraw-gutter!)
  (define gutter (unbox *gutter-view-ref*))
  (when gutter (tellv gutter setNeedsDisplay: #:type _BOOL #t)))

(set-line-number-installer! install-line-numbers!)
