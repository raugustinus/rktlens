#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/string
         "ide.rkt")

(provide show-search-bar! hide-search-bar! search-next! search-prev!
         search-visible? set-editor-container! install-search-observer!
         *search-field*)

(import-class NSView NSTextField NSButton NSFont NSColor NSNumber)

;; ---- Search state -----------------------------------------------------------
(define *search-bar*     (box #f))
(define *search-field*   (box #f))
(define *match-ranges*   (box '()))
(define *match-index*    (box 0))
(define *search-visible* (box #f))

(define (search-visible?) (unbox *search-visible*))

(define match-bg (tell (hex->NSColor "#fad07a") retain))
(define match-fg (tell (hex->NSColor "#111111") retain))
(define current-match-bg (tell (hex->NSColor "#ff005b") retain))

(define NS-BG-COLOR-NSSTR (tell (NSStr "NSBackgroundColor") retain))

;; ---- Find all matches -------------------------------------------------------
(define (find-matches src query)
  (if (or (string=? query "") (string=? src ""))
      '()
      (let ([qlen (string-length query)]
            [slen (string-length src)]
            [q-lower (string-downcase query)]
            [s-lower (string-downcase src)])
        (let loop ([i 0] [acc '()])
          (cond
            [(> (+ i qlen) slen) (reverse acc)]
            [(string=? (substring s-lower i (+ i qlen)) q-lower)
             (loop (+ i 1) (cons (list i qlen) acc))]
            [else (loop (+ i 1) acc)])))))

;; ---- Highlight matches in the text storage ----------------------------------
(define (highlight-matches! tv matches current-idx)
  (define storage (tell tv textStorage))
  (define total (tell #:type _NSUInteger storage length))
  (define um (tell tv undoManager))
  (when um (tellv um disableUndoRegistration))
  (tellv storage beginEditing)
  ;; Clear old highlights
  (when (> total 0)
    (tellv storage removeAttribute: NS-BG-COLOR-NSSTR
                   range: #:type _NSRange (make-NSRange 0 total)))
  ;; Apply match highlights
  (for ([m (in-list matches)] [i (in-naturals)])
    (define loc (car m))
    (define len (cadr m))
    (when (<= (+ loc len) total)
      (define bg (if (= i current-idx) current-match-bg match-bg))
      (define fg (if (= i current-idx) (hex->NSColor "#ffffff") match-fg))
      (tellv storage addAttribute: NS-BG-COLOR-NSSTR
                     value: bg
                     range: #:type _NSRange (make-NSRange loc len))
      (tellv storage addAttribute: NS-FG-COLOR-NSSTR
                     value: fg
                     range: #:type _NSRange (make-NSRange loc len))))
  (tellv storage endEditing)
  (when um (tellv um enableUndoRegistration)))

(define (clear-highlights! tv)
  (define storage (tell tv textStorage))
  (define total (tell #:type _NSUInteger storage length))
  (when (> total 0)
    (define um (tell tv undoManager))
    (when um (tellv um disableUndoRegistration))
    (tellv storage beginEditing)
    (tellv storage removeAttribute: NS-BG-COLOR-NSSTR
                   range: #:type _NSRange (make-NSRange 0 total))
    (tellv storage endEditing)
    (when um (tellv um enableUndoRegistration))
    ;; Re-apply syntax highlighting
    (define src (nsstring->string (tell storage string)))
    (apply-highlights! tv src)))

;; ---- Navigate matches -------------------------------------------------------
(define (goto-match! tv idx)
  (define matches (unbox *match-ranges*))
  (when (and (>= idx 0) (< idx (length matches)))
    (set-box! *match-index* idx)
    (define m (list-ref matches idx))
    (define loc (car m))
    (define len (cadr m))
    (tellv tv setSelectedRange: #:type _NSRange (make-NSRange loc len))
    (tellv tv scrollRangeToVisible: #:type _NSRange (make-NSRange loc len))
    (highlight-matches! tv matches idx)
    (update-match-count!)))

(define (do-search!)
  (define tv (unbox *editor-view*))
  (define field (unbox *search-field*))
  (when (and tv field)
    (define query (nsstring->string (tell field stringValue)))
    (define src (nsstring->string (tell (tell tv textStorage) string)))
    (define matches (find-matches src query))
    (set-box! *match-ranges* matches)
    (set-box! *match-index* 0)
    (if (null? matches)
        (clear-highlights! tv)
        (goto-match! tv 0))))

(define (search-next!)
  (define tv (unbox *editor-view*))
  (define matches (unbox *match-ranges*))
  (when (and tv (not (null? matches)))
    (define idx (modulo (add1 (unbox *match-index*)) (length matches)))
    (goto-match! tv idx)))

(define (search-prev!)
  (define tv (unbox *editor-view*))
  (define matches (unbox *match-ranges*))
  (when (and tv (not (null? matches)))
    (define idx (modulo (sub1 (unbox *match-index*)) (length matches)))
    (goto-match! tv idx)))

;; ---- Match count label ------------------------------------------------------
(define *count-label* (box #f))

(define (update-match-count!)
  (define label (unbox *count-label*))
  (define matches (unbox *match-ranges*))
  (define idx (unbox *match-index*))
  (when label
    (if (null? matches)
        (tellv label setStringValue: (NSStr "No results"))
        (tellv label setStringValue:
               (NSStr (format "~a of ~a" (add1 idx) (length matches)))))))

;; ---- Search field delegate --------------------------------------------------
(define-objc-class RktSearchHandler NSObject ()
  [- _void (searchChanged: [_id sender])
     (do-search!)]
  [- _void (searchAction: [_id sender])
     (search-next!)]
  [- _void (closeSearch: [_id sender])
     (hide-search-bar!)])

(define search-handler (tell (tell RktSearchHandler alloc) init))

;; ---- Build the search bar ---------------------------------------------------
(define (make-search-bar width)
  (define bar-h 32.0)
  (define bar
    (tell (tell NSView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 width bar-h)))
  (tellv bar setWantsLayer: #:type _BOOL #t)

  ;; Search field
  (define field
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 8 4 (- width 130) 24)))
  (tellv field setPlaceholderString: (NSStr "Find..."))
  (tellv field setFont: (tell NSFont systemFontOfSize: #:type _CGFloat 13.0))
  (tellv field setTarget: search-handler)
  (tellv field setAction: #:type _SEL (selector searchAction:))
  (tellv field setAutoresizingMask: #:type _NSUInteger 2)
  (set-box! *search-field* field)
  (tellv bar addSubview: field)

  ;; Match count
  (define count-lbl
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect (- width 120) 6 70 20)))
  (tellv count-lbl setEditable: #:type _BOOL #f)
  (tellv count-lbl setBordered: #:type _BOOL #f)
  (tellv count-lbl setDrawsBackground: #:type _BOOL #f)
  (tellv count-lbl setTextColor: (hex->NSColor "#6d6d6d"))
  (tellv count-lbl setFont: (tell NSFont systemFontOfSize: #:type _CGFloat 11.0))
  (tellv count-lbl setStringValue: (NSStr ""))
  (tellv count-lbl setAutoresizingMask: #:type _NSUInteger 1)
  (set-box! *count-label* count-lbl)
  (tellv bar addSubview: count-lbl)

  ;; Close button
  (define close-btn
    (tell (tell NSButton alloc)
          initWithFrame: #:type _NSRect (NSMakeRect (- width 44) 4 36 24)))
  (tellv close-btn setTitle: (NSStr "✕"))
  (tellv close-btn setBordered: #:type _BOOL #f)
  (tellv close-btn setTarget: search-handler)
  (tellv close-btn setAction: #:type _SEL (selector closeSearch:))
  (tellv close-btn setAutoresizingMask: #:type _NSUInteger 1)
  (tellv bar addSubview: close-btn)

  bar)

;; ---- Show / Hide ------------------------------------------------------------
(define *editor-container* (box #f))
(define (set-editor-container! c) (set-box! *editor-container* c))

(define (show-search-bar!)
  (when (not (unbox *search-visible*))
    (define container (unbox *editor-container*))
    (when container
      (define bar (or (unbox *search-bar*)
                      (let ([b (make-search-bar 900)])
                        (set-box! *search-bar* b)
                        b)))
      (define bounds (tell #:type _NSRect container bounds))
      (define bw (NSSize-w (NSRect-size bounds)))
      (define bh (NSSize-h (NSRect-size bounds)))
      (define bar-h 32.0)
      (tellv bar setFrame: #:type _NSRect (NSMakeRect 0 (- bh bar-h) bw bar-h))
      (tellv bar setAutoresizingMask: #:type _NSUInteger 10)
      (tellv container addSubview: bar)
      (set-box! *search-visible* #t)
      ;; Install live search observer + focus
      (define field (unbox *search-field*))
      (when field
        (install-search-observer! field)
        (tellv (tell container window) makeFirstResponder: field)))))

(define (hide-search-bar!)
  (when (unbox *search-visible*)
    (define bar (unbox *search-bar*))
    (when bar (tellv bar removeFromSuperview))
    (set-box! *search-visible* #f)
    ;; Clear highlights
    (define tv (unbox *editor-view*))
    (when tv
      (clear-highlights! tv)
      (tellv tv setNeedsDisplay: #:type _BOOL #t)
      (tellv (tell (tell tv superview) window) makeFirstResponder: tv))))

;; ---- Live search on text change ---------------------------------------------
(import-class NSNotificationCenter)

(define-objc-class RktSearchFieldObserver NSObject ()
  [- _void (textChanged: [_id notification])
     (when (search-visible?) (do-search!))])

(define search-field-observer (tell (tell RktSearchFieldObserver alloc) init))

(define (install-search-observer! field)
  (define nc (tell NSNotificationCenter defaultCenter))
  (tellv nc addObserver: search-field-observer
            selector: #:type _SEL (selector textChanged:)
            name: (NSStr "NSControlTextDidChangeNotification")
            object: field))
