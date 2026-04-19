#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/path
         "ide.rkt"
         "editor.rkt"
         "repl.rkt"
         "search.rkt")

(provide make-editor-with-tabs add-tab! switch-tab!)

(import-class NSView NSTextField NSFont NSColor NSButton NSImage
              NSBezierPath NSNotificationCenter)

;; ---- Tab state --------------------------------------------------------------
(define *open-tabs*   (box '()))
(define *active-tab*  (box #f))
(define *tab-bar-view* (box #f))
(define *play-button* (box #f))
(define tab-height 32.0)

(define tab-bg-active   (tell (hex->NSColor "#2b2b2b") retain))
(define tab-bg-inactive (tell (hex->NSColor "#111111") retain))
(define tab-fg-active   (tell (hex->NSColor "#e0e0e0") retain))
(define tab-fg-inactive (tell (hex->NSColor "#6d6d6d") retain))
(define tab-font (tell NSFont systemFontOfSize: #:type _CGFloat 12.0
                              weight: #:type _CGFloat 0.0))

;; ---- Tab click handler ------------------------------------------------------
(define-objc-class RktTabClickHandler NSObject ()
  [- _void (tabClicked: [_id sender])
     (define tag (tell #:type _NSInteger sender tag))
     (define tabs (unbox *open-tabs*))
     (when (and (>= tag 0) (< tag (length tabs)))
       (define path (list-ref tabs tag))
       (set-box! *active-tab* path)
       (redraw-tabs!)
       (defer! (lambda () (editor-open-file! path))))]
  [- _void (runClicked: [_id sender])
     (defer! repl-run-module!)])

(define tab-handler (tell (tell RktTabClickHandler alloc) init))

;; ---- Draw / rebuild tabs ----------------------------------------------------
(define (filename-from-path p)
  (if (path? p)
      (let-values ([(b n d?) (split-path p)]) (path->string n))
      (let-values ([(b n d?) (split-path (string->path p))]) (path->string n))))

(define (redraw-tabs!)
  (define bar (unbox *tab-bar-view*))
  (when bar
    ;; Remove old subviews
    (define subs (tell (tell bar subviews) copy))
    (define count (tell #:type _NSUInteger subs count))
    (for ([i (in-range count)])
      (tellv (tell subs objectAtIndex: #:type _NSUInteger i) removeFromSuperview))

    (define tabs (unbox *open-tabs*))
    (define active (unbox *active-tab*))
    (define x 6.0)
    (for ([path (in-list tabs)] [i (in-naturals)])
      (define name (filename-from-path path))
      (define w (+ (* (string-length name) 7.5) 24.0))
      (define active? (and active (equal? (if (path? path) (path->string path) path)
                                          (if (path? active) (path->string active) active))))
      (define btn
        (tell (tell NSButton alloc)
              initWithFrame: #:type _NSRect (NSMakeRect x 3.0 w (- tab-height 6.0))))
      (tellv btn setTitle: (NSStr name))
      (tellv btn setBordered: #:type _BOOL #f)
      (tellv btn setButtonType: #:type _NSUInteger 0)
      (tellv btn setFont: tab-font)
      (tellv btn setTag: #:type _NSInteger i)
      (tellv btn setTarget: tab-handler)
      (tellv btn setAction: #:type _SEL (selector tabClicked:))
      (if active?
          (begin
            (tellv btn setContentTintColor: tab-fg-active)
            (tellv btn setBezelStyle: #:type _NSUInteger 15)
            (tellv btn setBordered: #:type _BOOL #t))
          (begin
            (tellv btn setContentTintColor: tab-fg-inactive)))
      (tellv bar addSubview: btn)
      (set! x (+ x w 4.0)))
    ;; Re-add the play button
    (define pb (unbox *play-button*))
    (when pb (tellv bar addSubview: pb))))

;; ---- Public API -------------------------------------------------------------
(define (add-tab! path)
  (define path-str (if (path? path) (path->string path) path))
  (define tabs (unbox *open-tabs*))
  (unless (member path-str (map (lambda (p) (if (path? p) (path->string p) p)) tabs))
    (set-box! *open-tabs* (append tabs (list path-str))))
  (set-box! *active-tab* path-str)
  (redraw-tabs!))

(define (switch-tab! path)
  (add-tab! path))

(define (cycle-tab! delta)
  (define tabs (unbox *open-tabs*))
  (define active (unbox *active-tab*))
  (when (and active (> (length tabs) 1))
    (define idx (for/first ([t (in-list tabs)] [i (in-naturals)]
                            #:when (equal? t active))
                  i))
    (when idx
      (define new-idx (modulo (+ idx delta) (length tabs)))
      (define next (list-ref tabs new-idx))
      (set-box! *active-tab* next)
      (redraw-tabs!)
      (defer! (lambda () (editor-open-file! next))))))

(define (next-tab!) (cycle-tab! 1))
(define (prev-tab!) (cycle-tab! -1))

(define (close-active-tab!)
  (define active (unbox *active-tab*))
  (when active
    (define tabs (unbox *open-tabs*))
    (define idx (for/first ([t (in-list tabs)] [i (in-naturals)]
                            #:when (equal? t active))
                  i))
    (define new-tabs (filter (lambda (t) (not (equal? t active))) tabs))
    (set-box! *open-tabs* new-tabs)
    (cond
      [(null? new-tabs)
       (set-box! *active-tab* #f)
       (set-editor-text! "")
       (set-box! *current-file-path* #f)]
      [else
       (define new-idx (min (or idx 0) (sub1 (length new-tabs))))
       (define next (list-ref new-tabs new-idx))
       (set-box! *active-tab* next)
       (defer! (lambda () (editor-open-file! next)))])
    (redraw-tabs!)))

;; ---- Build editor with tab bar on top ---------------------------------------
(define (make-editor-with-tabs editor-vc)
  (define container
    (tell (tell NSView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 900 800)))
  (tellv container setAutoresizesSubviews: #:type _BOOL #t)

  ;; Tab bar at the top
  (define bar
    (tell (tell NSView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 (- 800 tab-height) 900 tab-height)))
  (tellv bar setAutoresizingMask: #:type _NSUInteger 10)
  (tellv bar setWantsLayer: #:type _BOOL #t)
  (set-box! *tab-bar-view* bar)

  ;; Editor content fills below the tab bar
  (define editor-view (tell editor-vc view))
  (tellv editor-view setFrame: #:type _NSRect
         (NSMakeRect 0 0 900 (- 800 tab-height)))
  (tellv editor-view setAutoresizingMask: #:type _NSUInteger 18)

  (tellv container addSubview: editor-view)
  (tellv container addSubview: bar)

  ;; Register tab navigation
  (set-tab-nav! next-tab! prev-tab!)
  (set-close-tab! close-active-tab!)
  ;; Register container for search bar overlay
  (set-editor-container! container)

  (wrap-view-in-vc container))
