#lang racket/base

;; cocoa-ffi.rkt — Pure Cocoa/AppKit bindings via ffi/unsafe/objc.
;; Reusable by any Racket application that needs native macOS UI.
;; No IDE-specific state lives here.

(require ffi/unsafe
         ffi/unsafe/objc)

(provide (all-defined-out))

;; ---- Frameworks -------------------------------------------------------------
(void (ffi-lib "/System/Library/Frameworks/Foundation.framework/Foundation"))
(void (ffi-lib "/System/Library/Frameworks/AppKit.framework/AppKit"))

;; ---- ctypes not provided by ffi/unsafe/objc --------------------------------
(define _NSInteger  _long)
(define _NSUInteger _ulong)
(define _CGFloat    _double)

(define-cstruct _NSPoint ([x _CGFloat] [y _CGFloat]))
(define-cstruct _NSSize  ([w _CGFloat] [h _CGFloat]))
(define-cstruct _NSRect  ([origin _NSPoint] [size _NSSize]))
(define-cstruct _NSRange ([location _NSUInteger] [length _NSUInteger]))

(define (NSMakeRect x y w h)
  (make-NSRect (make-NSPoint (exact->inexact x) (exact->inexact y))
               (make-NSSize  (exact->inexact w) (exact->inexact h))))

;; ---- Class imports ---------------------------------------------------------
(import-class NSObject NSString NSApplication NSWindow NSFont NSColor
              NSScrollView NSTableView NSTableColumn NSTextFieldCell
              NSTextView NSViewController NSSplitViewController NSSplitViewItem
              NSAutoreleasePool NSMenu NSMenuItem)

;; ---- String helpers ---------------------------------------------------------
(define (NSStr s)
  (tell NSString stringWithUTF8String: #:type _string/utf-8 s))

(define (nsstring->string ns)
  (tell #:type _string/utf-8 ns UTF8String))

;; ---- NSColor helpers -------------------------------------------------------
(define (NSColor-rgb r g b)
  (tell NSColor colorWithCalibratedRed: #:type _CGFloat (exact->inexact r)
                green: #:type _CGFloat (exact->inexact g)
                blue:  #:type _CGFloat (exact->inexact b)
                alpha: #:type _CGFloat 1.0))

(define (hex->NSColor hex)
  (define s (substring hex 1))
  (NSColor-rgb (/ (string->number (substring s 0 2) 16) 255.0)
               (/ (string->number (substring s 2 4) 16) 255.0)
               (/ (string->number (substring s 4 6) 16) 255.0)))

;; ---- Retained attribute key NSStrings ---------------------------------------
(define NS-FG-COLOR-KEY "NSColor")
(define NS-FG-COLOR-NSSTR (tell (NSStr NS-FG-COLOR-KEY) retain))

;; ---- Deferred work queue ----------------------------------------------------
;; Callbacks from Cocoa run inside sendEvent: FFI calls where Racket's thread
;; scheduler can't deschedule. Heavy work must be deferred to the main loop.
(define *deferred-queue* (box '()))

(define (defer! thunk)
  (set-box! *deferred-queue*
            (append (unbox *deferred-queue*) (list thunk))))

(define (drain-deferred!)
  (define q (unbox *deferred-queue*))
  (unless (null? q)
    (set-box! *deferred-queue* '())
    (for ([thunk (in-list q)])
      (thunk))))

;; ---- View construction helpers (generic) ------------------------------------
(define (wrap-view-in-vc view)
  (define vc (tell (tell NSViewController alloc) init))
  (tellv vc setView: view)
  vc)

(define (monospace name size)
  (define f (tell NSFont fontWithName: (NSStr name)
                         size: #:type _CGFloat (exact->inexact size)))
  (or f (tell NSFont userFixedPitchFontOfSize: #:type _CGFloat
                     (exact->inexact size))))

(define (make-window title w h vc)
  (define rect (NSMakeRect 100 100 w h))
  (define win
    (tell (tell NSWindow alloc)
          initWithContentRect: #:type _NSRect rect
          styleMask:           #:type _NSUInteger 15
          backing:             #:type _NSUInteger 2
          defer:               #:type _BOOL #f))
  (when title (tellv win setTitle: (NSStr title)))
  (tellv win setContentViewController: vc)
  (tellv win makeKeyAndOrderFront: #f)
  win)

(define (make-split-view children [vertical? #t])
  (define svc (tell (tell NSSplitViewController alloc) init))
  (tellv (tell svc splitView) setVertical: #:type _BOOL vertical?)
  (for ([c (in-list children)])
    (define item (tell NSSplitViewItem splitViewItemWithViewController: c))
    (tellv svc addSplitViewItem: item))
  svc)

;; ---- App delegate (generic: quit on last window close) ----------------------
(define-objc-class RktAppDelegate NSObject ()
  [- _BOOL (applicationShouldTerminateAfterLastWindowClosed: [_id sender])
      #t])

(define app-delegate-instance (tell (tell RktAppDelegate alloc) init))

;; ---- App lifecycle ----------------------------------------------------------
(define NSDefaultRunLoopMode (NSStr "kCFRunLoopDefaultMode"))
(define NSEventMaskAny #xFFFFFFFFFFFFFFFF)

(define (run-app build-content)
  (define pool (tell (tell NSAutoreleasePool alloc) init))
  (define app (tell NSApplication sharedApplication))
  (tellv app setActivationPolicy: #:type _NSInteger 0)
  (tellv app setDelegate: app-delegate-instance)
  (build-content)
  (tellv app activateIgnoringOtherApps: #:type _BOOL #t)
  (tellv app finishLaunching)
  (let loop ()
    (define pool (tell (tell NSAutoreleasePool alloc) init))
    (let drain ()
      (define event
        (tell app nextEventMatchingMask: #:type _NSUInteger NSEventMaskAny
                  untilDate: #f
                  inMode: NSDefaultRunLoopMode
                  dequeue: #:type _BOOL #t))
      (when event
        (tellv app sendEvent: event)
        (drain)))
    (tellv app updateWindows)
    (tellv pool drain)
    (drain-deferred!)
    (sleep 0.016)
    (loop)))
