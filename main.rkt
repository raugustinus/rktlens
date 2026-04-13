#lang racket

(require ffi/unsafe
         ffi/unsafe/objc
         "dsl.rkt"
         "ide.rkt"
         "editor.rkt"
         "editor-view.rkt"
         "line-numbers.rkt"
         "repl.rkt"
         "tab-bar.rkt"
         "status-bar.rkt"
         "settings.rkt"
         racket/runtime-path)

(import-class NSTitlebarAccessoryViewController NSImage NSButton NSData)

(define-objc-class RktPlayHandler NSObject ()
  [- _void (runClicked: [_id sender])
     (defer! repl-run-module!)])

(define play-handler (tell (tell RktPlayHandler alloc) init))

(define-runtime-path app-dir ".")

(define here
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (let ([p (string->path (vector-ref args 0))])
          (if (absolute-path? p) p (build-path (current-directory) p)))
        app-dir)))

(define initial-path (build-path here "main.rkt"))
(define initial-src  (file->string initial-path))
(define initial-anns (analyze-file initial-path))
(set-box! *current-file-path* initial-path)

(define (open-file name)
  (define p (build-path here name))
  (if (file-exists? p)
      (begin
        (add-tab! p)
        (defer! (lambda () (editor-open-file! p))))
      (set-editor-text! (format ";; missing: ~a\n" name))))

(define-application rktlens-ide
  (let ()
    (install-full-menu!)
    ;; App icon (Lucide aperture)
    (define icon-svg
      (string-append
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"128\" height=\"128\" viewBox=\"0 0 24 24\" "
       "fill=\"none\" stroke=\"#8197bf\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
       "<circle cx=\"12\" cy=\"12\" r=\"10\"/>"
       "<path d=\"m14.31 8 5.74 9.94\"/>"
       "<path d=\"M9.69 8h11.48\"/>"
       "<path d=\"m7.38 12 5.74-9.94\"/>"
       "<path d=\"M9.69 16 3.95 6.06\"/>"
       "<path d=\"M14.31 16H2.83\"/>"
       "<path d=\"m16.62 12-5.74 9.94\"/>"
       "</svg>"))
    (define icon-data
      (tell NSData dataWithBytes: #:type _bytes (string->bytes/utf-8 icon-svg)
                   length: #:type _NSUInteger (bytes-length (string->bytes/utf-8 icon-svg))))
    (define icon-img (tell (tell NSImage alloc) initWithData: icon-data))
    (when icon-img
      (tellv (tell NSApplication sharedApplication) setApplicationIconImage: icon-img))
    (define font (monospace "JetBrains Mono" 14))
    (define editor-vc
      (text-view #:storage initial-src
                 #:font font
                 #:highlights racket-syntax))
    (define editor-with-tabs
      (make-editor-with-tabs editor-vc))
    (add-tab! initial-path)
    (define editor-pane
      (wrap-in-panel editor-with-tabs 4.0 10.0 "#1A1B1D"))
    (define repl-pane
      (wrap-in-panel
       (repl-view #:font font)
       4.0 10.0 "#1A1B1D"))
    (define main-content
      (split-view #:orientation 'vertical
        (split-view #:divider-style 'thin
          (project-view #:root here
                        #:on-select open-file
                        #:width 250)
          editor-pane)
        repl-pane))
    ;; Set REPL to ~20% height (deferred so layout has happened)
    (defer!
     (lambda ()
       (define sv (tell main-content splitView))
       (define h (NSSize-h (NSRect-size (tell #:type _NSRect sv frame))))
       (tellv sv setPosition: #:type _CGFloat (* h 0.8)
                  ofDividerAtIndex: #:type _NSInteger 0)))
    (define win
      (window #:title "rktlens"
              #:size (1400 900)
        (wrap-with-status-bar main-content)))

    ;; Play button in titlebar (right side, inline)
    (define tb-vc (tell (tell NSTitlebarAccessoryViewController alloc) init))
    (define play-btn
      (tell (tell NSButton alloc)
            initWithFrame: #:type _NSRect (NSMakeRect 4 2 24 24)))
    (tellv play-btn setBordered: #:type _BOOL #f)
    (tellv play-btn setButtonType: #:type _NSUInteger 0)
    (define play-img
      (tell NSImage imageWithSystemSymbolName: (NSStr "play.fill")
                    accessibilityDescription: (NSStr "Run")))
    (when play-img (tellv play-btn setImage: play-img))
    (tellv play-btn setContentTintColor: (hex->NSColor "#4ec94e"))
    (tellv play-btn setTarget: play-handler)
    (tellv play-btn setAction: #:type _SEL (selector runClicked:))
    (define tb-view
      (tell (tell NSView alloc)
            initWithFrame: #:type _NSRect (NSMakeRect 0 0 32 28)))
    (tellv tb-view addSubview: play-btn)
    (tellv tb-vc setView: tb-view)
    ;; layoutAttribute: 2 = right side of titlebar (inline, no extra strip)
    (tellv tb-vc setLayoutAttribute: #:type _NSInteger 2)
    (tellv win addTitlebarAccessoryViewController: tb-vc)

    win))
