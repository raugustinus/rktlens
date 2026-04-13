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

(import-class NSTitlebarAccessoryViewController NSImage NSButton)

(define-objc-class RktPlayHandler NSObject ()
  [- _void (runClicked: [_id sender])
     (defer! repl-run-module!)])

(define play-handler (tell (tell RktPlayHandler alloc) init))

(define-runtime-path here ".")

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
    (define font (monospace "Berkeley Mono" 13))
    (define editor-vc
      (text-view #:storage initial-src
                 #:font font
                 #:highlights racket-syntax))
    (define editor-with-tabs
      (make-editor-with-tabs editor-vc))
    (add-tab! initial-path)
    (define editor-pane
      (wrap-in-panel editor-with-tabs 4.0 10.0 "#111111"))
    (define repl-pane
      (wrap-in-panel
       (repl-view #:font font)
       4.0 10.0 "#111111"))
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
