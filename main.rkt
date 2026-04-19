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

(import-class NSImage)

(define-runtime-path app-dir ".")

(define here
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (let ([p (string->path (vector-ref args 0))])
          (if (absolute-path? p) p (build-path (current-directory) p)))
        app-dir)))

(define (find-initial-file dir)
  (define candidates '("main.rkt" "info.rkt"))
  (or (for/first ([c (in-list candidates)]
                  #:when (file-exists? (build-path dir c)))
        (build-path dir c))
      (for/first ([f (in-list (directory-list dir))]
                  #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
        (build-path dir f))
      #f))

(define initial-path (find-initial-file here))
(define initial-src  (and initial-path (file->string initial-path)))
(define initial-anns (if initial-path (analyze-file initial-path) '()))
(when initial-path (set-box! *current-file-path* initial-path))

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
    ;; App icon from .icns file
    (define icon-path (build-path app-dir "icon.icns"))
    (when (file-exists? icon-path)
      (define icon-img (tell (tell NSImage alloc)
                             initWithContentsOfFile: (NSStr (path->string icon-path))))
      (when icon-img
        (tellv (tell NSApplication sharedApplication) setApplicationIconImage: icon-img)))
    (define font (monospace "JetBrains Mono" 14))
    (define editor-vc
      (text-view #:storage (or initial-src "")
                 #:font font
                 #:highlights racket-syntax))
    (define editor-with-tabs
      (make-editor-with-tabs editor-vc))
    (when initial-path (add-tab! initial-path))
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
    (window #:title "rktlens"
            #:size (1400 900)
      (wrap-with-status-bar main-content))))
