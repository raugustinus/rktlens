#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         "ide.rkt"
         "editor.rkt"
         "repl.rkt"
         "tab-bar.rkt"
         "line-numbers.rkt")

(provide install-full-menu! install-toolbar! show-preferences!)

(import-class NSPanel NSTextField NSButton NSOpenPanel NSSavePanel NSURL
              NSToolbar NSToolbarItem NSImage NSToolbarItemGroup)

;; Dynamic selector registration (for building menus from strings)
(define sel-reg
  (get-ffi-obj "sel_registerName" (ffi-lib "libobjc")
               (_fun _string/utf-8 -> _SEL)))

;; ---- Preferences state -----------------------------------------------------
(define *prefs-panel* (box #f))

(define (current-font-name)
  (define tv (unbox *editor-view*))
  (and tv
       (let ([f (tell tv font)])
         (and f (nsstring->string (tell f fontName))))))

(define (current-font-size)
  (define tv (unbox *editor-view*))
  (and tv
       (let ([f (tell tv font)])
         (and f (tell #:type _CGFloat f pointSize)))))

;; ---- Prefs controller: Apply + Close ----------------------------------------
(define *font-name-field* (box #f))
(define *font-size-field* (box #f))

(define-objc-class RktPrefsController NSObject ()
  [- _void (applyPrefs: [_id sender])
     (define name-field (unbox *font-name-field*))
     (define size-field (unbox *font-size-field*))
     (when (and name-field size-field)
       (define name (nsstring->string (tell name-field stringValue)))
       (define size (tell #:type _CGFloat size-field floatValue))
       (define font (monospace name (max 8.0 size)))
       (define tv (unbox *editor-view*))
       (when (and tv font)
         (tellv tv setFont: font)
         (define src (nsstring->string (tell tv string)))
         (apply-highlights! tv src)
         (redraw-gutter!)))]
  [- _void (closePrefs: [_id sender])
     (define panel (unbox *prefs-panel*))
     (when panel (tellv panel close))])

(define prefs-controller (tell (tell RktPrefsController alloc) init))

;; ---- Build the preferences panel -------------------------------------------
(define (make-label text x y w h)
  (define lbl
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect x y w h)))
  (tellv lbl setStringValue: (NSStr text))
  (tellv lbl setEditable: #:type _BOOL #f)
  (tellv lbl setBordered: #:type _BOOL #f)
  (tellv lbl setDrawsBackground: #:type _BOOL #f)
  lbl)

(define (make-field text x y w h)
  (define fld
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect x y w h)))
  (tellv fld setStringValue: (NSStr text))
  fld)

(define (make-button title x y w h target action-sel)
  (define btn
    (tell (tell NSButton alloc)
          initWithFrame: #:type _NSRect (NSMakeRect x y w h)))
  (tellv btn setTitle: (NSStr title))
  (tellv btn setBezelStyle: #:type _NSUInteger 1)
  (tellv btn setTarget: target)
  (tellv btn setAction: #:type _SEL action-sel)
  btn)

(define (show-preferences!)
  (define existing (unbox *prefs-panel*))
  (cond
    [existing
     (tellv existing makeKeyAndOrderFront: #f)]
    [else
     (define panel
       (tell (tell NSWindow alloc)
             initWithContentRect: #:type _NSRect (NSMakeRect 300 300 380 180)
             styleMask: #:type _NSUInteger 15
             backing: #:type _NSUInteger 2
             defer: #:type _BOOL #f))
     (tellv panel setTitle: (NSStr "Preferences"))
     (define cv (tell panel contentView))

     (tellv cv addSubview: (make-label "Font Name:" 20 120 100 24))
     (define name-fld (make-field (or (current-font-name) "Menlo") 130 120 230 24))
     (tellv cv addSubview: name-fld)
     (set-box! *font-name-field* name-fld)

     (tellv cv addSubview: (make-label "Font Size:" 20 80 100 24))
     (define sz (or (current-font-size) 13.0))
     (define size-fld
       (make-field (number->string (inexact->exact (round sz))) 130 80 80 24))
     (tellv cv addSubview: size-fld)
     (set-box! *font-size-field* size-fld)

     (tellv cv addSubview:
            (make-button "Apply" 170 20 90 32
                         prefs-controller (selector applyPrefs:)))
     (tellv cv addSubview:
            (make-button "Close" 270 20 90 32
                         prefs-controller (selector closePrefs:)))

     (set-box! *prefs-panel* panel)
     (tellv panel makeKeyAndOrderFront: #f)]))

;; ---- Menu delegate ----------------------------------------------------------
(define-objc-class RktMenuController NSObject ()
  [- _void (showPreferences: [_id sender])
     (defer! show-preferences!)]
  [- _void (newFile: [_id sender])
     (defer!
      (lambda ()
        (define panel (tell NSSavePanel savePanel))
        (tellv panel setTitle: (NSStr "New File"))
        (tellv panel setNameFieldStringValue: (NSStr "untitled.rkt"))
        (define cur (unbox *current-file-path*))
        (when cur
          (define dir (path->string (simplify-path (build-path cur 'up))))
          (tellv panel setDirectoryURL: (tell NSURL fileURLWithPath: (NSStr dir))))
        (define result (tell #:type _NSInteger panel runModal))
        (when (= result 1)
          (define url (tell panel URL))
          (define path (nsstring->string (tell url path)))
          (call-with-output-file path #:exists 'replace
            (lambda (out) (write-string "#lang racket\n\n" out)))
          (editor-open-file! path))))]
  [- _void (openFile: [_id sender])
     (defer!
      (lambda ()
        (define panel (tell NSOpenPanel openPanel))
        (define cur (unbox *current-file-path*))
        (when cur
          (define dir (path->string (simplify-path (build-path cur 'up))))
          (tellv panel setDirectoryURL: (tell NSURL fileURLWithPath: (NSStr dir))))
        (tellv panel setCanChooseFiles: #:type _BOOL #t)
        (tellv panel setCanChooseDirectories: #:type _BOOL #f)
        (tellv panel setAllowsMultipleSelection: #:type _BOOL #f)
        (define result (tell #:type _NSInteger panel runModal))
        (when (= result 1)
          (define urls (tell panel URLs))
          (define url (tell urls objectAtIndex: #:type _NSUInteger 0))
          (define path (nsstring->string (tell url path)))
          (editor-open-file! path))))]
  [- _void (saveFile: [_id sender])
     (defer! editor-save-file!)]
  [- _void (runModule: [_id sender])
     (defer! repl-run-module!)]
  [- _void (closeFile: [_id sender])
     (defer! close-active-tab!)]
  [- _void (makeFontBigger: [_id sender])
     (defer!
      (lambda ()
        (define tv (unbox *editor-view*))
        (when tv
          (define sz (or (current-font-size) 13.0))
          (define name (or (current-font-name) "Menlo"))
          (define font (monospace name (+ sz 1.0)))
          (when font
            (tellv tv setFont: font)
            (define src (nsstring->string (tell tv string)))
            (apply-highlights! tv src)
            (redraw-gutter!)))))]
  [- _void (makeFontSmaller: [_id sender])
     (defer!
      (lambda ()
        (define tv (unbox *editor-view*))
        (when tv
          (define sz (or (current-font-size) 13.0))
          (define name (or (current-font-name) "Menlo"))
          (define font (monospace name (max 8.0 (- sz 1.0))))
          (when font
            (tellv tv setFont: font)
            (define src (nsstring->string (tell tv string)))
            (apply-highlights! tv src)
            (redraw-gutter!)))))])

(define menu-controller (tell (tell RktMenuController alloc) init))

;; ---- Full menu bar ----------------------------------------------------------
(define (menu-item title action-name key)
  (tell (tell NSMenuItem alloc)
        initWithTitle: (NSStr title)
        action: #:type _SEL (if action-name (sel-reg action-name) #f)
        keyEquivalent: (NSStr key)))

(define (sep) (tell NSMenuItem separatorItem))

(define (install-full-menu!)
  (define app (tell NSApplication sharedApplication))
  (define bar (tell (tell NSMenu alloc) init))

  ;; ---- Application menu ----
  (define app-holder (tell (tell NSMenuItem alloc) init))
  (tellv bar addItem: app-holder)
  (define app-menu (tell (tell NSMenu alloc) initWithTitle: (NSStr "rktlens")))
  (tellv app-menu addItem: (menu-item "About rktlens" "orderFrontStandardAboutPanel:" ""))
  (tellv app-menu addItem: (sep))
  (define prefs-item (menu-item "Preferences…" #f ","))
  (tellv prefs-item setTarget: menu-controller)
  (tellv prefs-item setAction: #:type _SEL (selector showPreferences:))
  (tellv app-menu addItem: prefs-item)
  (tellv app-menu addItem: (sep))
  (define quit-item (menu-item "Quit rktlens" "terminate:" "q"))
  (tellv quit-item setTarget: app)
  (tellv app-menu addItem: quit-item)
  (tellv app-holder setSubmenu: app-menu)

  ;; ---- File menu ----
  (define file-holder (tell (tell NSMenuItem alloc) init))
  (tellv bar addItem: file-holder)
  (define file-menu (tell (tell NSMenu alloc) initWithTitle: (NSStr "File")))
  (define new-item (menu-item "New" #f "n"))
  (tellv new-item setTarget: menu-controller)
  (tellv new-item setAction: #:type _SEL (selector newFile:))
  (tellv file-menu addItem: new-item)
  (define open-item (menu-item "Open…" #f "o"))
  (tellv open-item setTarget: menu-controller)
  (tellv open-item setAction: #:type _SEL (selector openFile:))
  (tellv file-menu addItem: open-item)
  (tellv file-menu addItem: (sep))
  (define save-item (menu-item "Save" #f "s"))
  (tellv save-item setTarget: menu-controller)
  (tellv save-item setAction: #:type _SEL (selector saveFile:))
  (tellv file-menu addItem: save-item)
  (tellv file-menu addItem: (sep))
  (define close-item (menu-item "Close" #f "w"))
  (tellv close-item setTarget: menu-controller)
  (tellv close-item setAction: #:type _SEL (selector closeFile:))
  (tellv file-menu addItem: close-item)
  (tellv file-holder setSubmenu: file-menu)

  ;; ---- Edit menu ----
  (define edit-holder (tell (tell NSMenuItem alloc) init))
  (tellv bar addItem: edit-holder)
  (define edit-menu (tell (tell NSMenu alloc) initWithTitle: (NSStr "Edit")))
  (tellv edit-menu addItem: (menu-item "Undo" "undo:" "z"))
  (tellv edit-menu addItem: (menu-item "Redo" "redo:" "Z"))
  (tellv edit-menu addItem: (sep))
  (tellv edit-menu addItem: (menu-item "Cut" "cut:" "x"))
  (tellv edit-menu addItem: (menu-item "Copy" "copy:" "c"))
  (tellv edit-menu addItem: (menu-item "Paste" "paste:" "v"))
  (tellv edit-menu addItem: (sep))
  (tellv edit-menu addItem: (menu-item "Select All" "selectAll:" "a"))
  (tellv edit-holder setSubmenu: edit-menu)

  ;; ---- Run menu ----
  (define run-holder (tell (tell NSMenuItem alloc) init))
  (tellv bar addItem: run-holder)
  (define run-menu (tell (tell NSMenu alloc) initWithTitle: (NSStr "Run")))
  (define run-item (menu-item "Run Module" #f "r"))
  (tellv run-item setTarget: menu-controller)
  (tellv run-item setAction: #:type _SEL (selector runModule:))
  (tellv run-menu addItem: run-item)
  (tellv run-holder setSubmenu: run-menu)

  ;; ---- Format menu ----
  (define fmt-holder (tell (tell NSMenuItem alloc) init))
  (tellv bar addItem: fmt-holder)
  (define fmt-menu (tell (tell NSMenu alloc) initWithTitle: (NSStr "Format")))
  (tellv fmt-menu addItem: (menu-item "Show Fonts" "orderFrontFontPanel:" "t"))
  (tellv fmt-menu addItem: (sep))
  (define bigger (menu-item "Bigger" #f "+"))
  (tellv bigger setTarget: menu-controller)
  (tellv bigger setAction: #:type _SEL (selector makeFontBigger:))
  (tellv fmt-menu addItem: bigger)
  (define smaller (menu-item "Smaller" #f "-"))
  (tellv smaller setTarget: menu-controller)
  (tellv smaller setAction: #:type _SEL (selector makeFontSmaller:))
  (tellv fmt-menu addItem: smaller)
  (tellv fmt-holder setSubmenu: fmt-menu)

  (tellv app setMainMenu: bar))

;; ---- Toolbar ----------------------------------------------------------------
(define toolbar-item-ids '("run" "stop"))

(define-objc-class RktToolbarDelegate NSObject ()
  [- _id (toolbar: [_id tb] itemForItemIdentifier: [_id ident] willBeInsertedIntoToolbar: [_BOOL flag])
     (define name (nsstring->string ident))
     (define item (tell (tell NSToolbarItem alloc) initWithItemIdentifier: ident))
     (cond
       [(string=? name "run")
        (tellv item setLabel: (NSStr "Run"))
        (define img (tell NSImage imageWithSystemSymbolName: (NSStr "play.fill")
                                  accessibilityDescription: (NSStr "Run")))
        (when img
          (tellv item setImage: img))
        (tellv item setTarget: menu-controller)
        (tellv item setAction: #:type _SEL (selector runModule:))]
       [(string=? name "stop")
        (tellv item setLabel: (NSStr "Stop"))
        (define img (tell NSImage imageWithSystemSymbolName: (NSStr "stop.fill")
                                  accessibilityDescription: (NSStr "Stop")))
        (when img (tellv item setImage: img))])
     item]

  [- _id (toolbarAllowedItemIdentifiers: [_id tb])
     (define arr (tell (tell NSMutableArray alloc) init))
     (tellv arr addObject: (NSStr "run"))
     arr]

  [- _id (toolbarDefaultItemIdentifiers: [_id tb])
     (define arr (tell (tell NSMutableArray alloc) init))
     (tellv arr addObject: (NSStr "run"))
     arr])

(import-class NSMutableArray)

(define toolbar-delegate (tell (tell RktToolbarDelegate alloc) init))

(define (install-toolbar! win)
  (define tb (tell (tell NSToolbar alloc) initWithIdentifier: (NSStr "rktlens-toolbar")))
  (tellv tb setDelegate: toolbar-delegate)
  (tellv tb setDisplayMode: #:type _NSUInteger 1)
  (tellv win setToolbar: tb))
