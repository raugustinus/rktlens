#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/path
         racket/list
         racket/string
         rackit)

(provide make-project-tree-view)

(import-class NSOutlineView NSScrollView NSTableColumn NSTextFieldCell
              NSIndexSet NSNotificationCenter NSTextField NSView NSStackView
              NSVisualEffectView NSImage NSImageView NSTableCellView
              NSImageSymbolConfiguration NSMenu NSMenuItem NSWorkspace NSURL
              NSAlert NSSavePanel)

;; ---- File tree data model ---------------------------------------------------
;; Items are NSString* of relative paths (e.g., "src", "src/foo.rkt", "main.rkt").
;; Root children have parent = #f (nil).

(define *tree-root*     (box #f))
(define *tree-children* (box (make-hash)))
(define *tree-items*    (box (make-hash)))
(define *on-file-select* (box (lambda (name) (void))))

(define skip-dirs '("compiled" ".git" "node_modules" ".DS_Store"))

(define (scan-tree! root)
  (define children (make-hash))
  (define items (make-hash))

  (define (scan dir rel-prefix)
    (define entries
      (with-handlers ([exn:fail? (lambda _ '())])
        (directory-list dir)))
    (define sorted
      (sort (for/list ([e (in-list entries)]
                       #:when (not (member (path->string e) skip-dirs))
                       #:when (not (string-prefix? (path->string e) ".")))
              e)
            (lambda (a b)
              (define a-dir? (directory-exists? (build-path dir a)))
              (define b-dir? (directory-exists? (build-path dir b)))
              (cond
                [(and a-dir? (not b-dir?)) #t]
                [(and (not a-dir?) b-dir?) #f]
                [else (string<? (path->string a) (path->string b))]))))

    (define child-keys '())
    (for ([e (in-list sorted)])
      (define rel (if (equal? rel-prefix "")
                      (path->string e)
                      (string-append rel-prefix "/" (path->string e))))
      (define full (build-path dir e))
      (define ns (tell (NSStr rel) retain))
      (hash-set! items rel ns)
      (set! child-keys (cons rel child-keys))
      (when (directory-exists? full)
        (scan full rel)))
    (hash-set! children rel-prefix (reverse child-keys)))

  (scan root "")
  (set-box! *tree-root* root)
  (set-box! *tree-children* children)
  (set-box! *tree-items* items))

(define (item->rel-path item)
  (if item (nsstring->string item) ""))

(define (children-of rel)
  (hash-ref (unbox *tree-children*) rel '()))

(define (nsitem rel)
  (hash-ref (unbox *tree-items*) rel #f))

(define (item-expandable? rel)
  (define root (unbox *tree-root*))
  (and root (directory-exists? (build-path root (string->path rel)))))

(define (display-name rel)
  (define parts (string-split rel "/"))
  (if (null? parts) rel (last parts)))

;; ---- Lucide icons (SVG embedded, colored) -----------------------------------
(import-class NSData)

(define (lucide-icon svg-str color-hex size)
  (define colored (string-replace svg-str "currentColor" color-hex))
  (define data (tell NSData dataWithBytes: #:type _bytes (string->bytes/utf-8 colored)
                            length: #:type _NSUInteger (bytes-length (string->bytes/utf-8 colored))))
  (define img (tell (tell NSImage alloc) initWithData: data))
  (when img (tellv img setSize: #:type _NSSize (make-NSSize size size)))
  img)

(define icon-folder
  (lucide-icon
   "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M20 20a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.9a2 2 0 0 1-1.69-.9L9.6 3.9A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z\"/></svg>"
   "#8a7560" 16.0))

(define icon-racket
  (lucide-icon
   "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M7 21L12 4\"/><path d=\"M10 12L18 21\"/></svg>"
   "#8197bf" 16.0))

(define icon-text
  (lucide-icon
   "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M6 22a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h8a2.4 2.4 0 0 1 1.704.706l3.588 3.588A2.4 2.4 0 0 1 20 8v12a2 2 0 0 1-2 2z\"/><path d=\"M14 2v5a1 1 0 0 0 1 1h5\"/><path d=\"M10 9H8\"/><path d=\"M16 13H8\"/><path d=\"M16 17H8\"/></svg>"
   "#6d8a6d" 16.0))

(define icon-doc
  (lucide-icon
   "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M6 22a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h8a2.4 2.4 0 0 1 1.704.706l3.588 3.588A2.4 2.4 0 0 1 20 8v12a2 2 0 0 1-2 2z\"/><path d=\"M14 2v5a1 1 0 0 0 1 1h5\"/></svg>"
   "#6d6d6d" 16.0))

(define (icon-for-item rel name)
  (cond
    [(item-expandable? rel) icon-folder]
    [(regexp-match? #rx"\\.rkt$" name) icon-racket]
    [(regexp-match? #rx"\\.md$|\\.txt$" name) icon-text]
    [else icon-doc]))

;; ---- NSOutlineView data source ----------------------------------------------
(define-objc-class RktTreeDataSource NSObject ()
  [- _NSInteger (outlineView: [_id ov] numberOfChildrenOfItem: [_id item])
     (define rel (item->rel-path item))
     (length (children-of rel))]

  [- _BOOL (outlineView: [_id ov] isItemExpandable: [_id item])
     (define rel (item->rel-path item))
     (item-expandable? rel)]

  [- _id (outlineView: [_id ov] child: [_NSInteger idx] ofItem: [_id item])
     (define rel (item->rel-path item))
     (define kids (children-of rel))
     (if (and (>= idx 0) (< idx (length kids)))
         (nsitem (list-ref kids idx))
         #f)]

  [- _id (outlineView: [_id ov]
          objectValueForTableColumn: [_id col]
          byItem: [_id item])
     (define rel (item->rel-path item))
     (NSStr (display-name rel))])

;; ---- Vertically centered cell -----------------------------------------------
(define-objc-class RktCenteredCell NSTextFieldCell ()
  [- _void (drawInteriorWithFrame: [_NSRect frame] inView: [_id view])
     (define cell-size (tell #:type _NSSize self cellSizeForBounds: #:type _NSRect frame))
     (define cell-h (NSSize-h cell-size))
     (define frame-h (NSSize-h (NSRect-size frame)))
     (define y-off (max 0.0 (/ (- frame-h cell-h) 2.0)))
     (define adjusted
       (make-NSRect (make-NSPoint (NSPoint-x (NSRect-origin frame))
                                  (+ (NSPoint-y (NSRect-origin frame)) y-off))
                    (make-NSSize (NSSize-w (NSRect-size frame)) cell-h)))
     (super-tell drawInteriorWithFrame: #:type _NSRect adjusted inView: view)])

(define tree-source (tell (tell RktTreeDataSource alloc) init))

;; ---- Selection delegate -----------------------------------------------------
(define (make-cell-view ov name icon)
  (define row-h 26.0)
  (define icon-sz 16.0)
  (define icon-y (/ (- row-h icon-sz) 2.0))
  (define cv
    (tell (tell NSTableCellView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 200 row-h)))
  (define img-view
    (tell (tell NSImageView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 2 icon-y icon-sz icon-sz)))
  (when icon (tellv img-view setImage: icon))
  (define text-h 16.0)
  (define text-y (/ (- row-h text-h) 2.0))
  (define tf
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 20 text-y 178 text-h)))
  (tellv tf setStringValue: (NSStr name))
  (tellv tf setEditable: #:type _BOOL #f)
  (tellv tf setBordered: #:type _BOOL #f)
  (tellv tf setDrawsBackground: #:type _BOOL #f)
  (tellv tf setTextColor: (hex->NSColor "#c8c8c8"))
  (tellv tf setFont: (tell NSFont systemFontOfSize: #:type _CGFloat 14.0))
  (tellv tf setLineBreakMode: #:type _NSUInteger 4)
  (tellv tf setUsesSingleLineMode: #:type _BOOL #t)
  (tellv cv addSubview: img-view)
  (tellv cv addSubview: tf)
  (tellv cv setTextField: tf)
  (tellv cv setImageView: img-view)
  cv)

(define-objc-class RktTreeDelegate NSObject ()
  [- _id (outlineView: [_id ov] viewForTableColumn: [_id col] item: [_id item])
     (define rel (item->rel-path item))
     (define name (display-name rel))
     (define icon (icon-for-item rel name))
     (make-cell-view ov name icon)]

  [- _void (outlineViewSelectionDidChange: [_id notification])
     (define ov (tell notification object))
     (define row (tell #:type _NSInteger ov selectedRow))
     (when (>= row 0)
       (define item (tell ov itemAtRow: #:type _NSInteger row))
       (when item
         (define rel (item->rel-path item))
         (define root (unbox *tree-root*))
         (when (and root (not (item-expandable? rel)))
           ((unbox *on-file-select*) rel))))])

(define tree-delegate (tell (tell RktTreeDelegate alloc) init))

;; ---- File tree context menu -------------------------------------------------
(define *context-ov* (box #f))

(define (selected-rel)
  (define ov (unbox *context-ov*))
  (and ov
       (let ([row (tell #:type _NSInteger ov selectedRow)])
         (and (>= row 0)
              (let ([item (tell ov itemAtRow: #:type _NSInteger row)])
                (and item (item->rel-path item)))))))

(define (selected-full-path)
  (define rel (selected-rel))
  (define root (unbox *tree-root*))
  (and rel root (build-path root rel)))

(define-objc-class RktTreeContextHandler NSObject ()
  [- _void (newFile: [_id sender])
     (defer!
      (lambda ()
        (define root (unbox *tree-root*))
        (when root
          (define panel (tell NSSavePanel savePanel))
          (tellv panel setTitle: (NSStr "New File"))
          (tellv panel setNameFieldStringValue: (NSStr "untitled.rkt"))
          (tellv panel setDirectoryURL:
                 (tell NSURL fileURLWithPath: (NSStr (path->string root))))
          (define result (tell #:type _NSInteger panel runModal))
          (when (= result 1)
            (define url (tell panel URL))
            (define path (nsstring->string (tell url path)))
            (call-with-output-file path #:exists 'replace
              (lambda (out) (write-string "#lang racket\n\n" out)))
            (scan-tree! root)
            (define ov (unbox *context-ov*))
            (when ov (tellv ov reloadData))))))]

  [- _void (deleteFile: [_id sender])
     (defer!
      (lambda ()
        (define p (selected-full-path))
        (when (and p (file-exists? p))
          (define alert (tell (tell NSAlert alloc) init))
          (tellv alert setMessageText:
                 (NSStr (format "Delete ~a?" (let-values ([(b n d?) (split-path p)]) n))))
          (tellv alert setInformativeText: (NSStr "This cannot be undone."))
          (tellv alert addButtonWithTitle: (NSStr "Delete"))
          (tellv alert addButtonWithTitle: (NSStr "Cancel"))
          (define result (tell #:type _NSInteger alert runModal))
          (when (= result 1000)
            (delete-file p)
            (define root (unbox *tree-root*))
            (when root
              (scan-tree! root)
              (define ov (unbox *context-ov*))
              (when ov (tellv ov reloadData)))))))]

  [- _void (revealInFinder: [_id sender])
     (define p (selected-full-path))
     (when p
       (tellv (tell NSWorkspace sharedWorkspace)
              selectFile: (NSStr (path->string p))
              inFileViewerRootedAtPath: (NSStr "")))])

(define tree-context-handler (tell (tell RktTreeContextHandler alloc) init))

(define (make-tree-context-menu)
  (define menu (tell (tell NSMenu alloc) init))
  (define new-item
    (tell (tell NSMenuItem alloc)
          initWithTitle: (NSStr "New File")
          action: #:type _SEL (selector newFile:)
          keyEquivalent: (NSStr "")))
  (tellv new-item setTarget: tree-context-handler)
  (tellv menu addItem: new-item)
  (tellv menu addItem: (tell NSMenuItem separatorItem))
  (define del-item
    (tell (tell NSMenuItem alloc)
          initWithTitle: (NSStr "Delete")
          action: #:type _SEL (selector deleteFile:)
          keyEquivalent: (NSStr "")))
  (tellv del-item setTarget: tree-context-handler)
  (tellv menu addItem: del-item)
  (tellv menu addItem: (tell NSMenuItem separatorItem))
  (define reveal-item
    (tell (tell NSMenuItem alloc)
          initWithTitle: (NSStr "Reveal in Finder")
          action: #:type _SEL (selector revealInFinder:)
          keyEquivalent: (NSStr "")))
  (tellv reveal-item setTarget: tree-context-handler)
  (tellv menu addItem: reveal-item)
  menu)

;; ---- Build the outline view -------------------------------------------------
(define (make-project-tree-view root on-select width)
  (scan-tree! root)
  (when on-select (set-box! *on-file-select* on-select))

  (define w-val (exact->inexact width))
  (define frame (NSMakeRect 0 0 w-val 800))
  (define scroll
    (tell (tell NSScrollView alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv scroll setHasVerticalScroller: #:type _BOOL #t)
  (tellv scroll setFocusRingType: #:type _NSUInteger 1)

  (define ov
    (tell (tell NSOutlineView alloc)
          initWithFrame: #:type _NSRect frame))
  (define col
    (tell (tell NSTableColumn alloc)
          initWithIdentifier: (NSStr "name")))
  (tellv col setWidth: #:type _CGFloat (- w-val 10.0))
  (tellv ov addTableColumn: col)
  (tellv ov setOutlineTableColumn: col)
  (tellv ov setDataSource: tree-source)
  (tellv ov setDelegate: tree-delegate)
  (tellv ov setHeaderView: #f)
  (tellv ov setRowHeight: #:type _CGFloat 26.0)
  (tellv ov setSelectionHighlightStyle: #:type _NSInteger 0)
  (tellv ov setFocusRingType: #:type _NSUInteger 1)
  ;; Transparent backgrounds so NSVisualEffectView blur shows through
  (tellv ov setBackgroundColor: (tell NSColor clearColor))
  (tellv scroll setDrawsBackground: #:type _BOOL #f)
  (tellv ov reloadData)
  (tellv ov setMenu: (make-tree-context-menu))
  (set-box! *context-ov* ov)

  ;; Expand the root level
  (define root-kids (children-of ""))
  (for ([rel (in-list root-kids)])
    (when (item-expandable? rel)
      (define ns (nsitem rel))
      (when ns (tellv ov expandItem: ns))))

  (tellv scroll setDocumentView: ov)

  ;; Outer container with padding
  (define pad 4.0)
  (define inner-w (- w-val (* pad 2)))
  (define container
    (tell (tell NSView alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 0 0 w-val 800)))
  (tellv container setAutoresizesSubviews: #:type _BOOL #t)

  ;; Rounded panel (NSBox)
  (define panel
    (tell (tell NSBox alloc)
          initWithFrame: #:type _NSRect (NSMakeRect pad pad inner-w (- 800 (* pad 2)))))
  (tellv panel setBoxType: #:type _NSUInteger 4)
  (tellv panel setTitlePosition: #:type _NSUInteger 0)
  (tellv panel setFillColor: (hex->NSColor "#161618"))
  (tellv panel setCornerRadius: #:type _CGFloat 10.0)
  (tellv panel setBorderWidth: #:type _CGFloat 0.0)
  (tellv panel setContentViewMargins: #:type _NSSize (make-NSSize 0.0 0.0))
  (tellv panel setAutoresizesSubviews: #:type _BOOL #t)
  (tellv panel setAutoresizingMask: #:type _NSUInteger 18)

  ;; Project header showing CWD folder name
  (define panel-h (- 800 (* pad 2)))
  (define header-h 28.0)
  (define dir-name
    (let-values ([(base name dir?) (split-path (simplify-path root))])
      (path->string name)))
  (define header
    (tell (tell NSTextField alloc)
          initWithFrame: #:type _NSRect (NSMakeRect 8 (- panel-h header-h) (- inner-w 16) header-h)))
  (tellv header setEditable: #:type _BOOL #f)
  (tellv header setBordered: #:type _BOOL #f)
  (tellv header setDrawsBackground: #:type _BOOL #f)
  (tellv header setTextColor: (hex->NSColor "#ababab"))
  (tellv header setFont: (tell NSFont systemFontOfSize: #:type _CGFloat 12.0
                                      weight: #:type _CGFloat 0.5))
  (tellv header setStringValue: (NSStr (string-append "  ▸ " dir-name)))
  ;; Width-sizable + flexible bottom margin = sticks to top
  (tellv header setAutoresizingMask: #:type _NSUInteger 10)

  ;; Scroll fills below header (width + height sizable)
  (tellv scroll setFrame: #:type _NSRect (NSMakeRect 0 0 inner-w (- panel-h header-h)))
  (tellv scroll setAutoresizingMask: #:type _NSUInteger 18)

  (tellv panel addSubview: header)
  (tellv panel addSubview: scroll)
  (tellv container addSubview: panel)

  (wrap-view-in-vc container))
