#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/set
         racket/string
         syntax-color/racket-lexer
         "ide.rkt"
         "editor.rkt")

(provide show-completion-popup!
         hide-completion-popup!
         completion-visible?
         completion-navigate!
         complete-selection!)

(import-class NSWindow NSTableView NSTableColumn NSScrollView
              NSTextFieldCell NSView)

;; ---- Gather candidates from current buffer + annotations --------------------
(define (file-symbols src)
  (define in (open-input-string src))
  (port-count-lines! in)
  (let loop ([syms (set)])
    (define-values (lexeme type _paren start end)
      (with-handlers ([exn:fail? (lambda _ (values #f 'eof #f #f #f))])
        (racket-lexer in)))
    (cond
      [(eq? type 'eof) (set->list syms)]
      [(eq? type 'symbol)
       (define s (cond [(string? lexeme) lexeme]
                       [(symbol? lexeme) (symbol->string lexeme)]
                       [else #f]))
       (loop (if s (set-add syms s) syms))]
      [else (loop syms)])))

(define (annotation-identifiers)
  (define anns (get-annotations))
  (define ids (set))
  (for ([ann (in-list anns)])
    (define tag (vector-ref ann 0))
    (case tag
      [(syncheck:add-definition-target/phase-level+space)
       (define id (vector-ref ann 3))
       (when (symbol? id) (set! ids (set-add ids (symbol->string id))))
       (when (string? id) (set! ids (set-add ids id)))]
      [else (void)]))
  (set->list ids))

(define (get-candidates src prefix)
  (define all (set-union (list->set (file-symbols src))
                         (list->set (annotation-identifiers))))
  (define filtered
    (for/list ([s (in-set all)]
               #:when (and (string-prefix? s prefix)
                           (not (string=? s prefix))))
      s))
  (sort filtered string<?))

;; ---- Completion popup state -------------------------------------------------
(define *completion-window* (box #f))
(define *completion-items*  (box '()))
(define *completion-sel*    (box 0))
(define *completion-prefix* (box ""))
(define *completion-start*  (box 0))

(define (completion-visible?)
  (define w (unbox *completion-window*))
  (and w (tell #:type _BOOL w isVisible)))

;; ---- Data source for the completion table -----------------------------------
(define-objc-class RktCompletionSource NSObject ()
  [- _NSInteger (numberOfRowsInTableView: [_id tv])
     (length (unbox *completion-items*))]
  [- _id (tableView: [_id tv]
          objectValueForTableColumn: [_id col]
          row: [_NSInteger row])
     (define items (unbox *completion-items*))
     (if (and (>= row 0) (< row (length items)))
         (NSStr (list-ref items row))
         (NSStr ""))])

(define completion-source (tell (tell RktCompletionSource alloc) init))

;; ---- Build / show the popup ------------------------------------------------
(define *completion-table* (box #f))

(define (show-completion-popup! tv)
  (define storage (tell tv textStorage))
  (define src (nsstring->string (tell storage string)))
  (define sel (tell #:type _NSRange tv selectedRange))
  (define cursor-pos (NSRange-location sel))

  ;; Find prefix: scan backward for identifier characters
  (define prefix-start
    (let loop ([i cursor-pos])
      (cond
        [(zero? i) i]
        [else
         (define ch (string-ref src (sub1 i)))
         (if (or (char-alphabetic? ch) (char-numeric? ch)
                 (char=? ch #\-) (char=? ch #\_) (char=? ch #\?)
                 (char=? ch #\!) (char=? ch #\>))
             (loop (sub1 i))
             i)])))
  (define prefix (substring src prefix-start cursor-pos))
  (when (> (string-length prefix) 0)
    (define candidates (get-candidates src prefix))
    (when (> (length candidates) 0)
      (set-box! *completion-items* candidates)
      (set-box! *completion-sel* 0)
      (set-box! *completion-prefix* prefix)
      (set-box! *completion-start* prefix-start)

      ;; Position: get screen rect at prefix start (not cursor end)
      (define rect
        (tell #:type _NSRect tv
              firstRectForCharacterRange: #:type _NSRange (make-NSRange prefix-start 0)
              actualRange: #:type _pointer #f))

      (define existing (unbox *completion-window*))
      (when existing (tellv existing close))

      ;; Create a small borderless window below the current line
      (define row-h 26)
      (define popup-h (max 34 (min (* (length candidates) row-h) 260)))
      (define popup-w 250)
      (define popup-rect
        (NSMakeRect (NSPoint-x (NSRect-origin rect))
                    (- (NSPoint-y (NSRect-origin rect)) popup-h)
                    popup-w popup-h))
      (define popup
        (tell (tell NSWindow alloc)
              initWithContentRect: #:type _NSRect popup-rect
              styleMask: #:type _NSUInteger 0
              backing: #:type _NSUInteger 2
              defer: #:type _BOOL #f))
      (tellv popup setBackgroundColor: (hex->NSColor "#1c1c1c"))
      (tellv popup setLevel: #:type _NSInteger 3)
      (tellv popup setHasShadow: #:type _BOOL #t)
      (tellv (tell popup contentView) setWantsLayer: #:type _BOOL #t)
      (define popup-layer (tell (tell popup contentView) layer))
      (tellv popup-layer setCornerRadius: #:type _CGFloat 8.0)
      (tellv popup-layer setMasksToBounds: #:type _BOOL #t)

      ;; Table view
      (define sv (tell (tell NSScrollView alloc)
                       initWithFrame: #:type _NSRect
                       (NSMakeRect 0 0 popup-w popup-h)))
      (tellv sv setHasVerticalScroller: #:type _BOOL #t)
      (define tbl (tell (tell NSTableView alloc)
                        initWithFrame: #:type _NSRect
                        (NSMakeRect 0 0 popup-w popup-h)))
      (define col (tell (tell NSTableColumn alloc)
                        initWithIdentifier: (NSStr "c")))
      (tellv col setWidth: #:type _CGFloat (- popup-w 16.0))
      (define cell (tell (tell NSTextFieldCell alloc) init))
      (tellv cell setTextColor: (hex->NSColor "#ebebd8"))
      (tellv col setDataCell: cell)
      (tellv tbl addTableColumn: col)
      (tellv tbl setDataSource: completion-source)
      (tellv tbl setHeaderView: #f)
      (tellv tbl setRowHeight: #:type _CGFloat 22.0)
      (tellv tbl setBackgroundColor: (hex->NSColor "#1c1c1c"))
      (tellv tbl reloadData)
      (tellv sv setDocumentView: tbl)
      (tellv popup setContentView: sv)

      (set-box! *completion-table* tbl)
      (set-box! *completion-window* popup)
      (tellv popup orderFront: #f))))

(define (hide-completion-popup!)
  (define w (unbox *completion-window*))
  (when w
    (tellv w close)
    (set-box! *completion-window* #f)
    (set-box! *completion-items* '())))

(define (completion-navigate! delta)
  (define items (unbox *completion-items*))
  (define sel (unbox *completion-sel*))
  (define new-sel (max 0 (min (sub1 (length items)) (+ sel delta))))
  (set-box! *completion-sel* new-sel)
  (define tbl (unbox *completion-table*))
  (when tbl
    (define idx-set (tell NSIndexSet indexSetWithIndex: #:type _NSUInteger new-sel))
    (tellv tbl selectRowIndexes: idx-set byExtendingSelection: #:type _BOOL #f)
    (tellv tbl scrollRowToVisible: #:type _NSInteger new-sel)))

(import-class NSIndexSet)

(define (complete-selection! tv)
  (define items (unbox *completion-items*))
  (define sel (unbox *completion-sel*))
  (when (and (>= sel 0) (< sel (length items)))
    (define completion (list-ref items sel))
    (define start (unbox *completion-start*))
    (define prefix (unbox *completion-prefix*))
    (define plen (string-length prefix))
    (tellv tv setSelectedRange: #:type _NSRange (make-NSRange start plen))
    (tellv tv insertText: (NSStr completion))
    (hide-completion-popup!)))
