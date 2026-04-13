#lang racket/base

;; ide.rkt — rktlens IDE-specific state, editor view builder, and highlighting.
;; Requires rackit and re-provides everything from it so downstream
;; modules only need (require "ide.rkt").

(require ffi/unsafe
         ffi/unsafe/objc
         rackit)

(provide (all-from-out rackit)
         (all-defined-out))

;; ---- Late-binding extension points ------------------------------------------
(define *text-view-class* (box NSTextView))
(define (set-text-view-class! cls) (set-box! *text-view-class* cls))

(define *line-number-installer* (box #f))
(define (set-line-number-installer! f) (set-box! *line-number-installer* f))

(define *on-text-change* (box #f))
(define (set-on-text-change! f) (set-box! *on-text-change* f))

(define *next-tab-fn* (box #f))
(define *prev-tab-fn* (box #f))
(define *eval-in-repl-fn* (box #f))
(define (set-eval-in-repl! f) (set-box! *eval-in-repl-fn* f))
(define (set-tab-nav! next prev)
  (set-box! *next-tab-fn* next)
  (set-box! *prev-tab-fn* prev))
(define (next-tab!) (define f (unbox *next-tab-fn*)) (when f (f)))
(define (prev-tab!) (define f (unbox *prev-tab-fn*)) (when f (f)))

;; ---- Editor state -----------------------------------------------------------
(define *editor-view*        (box #f))
(define *highlighter*        (box #f))
(define *default-text-color* (box #f))

(define (set-highlighter! f)        (set-box! *highlighter* f))
(define (set-default-text-color! c) (set-box! *default-text-color* c))

;; ---- Syntax highlighting (lexer-based) --------------------------------------
(define (apply-highlights! tv source)
  (define h (unbox *highlighter*))
  (when (and tv h)
    (define tokens (h source))
    (define storage (tell tv textStorage))
    (define total (tell #:type _NSUInteger storage length))
    (define um (tell tv undoManager))
    (when um (tellv um disableUndoRegistration))
    (tellv storage beginEditing)
    (define default (unbox *default-text-color*))
    (when default
      (tellv storage addAttribute: NS-FG-COLOR-NSSTR
                     value: default
                     range: #:type _NSRange (make-NSRange 0 total)))
    (for ([tok (in-list tokens)])
      (define color (car tok))
      (define loc  (cadr tok))
      (define rlen (caddr tok))
      (when (and color (> rlen 0) (<= (+ loc rlen) total))
        (tellv storage addAttribute: NS-FG-COLOR-NSSTR
                       value: color
                       range: #:type _NSRange (make-NSRange loc rlen))))
    (tellv storage endEditing)
    (when um (tellv um enableUndoRegistration))))

(define (set-editor-text! s)
  (define tv (unbox *editor-view*))
  (when tv
    (define um (tell tv undoManager))
    (when um (tellv um disableUndoRegistration))
    (tellv tv setString: (NSStr s))
    (when um (tellv um enableUndoRegistration))
    (when um (tellv um removeAllActions))
    (apply-highlights! tv s)
    (tellv tv setNeedsDisplay: #:type _BOOL #t)))

;; ---- Flat table view (legacy, used by outline-view DSL macro) ---------------
(define *file-list* (box '()))
(define *on-select* (box (lambda (n) (void))))
(define (set-file-list! lst)  (set-box! *file-list* lst))
(define (set-on-select! p)    (set-box! *on-select* p))

(define-objc-class RktDataSource NSObject ()
  [- _NSInteger (numberOfRowsInTableView: [_id tv])
      (length (unbox *file-list*))]
  [- _id (tableView: [_id tv]
           objectValueForTableColumn: [_id col]
           row: [_NSInteger row])
      (define files (unbox *file-list*))
      (if (and (>= row 0) (< row (length files)))
          (NSStr (list-ref files row))
          (NSStr ""))])

(define-objc-class RktTableDelegate NSObject ()
  [- _void (tableAction: [_id sender])
      (define row (tell #:type _NSInteger sender selectedRow))
      (define files (unbox *file-list*))
      (when (and (>= row 0) (< row (length files)))
        ((unbox *on-select*) (list-ref files row)))])

(define data-source-instance    (tell (tell RktDataSource alloc) init))
(define table-delegate-instance (tell (tell RktTableDelegate alloc) init))

(define (make-table-view data on-select width)
  (when data      (set-file-list! data))
  (when on-select (set-on-select! on-select))
  (define w-val (exact->inexact width))
  (define frame (NSMakeRect 0 0 w-val 800))
  (define scroll
    (tell (tell NSScrollView alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv scroll setHasVerticalScroller: #:type _BOOL #t)
  (define table
    (tell (tell NSTableView alloc)
          initWithFrame: #:type _NSRect frame))
  (define col
    (tell (tell NSTableColumn alloc)
          initWithIdentifier: (NSStr "name")))
  (tellv col setWidth: #:type _CGFloat (- w-val 10.0))
  (define cell (tell (tell NSTextFieldCell alloc) init))
  (tellv col setDataCell: cell)
  (tellv table addTableColumn: col)
  (tellv table setDataSource: data-source-instance)
  (tellv table setTarget: table-delegate-instance)
  (tellv table setAction: #:type _SEL (selector tableAction:))
  (tellv table setHeaderView: #f)
  (tellv table reloadData)
  (tellv scroll setDocumentView: table)
  (wrap-view-in-vc scroll))

;; ---- Text change observer (fires on-text-change callback) ------------------
(import-class NSNotificationCenter)

(define-objc-class RktTextChangeObserver NSObject ()
  [- _void (textDidChange: [_id notification])
     (define cb (unbox *on-text-change*))
     (when cb (cb))])

(define text-change-observer (tell (tell RktTextChangeObserver alloc) init))

;; ---- Editor text view builder -----------------------------------------------
(define (make-text-view storage font highlights)
  (define frame (NSMakeRect 0 0 900 800))
  (define scroll
    (tell (tell NSScrollView alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv scroll setHasVerticalScroller: #:type _BOOL #t)
  (define tv
    (tell (tell (unbox *text-view-class*) alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv tv setEditable:  #:type _BOOL #t)
  (tellv tv setRichText:  #:type _BOOL #f)
  (tellv tv setAllowsUndo: #:type _BOOL #t)
  (define beans-bg (hex->NSColor "#111111"))
  (define beans-fg (hex->NSColor "#ebebd8"))
  (tellv tv setBackgroundColor:    beans-bg)
  (tellv tv setTextColor:          beans-fg)
  (tellv tv setInsertionPointColor: beans-fg)
  (set-default-text-color! beans-fg)
  (when font (tellv tv setFont: font))
  (when (procedure? highlights) (set-highlighter! highlights))
  (when storage (tellv tv setString: (NSStr storage)))
  (set-box! *editor-view* tv)
  (when storage (apply-highlights! tv storage))
  (tellv scroll setDocumentView: tv)
  (define installer (unbox *line-number-installer*))
  (when installer (installer scroll tv))
  ;; Observe text changes for debounced re-analysis
  (define nc (tell NSNotificationCenter defaultCenter))
  (tellv nc addObserver: text-change-observer
            selector: #:type _SEL (selector textDidChange:)
            name: (NSStr "NSTextDidChangeNotification")
            object: tv)
  (wrap-view-in-vc scroll))
