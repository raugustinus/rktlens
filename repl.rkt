#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/port
         racket/string
         "ide.rkt"
         "editor.rkt"
         "editor-view.rkt")

(provide make-repl-view repl-run-module! repl-eval-input! *repl-view*
         repl-append! repl-append-colored! *repl-namespace*)

(import-class NSTextView NSScrollView NSFont NSColor NSMenu NSMenuItem)

;; ---- REPL state -------------------------------------------------------------
(define *repl-view*    (box #f))
(define *sandbox*      (box #f))
(define *input-start*  (box 0))
(define *repl-history* (box '()))
(define *history-idx*  (box 0))

;; ---- Append text to the REPL view ------------------------------------------
(define (repl-append! str)
  (define tv (unbox *repl-view*))
  (when tv
    (define storage (tell tv textStorage))
    (define len (tell #:type _NSUInteger storage length))
    (define um (tell tv undoManager))
    (when um (tellv um disableUndoRegistration))
    (tellv tv setSelectedRange: #:type _NSRange (make-NSRange len 0))
    (tellv tv insertText: (NSStr str))
    (when um (tellv um enableUndoRegistration))
    (define new-len (tell #:type _NSUInteger storage length))
    (tellv tv scrollRangeToVisible: #:type _NSRange (make-NSRange new-len 0))))

(define (repl-append-colored! str color)
  (define tv (unbox *repl-view*))
  (when tv
    (define storage (tell tv textStorage))
    (define before (tell #:type _NSUInteger storage length))
    (repl-append! str)
    (define after (tell #:type _NSUInteger storage length))
    (define um (tell tv undoManager))
    (when um (tellv um disableUndoRegistration))
    (tellv storage beginEditing)
    (tellv storage addAttribute: (NSStr "NSColor")
                   value: color
                   range: #:type _NSRange (make-NSRange before (- after before)))
    (tellv storage endEditing)
    (when um (tellv um enableUndoRegistration))))

(define (repl-prompt!)
  (define path (unbox *current-file-path*))
  (define name (if path (let-values ([(base name dir?) (split-path path)]) (path->string name)) "racket"))
  (repl-append-colored! (format "~a> " name) (hex->NSColor "#8197bf"))
  (define tv (unbox *repl-view*))
  (when tv
    (define storage (tell tv textStorage))
    (set-box! *input-start* (tell #:type _NSUInteger storage length))))

;; ---- Sandbox management -----------------------------------------------------
(define *repl-namespace* (box #f))
(define *repl-output*    (box (open-output-string)))

(define (create-sandbox! path)
  (define p (if (string? path) (string->path path) path))
  (define ns (make-base-namespace))
  (with-handlers ([exn:fail? (lambda (e)
                               (repl-append-colored!
                                (format "error: ~a~n" (exn-message e))
                                (hex->NSColor "#ff005b")))])
    (define out (open-output-string))
    (define err (open-output-string))
    (parameterize ([current-namespace ns]
                   [current-output-port out]
                   [current-error-port err])
      (dynamic-require p #f))
    (define stdout (get-output-string out))
    (define stderr (get-output-string err))
    (when (> (string-length stdout) 0) (repl-append! stdout))
    (when (> (string-length stderr) 0)
      (repl-append-colored! stderr (hex->NSColor "#ff005b")))
    (define mod-ns
      (parameterize ([current-namespace ns])
        (module->namespace p)))
    (set-box! *repl-namespace* mod-ns)
    (set-box! *repl-output* (open-output-string))
    (set-box! *sandbox* #t)))

;; ---- Run current module (Cmd-R) ---------------------------------------------
(define (repl-run-module!)
  (define path (unbox *current-file-path*))
  (cond
    [(not path)
     (repl-append-colored! "no file open\n" (hex->NSColor "#ff005b"))
     (repl-prompt!)]
    [else
     (repl-append-colored!
      (format "--- running ~a ---~n"
              (let-values ([(b n d?) (split-path path)]) n))
      (hex->NSColor "#6d6d6d"))
     (create-sandbox! path)
     (repl-prompt!)]))

;; ---- Evaluate input line ----------------------------------------------------
(define (repl-eval-input!)
  (define tv (unbox *repl-view*))
  (when tv
    (define storage (tell tv textStorage))
    (define total (tell #:type _NSUInteger storage length))
    (define start (unbox *input-start*))
    (define input-len (- total start))
    (define input
      (if (> input-len 0)
          (nsstring->string
           (tell (tell storage string)
                 substringWithRange: #:type _NSRange (make-NSRange start input-len)))
          ""))
    (define trimmed (string-trim input))
    (repl-append! "\n")
    (when (> (string-length trimmed) 0)
      (set-box! *repl-history* (cons trimmed (unbox *repl-history*)))
      (set-box! *history-idx* 0)
      (define ns (unbox *repl-namespace*))
      (cond
        [(not ns)
         (repl-append-colored! "no module loaded — run with Cmd-R first\n"
                               (hex->NSColor "#ff005b"))]
        [else
         (with-handlers ([exn:fail? (lambda (e)
                                      (repl-append-colored!
                                       (format "~a~n" (exn-message e))
                                       (hex->NSColor "#ff005b")))])
           (define out (open-output-string))
           (define err (open-output-string))
           (define result
             (parameterize ([current-namespace ns]
                            [current-output-port out]
                            [current-error-port err])
               (eval (read (open-input-string trimmed)))))
           (define stdout (get-output-string out))
           (define stderr (get-output-string err))
           (when (> (string-length stdout) 0) (repl-append! stdout))
           (when (> (string-length stderr) 0)
             (repl-append-colored! stderr (hex->NSColor "#ff005b")))
           (when (not (void? result))
             (repl-append-colored! (format "~v~n" result)
                                   (hex->NSColor "#cee318"))))]))
    (repl-prompt!)))

;; ---- REPL text view subclass ------------------------------------------------
(define-objc-class RktReplView NSTextView ()
  [- _void (keyDown: [_id event])
     (define chars (nsstring->string (tell event characters)))
     (define flags (tell #:type _NSUInteger event modifierFlags))
     (define cmd? (> (bitwise-and flags #x100000) 0))
     (define keycode (tell #:type _ushort event keyCode))
     (cond
       [(string=? chars "\r")
        (defer! repl-eval-input!)]
       [(and cmd? (string=? chars "r"))
        (defer! repl-run-module!)]
       ;; History: up/down
       [(= keycode 126)
        (define hist (unbox *repl-history*))
        (define idx (unbox *history-idx*))
        (when (< idx (length hist))
          (replace-input! self (list-ref hist idx))
          (set-box! *history-idx* (min (sub1 (length hist)) (add1 idx))))]
       [(= keycode 125)
        (define idx (unbox *history-idx*))
        (when (> idx 0)
          (set-box! *history-idx* (sub1 idx))
          (define hist (unbox *repl-history*))
          (replace-input! self (list-ref hist (sub1 idx))))]
       ;; Auto-close brackets and quotes
       [(member chars '("(" "[" "{" "\""))
        (auto-close-pair! self chars)]
       ;; Skip over closing bracket if already there
       [(member chars '(")" "]" "}"))
        (skip-or-insert-close! self chars)]
       ;; Backspace: delete matching pair
       [(and (= keycode 51) (delete-matching-pair? self))
        (void)]
       [else (super-tell keyDown: event)])]

  [- _id (menuForEvent: [_id event])
     (define menu (tell (tell NSMenu alloc) init))
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Copy") action: #:type _SEL (selector copy:)
                  keyEquivalent: (NSStr "")))
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Paste") action: #:type _SEL (selector paste:)
                  keyEquivalent: (NSStr "")))
     (tellv menu addItem: (tell NSMenuItem separatorItem))
     (define clear-item
       (tell (tell NSMenuItem alloc)
             initWithTitle: (NSStr "Clear Output")
             action: #:type _SEL (selector clearRepl:)
             keyEquivalent: (NSStr "")))
     (tellv clear-item setTarget: repl-ctx-handler)
     (tellv menu addItem: clear-item)
     menu])

(define (replace-input! tv text)
  (define storage (tell tv textStorage))
  (define total (tell #:type _NSUInteger storage length))
  (define start (unbox *input-start*))
  (define um (tell tv undoManager))
  (when um (tellv um disableUndoRegistration))
  (tellv tv setSelectedRange: #:type _NSRange (make-NSRange start (- total start)))
  (tellv tv insertText: (NSStr text))
  (when um (tellv um enableUndoRegistration)))

(define-objc-class RktReplContextHandler NSObject ()
  [- _void (clearRepl: [_id sender])
     (define tv (unbox *repl-view*))
     (when tv
       (define um (tell tv undoManager))
       (when um (tellv um disableUndoRegistration))
       (tellv tv setString: (NSStr ""))
       (when um (tellv um enableUndoRegistration))
       (repl-append-colored! ";; output cleared\n" (hex->NSColor "#6d6d6d"))
       (repl-prompt!))])

(define repl-ctx-handler (tell (tell RktReplContextHandler alloc) init))

;; ---- Eval a string in the REPL (called from editor via late-binding) --------
(define (repl-eval-string! trimmed)
  (repl-append-colored! (format "~a~n" trimmed) (hex->NSColor "#6d6d6d"))
  (define ns (unbox *repl-namespace*))
  (cond
    [(not ns)
     (repl-append-colored! "no module loaded — run with Cmd-R first\n"
                           (hex->NSColor "#ff005b"))]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (repl-append-colored!
                                   (format "~a~n" (exn-message e))
                                   (hex->NSColor "#ff005b")))])
       (define out (open-output-string))
       (define err (open-output-string))
       (define result
         (parameterize ([current-namespace ns]
                        [current-output-port out]
                        [current-error-port err])
           (eval (read (open-input-string trimmed)))))
       (define stdout (get-output-string out))
       (define stderr (get-output-string err))
       (when (> (string-length stdout) 0) (repl-append! stdout))
       (when (> (string-length stderr) 0)
         (repl-append-colored! stderr (hex->NSColor "#ff005b")))
       (when (not (void? result))
         (repl-append-colored! (format "~v~n" result)
                               (hex->NSColor "#cee318"))))])
  (repl-prompt!))

(set-eval-in-repl! repl-eval-string!)

;; ---- Build the REPL view ---------------------------------------------------
(define (make-repl-view font)
  (define frame (NSMakeRect 0 0 900 200))
  (define scroll
    (tell (tell NSScrollView alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv scroll setHasVerticalScroller: #:type _BOOL #t)
  (define tv
    (tell (tell RktReplView alloc)
          initWithFrame: #:type _NSRect frame))
  (tellv tv setEditable: #:type _BOOL #t)
  (tellv tv setRichText: #:type _BOOL #f)
  (tellv tv setAllowsUndo: #:type _BOOL #t)
  (tellv tv setBackgroundColor: (hex->NSColor "#0a0a0a"))
  (tellv tv setTextColor: (hex->NSColor "#ebebd8"))
  (tellv tv setInsertionPointColor: (hex->NSColor "#ebebd8"))
  (when font (tellv tv setFont: font))
  (set-box! *repl-view* tv)
  (tellv scroll setDocumentView: tv)
  (repl-append-colored! ";; rktlens REPL — Cmd-R to run current file\n"
                        (hex->NSColor "#6d6d6d"))
  (repl-prompt!)
  (wrap-view-in-vc scroll))
