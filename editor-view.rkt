#lang racket/base

(require ffi/unsafe
         ffi/unsafe/objc
         racket/path
         racket/string
         "ide.rkt"
         "editor.rkt"
         "completion.rkt"
         "search.rkt")

(provide RktEditorView *vim-enabled* *vim-mode*
         auto-close-pair! skip-or-insert-close! delete-matching-pair?
         close-pairs)

(import-class NSTextView NSColor NSTrackingArea NSCursor NSApplication
              NSPasteboard NSMenu NSMenuItem)

;; ---- Editing mode -----------------------------------------------------------
;; Default: JetBrains-style (standard macOS editing + IDE shortcuts).
;; Vim mode is opt-in via settings.
(define *vim-enabled* (box #f))
(define *vim-mode*    (box 'insert))
(define *pending-key* (box #f))

(define (update-cursor-style! tv mode)
  (case mode
    [(normal)
     (tellv tv setInsertionPointColor: (hex->NSColor "#ff005b"))]
    [(insert)
     (tellv tv setInsertionPointColor: (hex->NSColor "#bcbec4"))]))

;; ---- Tracking area for hover ------------------------------------------------
(define (install-tracking-area! tv)
  (define existing (tell tv trackingAreas))
  (define count (tell #:type _NSUInteger existing count))
  (for ([i (in-range count)])
    (tellv tv removeTrackingArea:
           (tell existing objectAtIndex: #:type _NSUInteger i)))
  (define area
    (tell (tell NSTrackingArea alloc)
          initWithRect: #:type _NSRect (NSMakeRect 0 0 0 0)
          options: #:type _NSUInteger (+ #x02 #x20 #x200)
          owner: tv
          userInfo: #f))
  (tellv tv addTrackingArea: area))

;; ---- Hover tooltip ----------------------------------------------------------
(define (handle-mouse-move tv event)
  (define win-point (tell #:type _NSPoint event locationInWindow))
  (define local-point
    (tell #:type _NSPoint tv convertPoint: #:type _NSPoint win-point fromView: #f))
  (define idx
    (tell #:type _NSUInteger tv
          characterIndexForInsertionAtPoint: #:type _NSPoint local-point))
  (define hover-text (mouse-over-at idx))
  (if hover-text
      (tellv tv setToolTip: (NSStr hover-text))
      (tellv tv setToolTip: #f)))

;; ---- Jump-to-definition (Cmd-click) ----------------------------------------
(define (handle-cmd-click tv event)
  (define win-point (tell #:type _NSPoint event locationInWindow))
  (define local-point
    (tell #:type _NSPoint tv convertPoint: #:type _NSPoint win-point fromView: #f))
  (define idx
    (tell #:type _NSUInteger tv
          characterIndexForInsertionAtPoint: #:type _NSPoint local-point))
  (define target (jump-target-at idx))
  (when target
    (define filename (car target))
    (define identifier (cadr target))
    (defer!
     (lambda ()
       (define current (unbox *current-file-path*))
       (cond
         [(and current (equal? (simplify-path filename)
                               (simplify-path current)))
          (move-to-definition tv identifier)]
         [else
          (editor-open-file! filename)])))))

(define (move-to-definition tv identifier)
  (define annotations (get-annotations))
  (for ([ann (in-list annotations)])
    (when (and (eq? (vector-ref ann 0)
                    'syncheck:add-definition-target/phase-level+space)
               (equal? (vector-ref ann 3) identifier))
      (define pos (vector-ref ann 1))
      (tellv tv setSelectedRange: #:type _NSRange (make-NSRange pos 0))
      (tellv tv scrollRangeToVisible: #:type _NSRange (make-NSRange pos 0)))))

;; ---- JetBrains-style shortcuts ----------------------------------------------
;; Returns #t if handled, #f to pass through to NSTextView default.

(define (get-current-line-range tv)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define sel (tell #:type _NSRange tv selectedRange))
  (tell #:type _NSRange str lineRangeForRange: #:type _NSRange sel))

(define (handle-jetbrains-key tv chars flags keycode)
  (define cmd?   (> (bitwise-and flags #x100000) 0))
  (define shift? (> (bitwise-and flags #x20000) 0))
  (define opt?   (> (bitwise-and flags #x80000) 0))
  (define ctrl?  (> (bitwise-and flags #x40000) 0))
  (cond
    ;; Cmd-D: duplicate line
    [(and cmd? (string=? chars "d"))
     (define line (get-current-line-range tv))
     (define storage (tell tv textStorage))
     (define str (tell storage string))
     (define line-text
       (nsstring->string
        (tell str substringWithRange: #:type _NSRange line)))
     (define end (+ (NSRange-location line) (NSRange-length line)))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange end 0))
     (tellv tv insertText: (NSStr line-text))
     #t]

    ;; Cmd-Shift-K: delete line
    [(and cmd? shift? (= keycode 40))
     (define line (get-current-line-range tv))
     (tellv tv setSelectedRange: #:type _NSRange line)
     (tellv tv delete: #f)
     #t]

    ;; Cmd-/: toggle line comment
    [(and cmd? (string=? chars "/"))
     (toggle-line-comment! tv)
     #t]

    ;; Opt-Up: move line up
    [(and opt? (= keycode 126))
     (move-line! tv -1)
     #t]

    ;; Opt-Down: move line down
    [(and opt? (= keycode 125))
     (move-line! tv 1)
     #t]

    ;; Cmd-L: go to line (select current line for now)
    [(and cmd? (string=? chars "l"))
     (define line (get-current-line-range tv))
     (tellv tv setSelectedRange: #:type _NSRange line)
     (tellv tv scrollRangeToVisible: #:type _NSRange line)
     #t]

    ;; Tab: complete if after a word, indent if after whitespace
    [(and (not cmd?) (not opt?) (not ctrl?) (string=? chars "\t"))
     (if (has-word-prefix? tv)
         (show-completion-popup! tv)
         (tellv tv insertText: (NSStr "  ")))
     #t]

    ;; Enter: newline with smart indent
    [(and (not cmd?) (not opt?) (not ctrl?) (string=? chars "\r"))
     (auto-indent-newline! tv)
     #t]

    ;; Auto-close brackets and quotes
    [(and (not cmd?) (not opt?) (not ctrl?)
          (member chars '("(" "[" "{" "\"")))
     (auto-close-pair! tv chars)
     #t]

    ;; Skip over closing bracket if already there
    [(and (not cmd?) (not opt?) (not ctrl?)
          (member chars '(")" "]" "}")))
     (skip-or-insert-close! tv chars)
     #t]

    ;; Backspace: delete matching pair if cursor is between them
    [(and (not cmd?) (not opt?) (not ctrl?) (= keycode 51))
     (if (delete-matching-pair? tv)
         #t
         #f)]

    ;; Cmd-Enter: eval current line/selection in REPL
    [(and cmd? (string=? chars "\r"))
     (eval-in-repl! tv)
     #t]

    ;; Cmd-F: find
    [(and cmd? (string=? chars "f"))
     (show-search-bar!)
     #t]

    ;; Cmd-G: find next
    [(and cmd? (not shift?) (string=? chars "g"))
     (search-next!)
     #t]

    ;; Cmd-Shift-G: find previous
    [(and cmd? shift? (string=? chars "G"))
     (search-prev!)
     #t]

    ;; Escape: close search bar (if visible)
    [(and (not cmd?) (not opt?) (not ctrl?) (string=? chars "\u001b"))
     (cond
       [(search-visible?) (hide-search-bar!) #t]
       [else #f])]

    ;; Cmd-[: previous tab
    [(and cmd? (string=? chars "["))
     (prev-tab!)
     #t]

    ;; Cmd-]: next tab
    [(and cmd? (string=? chars "]"))
     (next-tab!)
     #t]

    ;; Ctrl-Space: completion (keycode 49 = space bar)
    [(and ctrl? (= keycode 49))
     (show-completion-popup! tv)
     #t]

    [else #f]))

;; ---- Eval current line or selection in REPL ---------------------------------
(define (eval-in-repl! tv)
  (define sel (tell #:type _NSRange tv selectedRange))
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define text
    (if (> (NSRange-length sel) 0)
        (nsstring->string
         (tell str substringWithRange: #:type _NSRange sel))
        (let ([line (tell #:type _NSRange str
                          lineRangeForRange: #:type _NSRange sel)])
          (nsstring->string
           (tell str substringWithRange: #:type _NSRange line)))))
  (define trimmed (string-trim text))
  (when (> (string-length trimmed) 0)
    (define fn (unbox *eval-in-repl-fn*))
    (when fn (defer! (lambda () (fn trimmed))))))

;; ---- Check if cursor is after a word (for Tab completion) -------------------
(define (identifier-char? c)
  (or (char-alphabetic? c) (char-numeric? c)
      (char=? c #\-) (char=? c #\_) (char=? c #\?)
      (char=? c #\!) (char=? c #\>)))

(define (word-prefix-length tv)
  (define storage (tell tv textStorage))
  (define total (tell #:type _NSUInteger storage length))
  (define sel (tell #:type _NSRange tv selectedRange))
  (define cursor (NSRange-location sel))
  (if (or (zero? cursor) (zero? total)) 0
      (let ([src (nsstring->string (tell storage string))])
        (let loop ([i cursor] [len 0])
          (cond
            [(zero? i) len]
            [(identifier-char? (string-ref src (sub1 i)))
             (loop (sub1 i) (add1 len))]
            [else len])))))

(define (has-word-prefix? tv)
  (> (word-prefix-length tv) 0))

;; ---- Toggle line comment (;; prefix) ----------------------------------------
(define (toggle-line-comment! tv)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define line (get-current-line-range tv))
  (define loc (NSRange-location line))
  (define len (NSRange-length line))
  (define line-text
    (nsstring->string (tell str substringWithRange: #:type _NSRange line)))
  (define trimmed (string-trim line-text #:right? #f))
  (cond
    [(string-prefix? trimmed ";; ")
     (define idx (- (string-length line-text) (string-length trimmed)))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange (+ loc idx) 3))
     (tellv tv insertText: (NSStr ""))]
    [else
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange loc 0))
     (tellv tv insertText: (NSStr ";; "))]))

;; ---- Move line up/down ------------------------------------------------------
(define (move-line! tv direction)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define total (tell #:type _NSUInteger storage length))
  (define line (get-current-line-range tv))
  (define loc (NSRange-location line))
  (define len (NSRange-length line))
  (define line-text
    (nsstring->string (tell str substringWithRange: #:type _NSRange line)))
  (cond
    [(and (= direction -1) (> loc 0))
     (define prev (tell #:type _NSRange str
                        lineRangeForRange: #:type _NSRange
                        (make-NSRange (sub1 loc) 0)))
     (tellv tv setSelectedRange: #:type _NSRange line)
     (tellv tv insertText: (NSStr ""))
     (define new-loc (NSRange-location prev))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange new-loc 0))
     (tellv tv insertText: (NSStr line-text))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange new-loc 0))]
    [(and (= direction 1) (< (+ loc len) total))
     (define next (tell #:type _NSRange str
                        lineRangeForRange: #:type _NSRange
                        (make-NSRange (+ loc len) 0)))
     (define next-text
       (nsstring->string (tell str substringWithRange: #:type _NSRange next)))
     (tellv tv setSelectedRange: #:type _NSRange next)
     (tellv tv insertText: (NSStr ""))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange loc 0))
     (tellv tv insertText: (NSStr next-text))
     (define new-loc (+ loc (string-length next-text)))
     (tellv tv setSelectedRange: #:type _NSRange (make-NSRange new-loc 0))]))

;; ---- Auto-close brackets and quotes -----------------------------------------
(define close-pairs (hash "(" ")" "[" "]" "{" "}" "\"" "\""))

(define (auto-close-pair! tv open)
  (define close (hash-ref close-pairs open))
  (tellv tv insertText: (NSStr (string-append open close)))
  (tellv tv moveLeft: #f))

(define (skip-or-insert-close! tv close)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define total (tell #:type _NSUInteger storage length))
  (define sel (tell #:type _NSRange tv selectedRange))
  (define cursor (NSRange-location sel))
  (define char-after
    (if (< cursor total)
        (nsstring->string
         (tell str substringWithRange: #:type _NSRange (make-NSRange cursor 1)))
        ""))
  (if (string=? char-after close)
      (tellv tv moveRight: #f)
      (tellv tv insertText: (NSStr close))))

(define (delete-matching-pair? tv)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define total (tell #:type _NSUInteger storage length))
  (define sel (tell #:type _NSRange tv selectedRange))
  (define cursor (NSRange-location sel))
  (cond
    [(and (> cursor 0) (< cursor total))
     (define before
       (nsstring->string
        (tell str substringWithRange: #:type _NSRange (make-NSRange (sub1 cursor) 1))))
     (define after
       (nsstring->string
        (tell str substringWithRange: #:type _NSRange (make-NSRange cursor 1))))
     (define expected (hash-ref close-pairs before #f))
     (cond
       [(and expected (string=? after expected))
        (tellv tv setSelectedRange: #:type _NSRange (make-NSRange (sub1 cursor) 2))
        (tellv tv insertText: (NSStr ""))
        #t]
       [else #f])]
    [else #f]))

;; ---- Auto-indent on Enter ---------------------------------------------------
(define (auto-indent-newline! tv)
  (define storage (tell tv textStorage))
  (define str (tell storage string))
  (define total (tell #:type _NSUInteger storage length))
  (define sel (tell #:type _NSRange tv selectedRange))
  (define cursor (NSRange-location sel))
  (define line (tell #:type _NSRange str lineRangeForRange: #:type _NSRange sel))
  (define line-text
    (nsstring->string
     (tell str substringWithRange: #:type _NSRange line)))
  ;; Base indent: match current line's leading whitespace
  (define base-indent
    (let loop ([i 0])
      (cond
        [(>= i (string-length line-text)) i]
        [(char=? (string-ref line-text i) #\space) (loop (add1 i))]
        [else i])))
  ;; Extra indent if the char before cursor is an open paren
  (define char-before
    (if (> cursor 0)
        (nsstring->string
         (tell str substringWithRange: #:type _NSRange (make-NSRange (sub1 cursor) 1)))
        ""))
  (define char-after
    (if (< cursor total)
        (nsstring->string
         (tell str substringWithRange: #:type _NSRange (make-NSRange cursor 1)))
        ""))
  (define extra (if (member char-before '("(" "[" "{")) 2 0))
  (define indent (+ base-indent extra))
  ;; If cursor is between matching brackets, split them
  (cond
    [(and (> extra 0) (member char-after '(")" "]" "}")))
     (tellv tv insertText:
            (NSStr (string-append "\n"
                                  (make-string indent #\space)
                                  "\n"
                                  (make-string base-indent #\space))))
     (tellv tv moveUp: #f)
     (tellv tv moveToEndOfLine: #f)]
    [else
     (tellv tv insertText:
            (NSStr (string-append "\n" (make-string indent #\space))))]))

;; ---- Vim normal-mode key dispatch -------------------------------------------
(define (handle-normal-key tv chars cmd? ctrl? event)
  (define pending (unbox *pending-key*))
  (cond
    [(string=? chars "h") (tellv tv moveLeft: #f)]
    [(string=? chars "j") (tellv tv moveDown: #f)]
    [(string=? chars "k") (tellv tv moveUp: #f)]
    [(string=? chars "l") (tellv tv moveRight: #f)]
    [(string=? chars "w") (tellv tv moveWordForward: #f)]
    [(string=? chars "b") (tellv tv moveWordBackward: #f)]
    [(string=? chars "0") (tellv tv moveToBeginningOfLine: #f)]
    [(string=? chars "$") (tellv tv moveToEndOfLine: #f)]
    [(string=? chars "G") (tellv tv moveToEndOfDocument: #f)]
    [(and (string=? chars "g") (equal? pending "g"))
     (tellv tv moveToBeginningOfDocument: #f)
     (set-box! *pending-key* #f)]
    [(string=? chars "g") (set-box! *pending-key* "g")]
    [(string=? chars "i")
     (set-box! *vim-mode* 'insert)
     (update-cursor-style! tv 'insert)]
    [(string=? chars "a")
     (tellv tv moveRight: #f)
     (set-box! *vim-mode* 'insert)
     (update-cursor-style! tv 'insert)]
    [(string=? chars "o")
     (tellv tv moveToEndOfLine: #f)
     (tellv tv insertNewline: #f)
     (set-box! *vim-mode* 'insert)
     (update-cursor-style! tv 'insert)]
    [(string=? chars "x") (tellv tv deleteForward: #f)]
    [(and (string=? chars "d") (equal? pending "d"))
     (define line (get-current-line-range tv))
     (tellv tv setSelectedRange: #:type _NSRange line)
     (tellv tv delete: #f)
     (set-box! *pending-key* #f)]
    [(string=? chars "d") (set-box! *pending-key* "d")]
    [(string=? chars "u") (tellv tv undo: #f)]
    [else (set-box! *pending-key* #f)]))

;; ---- The subclass -----------------------------------------------------------
(define-objc-class RktEditorView NSTextView ()
  [- _void (keyDown: [_id event])
     (define chars (nsstring->string (tell event characters)))
     (define flags (tell #:type _NSUInteger event modifierFlags))
     (define cmd?  (> (bitwise-and flags #x100000) 0))
     (define ctrl? (> (bitwise-and flags #x40000) 0))
     (define keycode (tell #:type _ushort event keyCode))
     (define vim? (unbox *vim-enabled*))
     (define mode (unbox *vim-mode*))

     (cond
       ;; Completion popup — always highest priority
       [(completion-visible?)
        (cond
          [(= keycode 125) (completion-navigate! 1)]
          [(= keycode 126) (completion-navigate! -1)]
          [(or (string=? chars "\r") (string=? chars "\t"))
           (complete-selection! self)]
          [(string=? chars "\u001b")
           (hide-completion-popup!)]
          [else
           (hide-completion-popup!)
           (super-tell keyDown: event)
           (when (>= (word-prefix-length self) 2)
             (show-completion-popup! self))])]

       ;; Vim normal mode (only when vim is enabled)
       [(and vim? (eq? mode 'normal))
        (cond
          [cmd? (super-tell keyDown: event)]
          [else (handle-normal-key self chars cmd? ctrl? event)])]

       ;; Vim insert mode — Escape returns to normal
       [(and vim? (eq? mode 'insert) (string=? chars "\u001b"))
        (set-box! *vim-mode* 'normal)
        (update-cursor-style! self 'normal)]

       ;; JetBrains shortcuts (always active, both modes)
       [(handle-jetbrains-key self chars flags keycode)
        (void)]

       ;; Default NSTextView handling
       [else
        (super-tell keyDown: event)
        ;; Auto-complete: show popup after 2+ identifier chars
        (define plen (word-prefix-length self))
        (cond
          [(>= plen 2) (show-completion-popup! self)]
          [(completion-visible?) (hide-completion-popup!)])])]

  [- _void (mouseDown: [_id event])
     (define flags (tell #:type _NSUInteger event modifierFlags))
     (define cmd? (> (bitwise-and flags #x100000) 0))
     (if cmd?
         (handle-cmd-click self event)
         (super-tell mouseDown: event))]

  [- _void (mouseMoved: [_id event])
     (handle-mouse-move self event)]

  [- _void (updateTrackingAreas)
     (super-tell updateTrackingAreas)
     (install-tracking-area! self)]

  [- _id (menuForEvent: [_id event])
     (define menu (tell (tell NSMenu alloc) init))
     ;; Standard editing
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Cut") action: #:type _SEL (selector cut:)
                  keyEquivalent: (NSStr "")))
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Copy") action: #:type _SEL (selector copy:)
                  keyEquivalent: (NSStr "")))
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Paste") action: #:type _SEL (selector paste:)
                  keyEquivalent: (NSStr "")))
     (tellv menu addItem: (tell NSMenuItem separatorItem))
     ;; IDE actions
     (define goto-item
       (tell (tell NSMenuItem alloc)
             initWithTitle: (NSStr "Go to Definition")
             action: #:type _SEL (selector goToDefinition:)
             keyEquivalent: (NSStr "")))
     (tellv goto-item setTarget: editor-ctx-handler)
     (tellv menu addItem: goto-item)
     (tellv menu addItem: (tell NSMenuItem separatorItem))
     (tellv menu addItem:
            (tell (tell NSMenuItem alloc)
                  initWithTitle: (NSStr "Select All") action: #:type _SEL (selector selectAll:)
                  keyEquivalent: (NSStr "")))
     menu])

(define-objc-class RktEditorContextHandler NSObject ()
  [- _void (goToDefinition: [_id sender])
     (define tv (unbox *editor-view*))
     (when tv
       (define sel (tell #:type _NSRange tv selectedRange))
       (define pos (NSRange-location sel))
       (define target (jump-target-at pos))
       (when target
         (define filename (car target))
         (define identifier (cadr target))
         (defer!
          (lambda ()
            (define current (unbox *current-file-path*))
            (cond
              [(and current (equal? (simplify-path filename)
                                    (simplify-path current)))
               (move-to-definition tv identifier)]
              [else (editor-open-file! filename)])))))])

(define editor-ctx-handler (tell (tell RktEditorContextHandler alloc) init))

(set-text-view-class! RktEditorView)
