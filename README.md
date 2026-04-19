# rktlens

A native macOS IDE for Racket, written in Racket.

rktlens talks directly to Cocoa/AppKit through `ffi/unsafe/objc` — it does **not** use `racket/gui`. The goal is a fast, native-feeling editor that leverages DrRacket's semantic analysis (`drracket/check-syntax`) without being tied to DrRacket's GUI.

## Features

- Native Cocoa text editor with Racket syntax highlighting
- Tab bar for multiple open files
- Project tree sidebar
- Embedded REPL pane
- Status bar
- Line numbers, search, completion
- Macro stepper
- Integrated `check-syntax` analysis

## Requirements

- macOS (Cocoa/AppKit, not portable to other platforms)
- [Racket](https://download.racket-lang.org/) 8.0 or newer (main distribution, for `drracket/check-syntax`)

No third-party Racket packages required — everything is pulled from the standard Racket distribution.

## Running from source

```sh
racket main.rkt [path]
```

If `path` is a directory, rktlens opens the first `.rkt` file it finds (preferring `main.rkt` or `info.rkt`). If omitted, it opens its own source directory.

## Building

```sh
make build      # compiles and produces the `rl` binary
make install    # installs `rl` to /usr/local/bin (override with PREFIX=...)
make clean
make uninstall
```

After `make install`:

```sh
rl ~/some/racket/project
```

## Project layout

| File | Purpose |
| --- | --- |
| `main.rkt` | Application entry point — wires everything together |
| `cocoa-ffi.rkt` | Reusable Cocoa/AppKit bindings |
| `dsl.rkt` | Declarative DSL for building the UI |
| `ide.rkt` | Application shell, menus, window management |
| `editor.rkt`, `editor-view.rkt` | Text editor core |
| `line-numbers.rkt`, `search.rkt`, `completion.rkt` | Editor features |
| `tab-bar.rkt`, `status-bar.rkt`, `project.rkt` | Chrome around the editor |
| `repl.rkt` | Embedded Racket REPL |
| `macro-stepper.rkt` | Macro expansion viewer |
| `settings.rkt` | Preferences |

## License

MIT — see [LICENSE](LICENSE).
