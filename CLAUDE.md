# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A personal Emacs configuration written in Emacs Lisp. There is no build system, test suite, or compilation step — changes take effect when Emacs loads the relevant file. 

## Loading architecture

**Entry point:** `~/.emacs` points to `main.el`, which:
1. Copies `000.init.el` → `early-init.el` if the former is newer
2. Loads platform-specific files from `darwin/` or `linux/` (in order: `000.init` → `paths` → others alphabetically)
3. Loads all `*.init.el` files in the root directory alphabetically (excluding `000.init.el`)
4. Adds `pkg/` to `load-path` so custom packages are available as libraries

**Platform detection:** `(eq system-type 'darwin)` gates macOS-specific loading. Linux equivalent lives in `linux/`.

## File roles

| File | Responsibility |
|------|---------------|
| `000.init.el` | Bootstrap — package setup, early UI, frame config. Doubles as `early-init.el`. |
| `001-theme.init.el` | Theme, modeline (mood-line), font/face config, UI toggles |
| `.gnus.el` | Gnus runtime config — group parameters, search engines, mail sending, nov cache |
| `communication.init.el` | elfeed, circe, w3m, Gnus |
| `development.init.el` | Language support: TreeSitter, LSP via eglot, Java/JS/Rust/Python/Ruby/Go/Clojure, AI tools |
| `encryption.init.el` | GPG/EPA encryption config |
| `keybindings.init.el` | Global keymap definitions and custom key prefix setup |
| `misc.init.el` | Catch-all for packages that don't fit the other categories |
| `navigation.init.el` | projectile config, display-buffer rules, window/frame navigation |
| `org.init.el` | org-mode, org-roam, task states, agenda, holidays |
| `snippets.init.el` | YASnippet configuration |
| `term.init.el` | eshell, vterm, project helpers |
| `uniquify.init.el` | Unique buffer naming |
| `whitespace.init.el` | whitespace-mode config |
| `writing.init.el` | Prose writing aids — flyspell, visual-line, etc. |

**Platform files (`darwin/`, `linux/`)**

| File | Responsibility |
|------|---------------|
| `darwin/000.init.el` | macOS-specific config (key modifiers, clipboard, system integration) |
| `darwin/browse.init.el` | macOS URL/browser handling |
| `darwin/paths.init.el` | JVM homes, Homebrew paths, Eclipse dir, Lombok jar |
| `darwin/tls.init.el` | TLS/SNI config for macOS |
| `linux/000.init.el` | Linux-specific early config |
| `linux/paths.init.el` | Linux equivalents of JVM/toolchain paths |

## Custom packages (`pkg/`)

| Package | Purpose |
|---------|---------|
| `jmi-autoframe.el` | Creates one Emacs frame per connected display; handles monitor hotplug |
| `jmi-location.el` | Fetches calendar lat/lon/location-name from ipinfo.io; `jmi/location-mode` polls on an interval |
| `jmi-startup-screen.el` | Startup screen showing org agenda, eshell, and weather |
| `jmi-suppress-bufferswitch.el` | Prevents specific functions from switching the visible buffer |

## Key conventions

- All packages are managed with `use-package`. Add new packages using that macro.
- Always use `use-package` patterns (`:hook`, `:config`, `:custom`, `:bind`) instead of raw `add-hook`, `setq`, or `global-set-key` calls.
- `darwin/paths.init.el` sets JVM homes, homebrew paths, Eclipse dir, and Lombok jar — update there for Java toolchain changes.
- Java formatting uses `conf/intellijCompatFormatter.xml` via `google-java-format` configured in `development.init.el`.
- Org task states: `TODO` → `INPROGRESS` → `BLOCKED` → `DONE`/`CANCELLED` (defined in `org.init.el`).
- Projects are expected under `~/projects/<workspace>/<project>/`

## How to test changes

Reload a single file from within Emacs:
```
M-x load-file RET /path/to/file.init.el RET
```

Evaluate a buffer or region:
```
M-x eval-buffer
C-x C-e   ; eval sexp before point
```

Check startup time impact:
```
M-x benchmark-init/show-durations-tree
```
