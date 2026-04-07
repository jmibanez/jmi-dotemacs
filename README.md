My Personal Emacs Config
========================

This repo contains my personal Emacs configuration, split into
different files depending on function. If you want to use this, make
sure to replace your `.emacs` or `.emacs.el` to load `main.el`. On my
machines, I've placed this repo in `~/.emacs.init/`, so my `.emacs`
just contains:

```elisp
    (load-file "~/.emacs.init/main.el")
```

   - [`main.el`](main.el) loads everything else, so should be loaded
     in `.emacs`

   - [`000.init.el`](000.init.el) contains early configuration: stuff
     that should be configured and loaded as early as possible. It's
     the first init file that `main.el` loads

   - [`001-theme.init.el`](001-theme.init.el) is where I configure how
     I like Emacs to look, including setting the theme, and
     configuring the modeline

   - [`.gnus.el`](.gnus.el) is the Gnus runtime config: per-group
     parameters, search engine setup, mail sending configuration, and
     nov cache settings

   - [`communication.init.el`](communication.init.el) contains
     config for stuff like IRC, Slack, RSS, etcetera

   - [`development.init.el`](development.init.el) contains all my
     configuration for doing software development from within Emacs

   - [`encryption.init.el`](encryption.init.el) configures encryption
     via EPA/GPG

   - [`keybindings.init.el`](keybindings.init.el) sets up some global
     keybindings

   - [`misc.init.el`](misc.init.el) is a catch-all for all other odd
     configuration bits and bobs

   - [`navigation.init.el`](navigation.init.el) is where I configure
     stuff I use to get around Emacs

   - [`org.init.el`](org.init.el) contains my Org mode config

   - [`snippets.init.el`](snippets.init.el) configures YASnippet

   - [`term.init.el`](term.init.el) configures all my terminal-in-Emacs
     stuff, such as Eshell and VTerm

   - [`uniquify.init.el`](uniquify.init.el) is where I set up how I
     want my buffer names to be disambiguated

   - [`whitespace.init.el`](whitespace.init.el) is where I configure
     whitespace handling in files -- I want Emacs to highlight trailing
     whitespace so I can eliminate it

   - [`writing.init.el`](writing.init.el) is where I configure all my
     doc writing tools, including setting up spell checking and
     providing a custom environment for writing docs in Markdown

My custom YASnippet snippets are all in [snippets/](snippets/).

Custom scripts that the config depends on are in [`scripts/`](scripts/):

   - [`scripts/mutt_oauth2.py`](scripts/mutt_oauth2.py) — OAuth2 token
     management for mbsync and msmtp

   - [`scripts/bootstrap_notmuch.sh`](scripts/bootstrap_notmuch.sh) —
     initialises the notmuch mail index from scratch

Custom ELisp packages that I use that aren't available in ELPA/MELPA
are in [pkg/](pkg/).

EShell-specific aliases and scripts are in [eshell/](eshell/).

Finally, there are also various platform-specific files that get
loaded, if a directory for that platform exists. Currently, I have
config for macOS ([`darwin/`](darwin/)) and [Linux](linux/):

   - `000.init.el` has any platform-specific early config: key modifiers,
     clipboard, and system-level settings ([macOS](darwin/000.init.el),
     [linux](linux/000.init.el))

   - `paths.init.el` contains toolchain paths: JVM homes, Eclipse dir,
     Lombok jar, and other environment-specific locations
     ([macOS](darwin/paths.init.el), [linux](linux/paths.init.el))

   - [`darwin/browse.init.el`](darwin/browse.init.el) is where I set up
     any macOS-specific URL/browser handling knobs; currently, this only
     sets things up so browse-url opens using the platform browser
     (Safari in my case)

   - [`darwin/tls.init.el`](darwin/tls.init.el) configures TLS/SNI on
     macOS to use Homebrew-installed openssl binaries -- on macOS I want
     to avoid Emacs using gnutls, even if it's a required dependency
