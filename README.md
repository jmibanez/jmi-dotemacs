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

   - [`communication.init.el`](communication.init.el) contains
     config for stuff like IRC, Slack, RSS, etcetera

   - [`development.init.el`](development.init.el) contains all my
     configuration for doing software development from within Emacs

   - [`encryption.init.el`](encryption.init.el) configures encryption
     ([PGG](https://www.gnu.org/software/emacs/manual/html_mono/pgg.html))

   - [`keybindings.init.el`](keybindings.init.el) sets up some global 
     keybindings

   - [`misc.init.el`](misc.init.el) is a catch-all for all other odd
     configuration bits and bobs

   - [`navigation.init.el`](navigation.init.el) is where I configure
     stuff I use to get around Emacs

   - [`org.init.el`](org.init.el) contains my Org mode config

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

Any custom scripts and such that my config depends on (for instance
for [handling OAuth2 tokens for mail](scripts/mutt_oauth2.py)) are in
[scripts/](scripts/).

Custom ELisp packages that I use that aren't available in ELPA/MELPA
are in [pkg/](pkg/).

EShell-specific aliases and scripts are in [eshell/](eshell/).

Finally, there are also various plaform-specific files that get
loaded, if a directory for that plaform exists. Currently, I have
config for macOS ([`darwin`](darwin/)) and [Linux](linux/).
