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

   - [`001-custom.init.el`](001-custom.init.el) is where I dump
     everything that is set via `customize` -- at least temporarily. I
     move stuff from this file to their proper place. (I should
     probably move the `custom-set-faces` stuff to
     `001-theme.init.el`)

   - [`001-theme.init.el`](001-theme.init.el) is where I configure how
     I like Emacs to look, including setting the theme, and
     configuring the modeline

   - [`development.init.el`](development.init.el) contains all my
     configuration for doing software development from within Emacs

   - [`navigation.init.el`](navigation.init.el) is where I configure
     stuff I use to get around Emacs, namely
     [Helm](https://emacs-helm.github.io/helm/)

   - [`misc.init.el`](misc.init.el) is a catch-all for all other odd
     configuration bits and bobs

   - [`uniquify.init.el`](uniquify.init.el) is where I set up how I
     want my buffer names to be disambiguated

   - [`whitespace.init.el`](whitespace.init.el) is where I configure
     whitespace handling in files -- I want Emacs to highlight trailing
     whitespace so I can eliminate it

There are also various plaform-specific files that get loaded, if a
directory for that plaform exists. Currently, I have config for macOS
([`darwin`](darwin/)) and [Linux](linux/).
