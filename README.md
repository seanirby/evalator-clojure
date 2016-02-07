# EVALATOR-CLOJURE #

```evalator-clojure``` is an Emacs package that provides Clojure
support for [Evalator](https://github.com/seanirby/evalator) via
[CIDER](https://github.com/clojure-emacs/cider).

See [here](https://github.com/seanirby/evalator) for details.

## Installation ##

UPDATE WHEN ADDED TO MELPA

## Setup ##

The recommended setup uses one group of key bindings, and sets the
```evalator-config-mode-context-alist``` var such that whenever a
command is called to start an evalator session in a buffer where
```clojure-mode``` is enabled, evalator will start with
```evalator-clojure-context``` as its evaluation context.

```
(global-set-key (kbd "C-c e e") 'evalator)
(global-set-key (kbd "C-c e x") 'evalator-explicit)
(global-set-key (kbd "C-c e r") 'evalator-resume)
(global-set-key (kbd "C-c e i") 'evalator-insert-equiv-expr)

(setq evalator-config-mode-context-alist nil)
(add-to-list 'evalator-config-mode-context-alist '(clojure-mode . evalator-clojure-context))
``` 

See
[here](https://github.com/seanirby/evalator#setup-auto-detect-context)
for more information on configuring context auto detection.

If you just want to provide new key bindings such that they don't interfere with
your existing ```evalator``` key bindings then do the following:

```
(global-set-key (kbd "C-c c e") 'evalator-clojure)
(global-set-key (kbd "C-c c x") 'evalator-clojure-explicit)
```

## Usage ##

This package provides two helper commands that can each be used to
start an evalator session with a Clojure evaluation context.

`evalator-clojure' will start evalator in normal mode.

`evalator-clojure-explicit' will start evalator in explicit mode.

**A CIDER server must be running and connected to before you can use
```evalator-clojure```.** You can do this by executing
```cider-jack-in```, which is normally bound to <kbd>C-c M-j</kbd>

See the [evalator](https://github.com/seanirby/evalator) repo for more
details on using evalator.
