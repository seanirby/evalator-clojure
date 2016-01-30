# EVALATOR-CONTEXT-CIDER #

```evalator-context-cider``` is an Emacs package that provides Clojure support for [Evalator](https://github.com/seanirby/evalator) via [CIDER](https://github.com/clojure-emacs/cider).

By installing this package you can use Clojure as your evaluation context in evalator sessions.  See [here](https://github.com/seanirby/evalator) for details.

## Installation ##

UPDATE WHEN ADDED TO MELPA

## Setup ##

To use Clojure as your evaluation context you need to set the var ```evalator-context-to-use``` to ```evalator-context-cider```.

Evalator uses an Elisp evaluation context by default, but if the var ```evalator-context-to-use``` is set, then its value is used instead.  I recommend defining a hook so that this var is given a buffer-local value in buffers where ```clojure-mode``` is on.  This way, Elisp remains the default context except when you're working in a Clojure buffer.  If this is what you want then add the following to your init file:

```
(add-hook 'clojure-mode-hook' (lambda () (setq-local evalator-context-to-use evalator-context-cider))
```

If you'd like to use clojure as your evaluation context in all buffers, then add the following.
```
(setq evalator-context-to-use evalator-context-cider)

```

## Usage ##

Assuming you've setup the ```evalator-context-to-use``` var appropriately, all you need to do is run the ```evalator``` command.

**A CIDER server must be running and connected to before you can use ```evalator-context-cider```.**
You can do this by executing ```cider-jack-in```, which is normally bound to <kbd>C-c M-j</kbd>
