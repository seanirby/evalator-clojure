# EVALATOR-CONTEXT-CIDER #

```evalator-context-cider``` is an Emacs package that provides Clojure support for [Evalator](https://github.com/seanirby/evalator) via [CIDER](https://github.com/clojure-emacs/cider).

By installing this package you can use Clojure as your evaluation context in evalator sessions.  See [here](https://github.com/seanirby/evalator) for details.

## INSTALLATION ##

UPDATE WHEN ADDED TO MELPA

## SETUP ##

To use ```evalator-context-cider``` as your evaluation context you need to set the var ```evalator-context-to-use``` to ```evalator-context-cider```.

This var is preset to ```evalator-context-elisp``` so evalator can be used anywhere with Elisp.  You'd probably like to keep this behavior but use Clojure instead when you're in a Clojure file.  If you want that, add the following to your init file:

```
(add-hook 'cider-mode-hook' (lambda () (setq-local evalator-context-to-use evalator-context-cider))
```

If you'd like to use clojure as your evaluation context in all buffers, then add the following.
```
(setq evalator-context-to-use evalator-context-cider)

```

## USAGE ##

Assuming you've setup the ```evalator-context-to-use``` var appropriately, all you need to do is run the ```evalator``` command.

NOTE: A CIDER server must be running and connected to before you can use ```evalator-context-cider```.  You can do this by executing ```cider-jack-in```, which is normally bound to <kbd>C-c M-j</kbd>
