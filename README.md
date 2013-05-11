## Welcome to amd elisp

These are my emacs dotfiles. I began accumulating these back at school and I've been updating them (with help!) ever since. Only **emacs 24** is supported.

### Getting started

1. Clone the repo into ~/amdelisp or similar.
1. Put the following at the top of your ~/.emacs:

```lisp
(load "/Users/yourname/amdelisp/start")
```

### Optional stuff

There's a sample-dotemacs in there too. It contains stuff that I personally love but it isn't turned on by default. I recognize that some of this is a matter of taste. For example:

* color scheme (solarized)
* hippie-tab
* hungry delete
* find-file-in-project / ido
* always delete trailing whitespace
