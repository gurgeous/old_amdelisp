
;;;### (autoloads (abtags-find-include abtags-find-next-file abtags-find-file)
;;;;;;  "abtags" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/abtags.el"
;;;;;;  (18801 4854))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/abtags.el

(autoload 'abtags-find-file "abtags" "\
Prompts the user to select a filename from among those in the tags file.
   Visits the selected file.

\(fn)" t nil)

(autoload 'abtags-find-next-file "abtags" "\
Prompts the user to select a filename from among those in the tags file.
   Visits the selected file.

\(fn)" t nil)

(autoload 'abtags-find-include "abtags" "\
Follow a #include statement.

\(fn)" t nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/ascii.el"
;;;;;;  (19755 24096))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/ascii.el

(autoload 'ascii-customize "ascii" "\
Customize ASCII options.

\(fn)" t nil)

(autoload 'ascii-display "ascii" "\
Toggle ASCII code display.

If ARG is null, toggle ASCII code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display.

\(fn &optional ARG)" t nil)

(autoload 'ascii-on "ascii" "\
Turn on ASCII code display.

\(fn)" t nil)

(autoload 'ascii-off "ascii" "\
Turn off ASCII code display.

\(fn)" t nil)

;;;***

;;;### (autoloads (coffee-mode) "coffee-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/coffee-mode.el"
;;;;;;  (19928 13228))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript...

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;;;***

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "color-theme" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/color-theme.el"
;;;;;;  (18801 4852))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/color-theme.el

(autoload 'color-theme-select "color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

(autoload 'color-theme-describe "color-theme" "\
Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'.

\(fn)" t nil)

(autoload 'color-theme-install-at-mouse "color-theme" "\
Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called.

\(fn EVENT)" t nil)

(autoload 'color-theme-install-at-point-for-current-frame "color-theme" "\
Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'.

\(fn)" t nil)

(autoload 'color-theme-print "color-theme" "\
Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    (require 'color-theme)
    (defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      (interactive)
      (color-theme-install
       '(...
	 ...
	 ...)))
    (my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    (require 'color-theme)
    (color-theme-gnome2)

\(fn &optional BUF)" t nil)

(autoload 'color-theme-analyze-defun "color-theme" "\
Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author.

\(fn)" t nil)

(autoload 'color-theme-make-snapshot "color-theme" "\
Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment.

\(fn)" nil nil)

(autoload 'color-theme-compare "color-theme" "\
Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference.

\(fn THEME-A THEME-B)" t nil)

(autoload 'color-theme-install "color-theme" "\
Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

\(fn THEME)" nil nil)

(autoload 'color-theme-submit "color-theme" "\
Submit your color-theme to the maintainer.

\(fn)" t nil)

(autoload 'color-theme-initialize "color-theme" "\
Initialize the color theme package by loading color-theme-libraries.

\(fn)" t nil)

;;;***

;;;### (autoloads (csharp-mode csharp-cmd-line-limit csharp-msbuild-tool
;;;;;;  csharp-make-tool csharp-want-imenu csharp-want-yasnippet-fixup
;;;;;;  csharp-want-flymake-fixup csharp-mode-hook) "csharp-mode-0.8.5"
;;;;;;  "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/csharp-mode-0.8.5.el"
;;;;;;  (20163 2893))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/csharp-mode-0.8.5.el

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(custom-autoload 'csharp-mode-hook "csharp-mode-0.8.5" t)

(defvar csharp-want-flymake-fixup t "\
*Whether to enable the builtin C# support for flymake. This is meaningful
only if flymake is loaded.")

(custom-autoload 'csharp-want-flymake-fixup "csharp-mode-0.8.5" t)

(defvar csharp-want-yasnippet-fixup t "\
*Whether to enable the builtin C# support for yasnippet. This is meaningful
only if flymake is loaded.")

(custom-autoload 'csharp-want-yasnippet-fixup "csharp-mode-0.8.5" t)

(defvar csharp-want-imenu t "\
*Whether to generate a buffer index via imenu for C# buffers.")

(custom-autoload 'csharp-want-imenu "csharp-mode-0.8.5" t)

(defvar csharp-make-tool "nmake.exe" "\
*The make tool to use. Defaults to nmake, found on path. Specify
a full path or alternative program name, to tell csharp-mode to use
a different make tool in compile commands.

See also, `csharp-msbuild-tool'.

")

(custom-autoload 'csharp-make-tool "csharp-mode-0.8.5" t)

(defvar csharp-msbuild-tool "msbuild.exe" "\
*The tool to use to build .csproj files. Defaults to msbuild, found on
path. Specify a full path or alternative program name, to tell csharp-mode
to use a different make tool in compile commands.

See also, `csharp-make-tool'.

")

(custom-autoload 'csharp-msbuild-tool "csharp-mode-0.8.5" t)

(defvar csharp-cmd-line-limit 28 "\
The number of lines at the top of the file to look in, to find
the command that csharp-mode will use to compile the current
buffer, or the command \"stub\" that csharp-mode will use to
check the syntax of the current buffer via flymake.

If the value of this variable is zero, then csharp-mode looks
everywhere in the file.  If the value is positive, then only in
the first N lines. If negative, then only in the final N lines.

The line should appear in a comment inside the C# buffer.


Compile
--------

In the case of compile, the compile command must be prefixed with
\"compile:\".  For example,

 // compile: csc.exe /r:Hallo.dll Arfie.cs


This command will be suggested as the compile command when the
user invokes `compile' for the first time.


Flymake
--------

In the case of flymake, the command \"stub\" string must be
prefixed with \"flymake:\".  For example,

 // flymake: DOTNETDIRcsc.exe /target:netmodule /r:foo.dll @@FILE@@

In the case of flymake, the string should NOT include the name of
the file for the buffer being checked. Instead, use the token
@@FILE@@ .  csharp-mode will replace this token with the name of
the source file to compile, before passing the command to flymake
to run it.

If for some reason the command is invalid or illegal, flymake
will report an error and disable itself.

It might be handy to run fxcop, for example, via flymake.

 // flymake: fxcopcmd.exe /c  /f:MyLibrary.dll



In all cases
------------

Be sure to specify the proper path for your csc.exe, whatever
version that might be, or no path if you want to use the system
PATH search.

If the buffer depends on external libraries, then you will want
to include /R arguments to that csc.exe command.

To be clear, this variable sets the number of lines to search for
the command.  This cariable is an integer.

If the marker string (either \"compile:\" or \"flymake:\"
is present in the given set of lines, csharp-mode will take
anything after the marker string as the command to run.

")

(custom-autoload 'csharp-cmd-line-limit "csharp-mode-0.8.5" t)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(autoload 'csharp-mode "csharp-mode-0.8.5" "\
Major mode for editing C# code. This mode is derived from CC Mode to
support C#.

Normally, you'd want to autoload this mode by setting `auto-mode-alist' with
an entry for csharp, in your .emacs file:

   (autoload 'csharp-mode \"csharp-mode\" \"Major mode for editing C# code.\" t)
   (setq auto-mode-alist
      (append '((\"\\.cs$\" . csharp-mode)) auto-mode-alist))

The mode provides fontification and indent for C# syntax, as well
as some other handy features.

At mode startup, there are two interesting hooks that run:
`c-mode-common-hook' is run with no args, then `csharp-mode-hook' is run after
that, also with no args.

To run your own logic after csharp-mode starts, do this:

  (defun my-csharp-mode-fn ()
    \"my function that runs when csharp-mode is initialized for a buffer.\"
    (turn-on-font-lock)
    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
    (setq indent-tabs-mode nil) ;; tabs are evil
    (flymake-mode 1)
    (yas/minor-mode-on)
    (require 'rfringe)  ;; handy for flymake
    (require 'flymake-cursor) ;; also handy for flymake
    ....your own code here...
  )
  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


The function above is just a suggestion.


Compile integration:
========================

csharp-mode binds the function `csharp-invoke-compile-interactively' to
\"\" .  This function attempts to intellgently guess the format of the
compile command to use for a buffer.  It looks in the comments at the head of
the buffer for a line that begins with compile: .  For exammple:

  // compile: csc.exe /t:library /r:Mylib.dll Foo.cs

If csharp-mode finds a line like this, it will suggest the text that follows
as the compilation command when running `compile' for the first time.  If such
a line is not found, csharp-mode falls back to a msbuild or nmake command.
See the documentation on `csharp-cmd-line-limit' for further information. If
you don't want this magic, then you can just run `compile' directly, rather
than `csharp-invoke-compile-interactively' .

This mode will also automatically add a symbol and regexp to the
`compilation-error-regexp-alist' and`compilation-error-regexp-alist-alist'
respectively, for Csc.exe error and warning messages. If you invoke `compile',
then `next-error' should work properly for error messages produced by csc.exe.


Flymake Integraiton
========================

You can use flymake with csharp mode to automatically check the syntax of your
csharp code, and highlight errors.  To do so, add a comment line like this to
each .cs file that you use flymake with:

   //  flymake: csc.exe /t:module /R:Foo.dll @@FILE@@

csharp-mode replaces special tokens in the command with different values:

  @@ORIG@@ - gets replaced with the original filename
  @@FILE@@ - gets replaced with the name of the temporary file
      created by flymake. This is usually what you want in place of the
      name of the file to be compiled.

See the documentation on `csharp-cmd-line-limit' for further information.

You may also want to run a syntax checker, like fxcop:

   //  flymake: fxcopcmd.exe /c /F:MyLibrary.dll

In this case you don't need either of the tokens described above.

If the module has no external dependencies, then you need not specify any
flymake command at all. csharp-mode will implicitly act as if you had
specified the command:

     // flymake: csc.exe /t:module /nologo @@FILE@@

It looks for the EXE on the path.  You can specify a full path if you like.


YASnippet and IMenu Integraiton
===============================

Check the menubar for menu entries for YASnippet and Imenu; the latter
is labelled \"Index\".

The Imenu index gets computed when the file is .cs first opened and loaded.
This may take a moment or two.  If you don't like this delay and don't
use imenu, you can turn this off with the variable `csharp-want-imenu'.



Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (css-mode) "css-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/css-mode.el"
;;;;;;  (20130 15893))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/css-mode.el

(autoload 'css-mode "css-mode" "\
Major mode to edit Cascading Style Sheets.

\(fn)" t nil)

;;;***

;;;### (autoloads (color-theme-solarized-light color-theme-solarized-dark
;;;;;;  color-theme-solarized) "color-theme-solarized" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/emacs-color-theme-solarized/color-theme-solarized.el"
;;;;;;  (20026 6466))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/emacs-color-theme-solarized/color-theme-solarized.el

(autoload 'color-theme-solarized "color-theme-solarized" "\
Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.

\(fn MODE)" t nil)

(autoload 'color-theme-solarized-dark "color-theme-solarized" "\
Not documented

\(fn)" t nil)

(autoload 'color-theme-solarized-light "color-theme-solarized" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (haml-mode) "haml-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/haml-mode.el"
;;;;;;  (20360 30858))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (js2-mode) "js2-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/js2-mode.el"
;;;;;;  (20376 20302))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/js2-mode.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/lua-mode.el"
;;;;;;  (19755 23758))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/php-mode.el"
;;;;;;  (19530 2390))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/ruby/ruby-mode.el"
;;;;;;  (20262 2475))
;;; Generated autoloads from ../../../../../../mnt/d2/home/amd/Dropbox/work/elisp/emacslib/ruby/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable `ruby-indent-level' controls the amount of indentation.

\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.rb\\'") 'ruby-mode))

(dolist (name (list "ruby" "rbx" "jruby" "ruby1.9" "ruby1.8")) (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'ruby-mode)))

;;;***
