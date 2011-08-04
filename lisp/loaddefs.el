
;;;### (autoloads (abtags-find-include abtags-find-next-file abtags-find-file)
;;;;;;  "abtags" "../abtags.el" (18801 4854))
;;; Generated autoloads from ../abtags.el

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

;;;### (autoloads (apache-mode) "apache-mode" "../emacslib/apache-mode.el"
;;;;;;  (18801 4852))
;;; Generated autoloads from ../emacslib/apache-mode.el

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "../emacslib/ascii.el" (19755 24096))
;;; Generated autoloads from ../emacslib/ascii.el

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

;;;### (autoloads (coffee-mode) "coffee-mode" "../emacslib/coffee-mode.el"
;;;;;;  (19928 13228))
;;; Generated autoloads from ../emacslib/coffee-mode.el

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
;;;;;;  "color-theme" "../emacslib/color-theme.el" (18801 4852))
;;; Generated autoloads from ../emacslib/color-theme.el

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

;;;### (autoloads (color-theme-solarized-light color-theme-solarized-dark
;;;;;;  color-theme-solarized) "color-theme-solarized" "../emacslib/emacs-color-theme-solarized/color-theme-solarized.el"
;;;;;;  (20026 6466))
;;; Generated autoloads from ../emacslib/emacs-color-theme-solarized/color-theme-solarized.el

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

;;;### (autoloads (haml-mode) "haml-mode" "../emacslib/haml-mode.el"
;;;;;;  (19631 21426))
;;; Generated autoloads from ../emacslib/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "../emacslib/htmlize.el"
;;;;;;  (18917 63250))
;;; Generated autoloads from ../emacslib/htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2" "../emacslib/js2.el" (19755 25076))
;;; Generated autoloads from ../emacslib/js2.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "../emacslib/lua-mode.el"
;;;;;;  (19755 23758))
;;; Generated autoloads from ../emacslib/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "../emacslib/php-mode.el"
;;;;;;  (19530 2390))
;;; Generated autoloads from ../emacslib/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "../emacslib/ruby/ruby-mode.el"
;;;;;;  (19530 3684))
;;; Generated autoloads from ../emacslib/ruby/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb2x" "../emacslib/ruby/rubydb2x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb3x" "../emacslib/ruby/rubydb3x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (make-current-directory make-magic make-remake
;;;;;;  make make-doit) "make" "../make.el" (18801 4854))
;;; Generated autoloads from ../make.el

(autoload 'make-doit "make" "\
Compile in a given dir with the given command, preserving the
  existing command and directory. Kill the old compile if necessary.

\(fn COMMAND &rest DIR)" nil nil)

(autoload 'make "make" "\
Compile with make using the current values of make-directory and
  make-command.  If a prefix argument is specified the user will be
  prompted for the make command.  Otherwise, if either the directory
  or command is not specified, the user will be prompted then as
  well.

\(fn &rest PROMPT)" nil nil)

(autoload 'make-remake "make" "\
Select a directory & command, then make.

\(fn)" t nil)

(autoload 'make-magic "make" "\
Magically figure out how to compile the current file.

\(fn)" t nil)

(autoload 'make-current-directory "make" "\
Run make in the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (svn-root svn-exec svn-blame svn-log svn-info svn-resolved
;;;;;;  svn-ediff svn-diff svn-revert svn-rm svn-add) "svn" "../svn.el"
;;;;;;  (18948 49466))
;;; Generated autoloads from ../svn.el

(autoload 'svn-add "svn" "\
Add current file to svn.

\(fn)" t nil)

(autoload 'svn-rm "svn" "\
Remove current file from svn.

\(fn)" t nil)

(autoload 'svn-revert "svn" "\
Revert current file.

\(fn &rest FORCE)" t nil)

(autoload 'svn-diff "svn" "\
Show diff for current file.

\(fn)" t nil)

(autoload 'svn-ediff "svn" "\
Show ediff for current file.

\(fn)" t nil)

(autoload 'svn-resolved "svn" "\
Mark the current file as resolved.

\(fn)" t nil)

(autoload 'svn-info "svn" "\
Display the subversion info about the current file.

\(fn)" t nil)

(autoload 'svn-log "svn" "\
Show log for current file.

\(fn)" t nil)

(autoload 'svn-blame "svn" "\
Show blame for current file.

\(fn)" t nil)

(autoload 'svn-exec "svn" "\
Run svn into *SVN*, with args.

\(fn ARGS &optional SHOW)" nil nil)

(autoload 'svn-root "svn" "\
Return the topmost project in the given path

\(fn)" nil nil)

;;;***

;;;### (autoloads (abtags-find-include abtags-find-next-file abtags-find-file)
;;;;;;  "abtags" "../abtags.el" (18801 4854))
;;; Generated autoloads from ../abtags.el

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

;;;### (autoloads (apache-mode) "apache-mode" "../emacslib/apache-mode.el"
;;;;;;  (18801 4852))
;;; Generated autoloads from ../emacslib/apache-mode.el

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "../emacslib/ascii.el" (19755 24096))
;;; Generated autoloads from ../emacslib/ascii.el

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

;;;### (autoloads (coffee-mode) "coffee-mode" "../emacslib/coffee-mode.el"
;;;;;;  (19928 13228))
;;; Generated autoloads from ../emacslib/coffee-mode.el

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
;;;;;;  "color-theme" "../emacslib/color-theme.el" (18801 4852))
;;; Generated autoloads from ../emacslib/color-theme.el

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

;;;### (autoloads (haml-mode) "haml-mode" "../emacslib/haml-mode.el"
;;;;;;  (19631 21426))
;;; Generated autoloads from ../emacslib/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "../emacslib/htmlize.el"
;;;;;;  (18917 63250))
;;; Generated autoloads from ../emacslib/htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2" "../emacslib/js2.el" (19755 25076))
;;; Generated autoloads from ../emacslib/js2.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "../emacslib/lua-mode.el"
;;;;;;  (19755 23758))
;;; Generated autoloads from ../emacslib/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "../emacslib/php-mode.el"
;;;;;;  (19530 2390))
;;; Generated autoloads from ../emacslib/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "../emacslib/ruby/ruby-mode.el"
;;;;;;  (19530 3684))
;;; Generated autoloads from ../emacslib/ruby/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb2x" "../emacslib/ruby/rubydb2x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb3x" "../emacslib/ruby/rubydb3x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (make-current-directory make-magic make-remake
;;;;;;  make make-doit) "make" "../make.el" (18801 4854))
;;; Generated autoloads from ../make.el

(autoload 'make-doit "make" "\
Compile in a given dir with the given command, preserving the
  existing command and directory. Kill the old compile if necessary.

\(fn COMMAND &rest DIR)" nil nil)

(autoload 'make "make" "\
Compile with make using the current values of make-directory and
  make-command.  If a prefix argument is specified the user will be
  prompted for the make command.  Otherwise, if either the directory
  or command is not specified, the user will be prompted then as
  well.

\(fn &rest PROMPT)" nil nil)

(autoload 'make-remake "make" "\
Select a directory & command, then make.

\(fn)" t nil)

(autoload 'make-magic "make" "\
Magically figure out how to compile the current file.

\(fn)" t nil)

(autoload 'make-current-directory "make" "\
Run make in the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (svn-root svn-exec svn-blame svn-log svn-info svn-resolved
;;;;;;  svn-ediff svn-diff svn-revert svn-rm svn-add) "svn" "../svn.el"
;;;;;;  (18948 49466))
;;; Generated autoloads from ../svn.el

(autoload 'svn-add "svn" "\
Add current file to svn.

\(fn)" t nil)

(autoload 'svn-rm "svn" "\
Remove current file from svn.

\(fn)" t nil)

(autoload 'svn-revert "svn" "\
Revert current file.

\(fn &rest FORCE)" t nil)

(autoload 'svn-diff "svn" "\
Show diff for current file.

\(fn)" t nil)

(autoload 'svn-ediff "svn" "\
Show ediff for current file.

\(fn)" t nil)

(autoload 'svn-resolved "svn" "\
Mark the current file as resolved.

\(fn)" t nil)

(autoload 'svn-info "svn" "\
Display the subversion info about the current file.

\(fn)" t nil)

(autoload 'svn-log "svn" "\
Show log for current file.

\(fn)" t nil)

(autoload 'svn-blame "svn" "\
Show blame for current file.

\(fn)" t nil)

(autoload 'svn-exec "svn" "\
Run svn into *SVN*, with args.

\(fn ARGS &optional SHOW)" nil nil)

(autoload 'svn-root "svn" "\
Return the topmost project in the given path

\(fn)" nil nil)

;;;***

;;;### (autoloads (abtags-find-include abtags-find-next-file abtags-find-file)
;;;;;;  "abtags" "../abtags.el" (18801 4854))
;;; Generated autoloads from ../abtags.el

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

;;;### (autoloads (apache-mode) "apache-mode" "../emacslib/apache-mode.el"
;;;;;;  (18801 4852))
;;; Generated autoloads from ../emacslib/apache-mode.el

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "../emacslib/ascii.el" (19755 24097))
;;; Generated autoloads from ../emacslib/ascii.el

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

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "color-theme" "../emacslib/color-theme.el" (18801 4853))
;;; Generated autoloads from ../emacslib/color-theme.el

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

;;;### (autoloads (haml-mode) "haml-mode" "../emacslib/haml-mode.el"
;;;;;;  (19631 21427))
;;; Generated autoloads from ../emacslib/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "../emacslib/htmlize.el"
;;;;;;  (18917 63250))
;;; Generated autoloads from ../emacslib/htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2" "../emacslib/js2.el" (18801 4852))
;;; Generated autoloads from ../emacslib/js2.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "../emacslib/lua-mode.el"
;;;;;;  (19755 23759))
;;; Generated autoloads from ../emacslib/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "../emacslib/php-mode.el"
;;;;;;  (19530 2391))
;;; Generated autoloads from ../emacslib/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "../emacslib/ruby/ruby-mode.el"
;;;;;;  (19530 3684))
;;; Generated autoloads from ../emacslib/ruby/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb2x" "../emacslib/ruby/rubydb2x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb3x" "../emacslib/ruby/rubydb3x.el"
;;;;;;  (19147 29950))
;;; Generated autoloads from ../emacslib/ruby/rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (make-current-directory make-magic make-remake
;;;;;;  make make-doit) "make" "../make.el" (18801 4854))
;;; Generated autoloads from ../make.el

(autoload 'make-doit "make" "\
Compile in a given dir with the given command, preserving the
  existing command and directory. Kill the old compile if necessary.

\(fn COMMAND &rest DIR)" nil nil)

(autoload 'make "make" "\
Compile with make using the current values of make-directory and
  make-command.  If a prefix argument is specified the user will be
  prompted for the make command.  Otherwise, if either the directory
  or command is not specified, the user will be prompted then as
  well.

\(fn &rest PROMPT)" nil nil)

(autoload 'make-remake "make" "\
Select a directory & command, then make.

\(fn)" t nil)

(autoload 'make-magic "make" "\
Magically figure out how to compile the current file.

\(fn)" t nil)

(autoload 'make-current-directory "make" "\
Run make in the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (svn-root svn-exec svn-blame svn-log svn-info svn-resolved
;;;;;;  svn-ediff svn-diff svn-revert svn-rm svn-add) "svn" "../svn.el"
;;;;;;  (18948 49467))
;;; Generated autoloads from ../svn.el

(autoload 'svn-add "svn" "\
Add current file to svn.

\(fn)" t nil)

(autoload 'svn-rm "svn" "\
Remove current file from svn.

\(fn)" t nil)

(autoload 'svn-revert "svn" "\
Revert current file.

\(fn &rest FORCE)" t nil)

(autoload 'svn-diff "svn" "\
Show diff for current file.

\(fn)" t nil)

(autoload 'svn-ediff "svn" "\
Show ediff for current file.

\(fn)" t nil)

(autoload 'svn-resolved "svn" "\
Mark the current file as resolved.

\(fn)" t nil)

(autoload 'svn-info "svn" "\
Display the subversion info about the current file.

\(fn)" t nil)

(autoload 'svn-log "svn" "\
Show log for current file.

\(fn)" t nil)

(autoload 'svn-blame "svn" "\
Show blame for current file.

\(fn)" t nil)

(autoload 'svn-exec "svn" "\
Run svn into *SVN*, with args.

\(fn ARGS &optional SHOW)" nil nil)

(autoload 'svn-root "svn" "\
Return the topmost project in the given path

\(fn)" nil nil)

;;;***
