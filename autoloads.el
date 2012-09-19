
;;;### (autoloads (find-file-in-project) "emacslib/find-file-in-project"
;;;;;;  "emacslib/find-file-in-project.el" (20475 5769))
;;; Generated autoloads from emacslib/find-file-in-project.el

(autoload 'find-file-in-project "emacslib/find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable.

\(fn)" t nil)

;;;***

;;;### (autoloads (haml-mode) "emacslib/haml-mode" "emacslib/haml-mode.el"
;;;;;;  (20411 48606))
;;; Generated autoloads from emacslib/haml-mode.el

(autoload 'haml-mode "emacslib/haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (jade-mode) "emacslib/jade-mode" "emacslib/jade-mode.el"
;;;;;;  (20561 18774))
;;; Generated autoloads from emacslib/jade-mode.el

(autoload 'jade-mode "emacslib/jade-mode" "\
Major mode for editing jade node.js templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;***

;;;### (autoloads (stylus-mode) "emacslib/stylus-mode" "emacslib/stylus-mode.el"
;;;;;;  (20561 18783))
;;; Generated autoloads from emacslib/stylus-mode.el

(autoload 'stylus-mode "emacslib/stylus-mode" "\
Major mode for editing stylus node.js templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))

;;;***

;;;### (autoloads (sws-mode) "emacslib/sws-mode" "emacslib/sws-mode.el"
;;;;;;  (20561 18786))
;;; Generated autoloads from emacslib/sws-mode.el

(autoload 'sws-mode "emacslib/sws-mode" "\
Major mode for editing significant whitespace files

\(fn)" t nil)

;;;***
