
;;;### (autoloads (find-file-in-project) "find-file-in-project" "emacslib/find-file-in-project.el"
;;;;;;  (20475 5769))
;;; Generated autoloads from emacslib/find-file-in-project.el

(autoload 'find-file-in-project "find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable.

\(fn)" t nil)

;;;***

;;;### (autoloads (haml-mode) "haml-mode" "emacslib/haml-mode.el"
;;;;;;  (20411 48606))
;;; Generated autoloads from emacslib/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***
