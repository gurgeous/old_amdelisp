;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the lisp directories to the load path so we can find these
;; packages easily. First add /elisp, then subdirs.

(defvar AMDELISP nil)

(when (not AMDELISP)
  (error "You must set AMDELISP!"))

; check version
(when (< (string-to-number (substring emacs-version 0 2)) 24)
  (error "AMDELISP only works with emacs 24 or higher"))

(setq load-path (cons AMDELISP load-path))
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory AMDELISP)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))

(setq is-win32 (memq system-type '(windows-nt ms-dos ms-windows)))
;(setq source-directory (if is-win32 (getenv "emacs_dir") "/usr/share/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(package-initialize)

(let ((package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
      (packages
       '(ascii coffee-mode csharp-mode css-mode find-file-in-project go-mode
               guess-offset js2-mode lua-mode markdown-mode pager
               php-mode rainbow-mode ruby-mode solarized-theme
               volatile-highlights yaml-mode yari))
      (install nil))
  ;; do we have to install anything?
  (dolist (i packages)
    (if (not (package-installed-p i))
        (setf install t)))
  (when install
    (message "Refreshing package database...")
    (package-refresh-contents)
    (dolist (i packages)
      (when (not (package-installed-p i))
        (message "Installing package %s" i)
        (package-install i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preload stuff.

(load "added")
(load "modes")
(when window-system
  (load "window"))
(load "prefs")
(load "patches")
(load "autoloads")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inhibit-startup-message is reset to nil right after this file
;;; is loaded, so we have to add an after-init-hook to reset it.

(if inhibit-startup-message
    (add-hook 'after-init-hook (lambda () (setq inhibit-startup-message t))))
