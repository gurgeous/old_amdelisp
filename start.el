;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the lisp directories to the load path so we can find these
;; packages easily. First add /elisp, then subdirs.

(defvar AMDELISP nil)

(when (not AMDELISP)
  (error "You must set AMDELISP!"))

; check version
(when (< (string-to-number (substring emacs-version 0 2)) 23)
  (error "AMDELISP only works with emacs 23 or higher"))

(setq load-path (cons AMDELISP load-path))
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory AMDELISP)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))

(setq is-win32 (memq system-type '(windows-nt ms-dos ms-windows)))
(setq source-directory (if is-win32 (getenv "emacs_dir") "/usr/share/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preload stuff.

(load "added")
(load "modes")
(when window-system
  (load "window"))
(load "prefs")
(load (format "%s/lisp/loaddefs" AMDELISP))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inhibit-startup-message is reset to nil right after this file
;;; is loaded, so we have to add an after-init-hook to reset it.

(if inhibit-startup-message
    (add-hook 'after-init-hook (lambda () (setq inhibit-startup-message t))))
