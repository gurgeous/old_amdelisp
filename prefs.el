;;;
;;; Emacs preferences file - contains personal preferences.
;;; copy this into your ~/.emacs to personalize.
;;;


;; you probably don't care about these
(setq auto-revert-interval 2
      auto-save-list-file-prefix nil
      backup-by-copying t
      comint-input-ring-size 99      
      completion-ignore-case t
      default-major-mode 'text-mode
      default-tab-width 4
      display-time-format nil
      file-name-buffer-file-type-alist '(("\\.cgi$" . t))
      fill-column 60
      gc-cons-threshold 200000
      inhibit-startup-message t
      initial-scratch-message nil
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-time 1
      line-move-visual nil
      line-number-display-limit 3000000
      line-number-display-limit-width 4000
      message-log-max 200
      PC-word-delimiters "-_.="
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      save-abbrevs nil
      speedbar-track-mouse-flag nil
      track-eol nil
      truncate-partial-width-windows nil
      w32-use-full-screen-buffer nil
      x-select-enable-clipboard t)
(set-clipboard-coding-system 'utf-8)

;; cleanup make output
(setenv "TERM" "emacs")

;; you might want to customize these
(setq backup-inhibited t
      backward-delete-char-untabify-method 'hungry
      column-number-mode t
      confirm-before-kill-emacs nil
      line-number-mode t
      require-final-newline nil)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)

(put 'eval-expression 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing

;; philbo says these are not helpful
;; (when (not is-win32)
;;   (keyboard-translate ?\C-h ?\C-?))
;; (global-set-key-override "\177" 'backward-delete-char-untabify)

(global-set-key "\C-\\"     'advertised-undo)
(global-set-key "\C-c\C-c"      'comment-region)  
(global-set-key "\C-c\C-u"  'uncomment-region)
(global-set-key "\C-m"      'newline-and-indent)
(global-set-key "\C-x."     'find-tag)
(global-set-key "\C-x\C-b"      'electric-buffer-list)
(global-set-key "\M-."      'find-tag-non-interactive)
(global-set-key "\M-;"      'tags-return)
(global-set-key "\M-g"          'goto-line)
(global-set-key [C-backspace]   'backward-kill-word)
(global-set-key [C-kp-right]    'indent-for-tab-command)
(global-set-key [C-right]       'indent-for-tab-command)

;; mini-buffer
(define-key minibuffer-local-map "\t" 'hippie-expand)
(define-key minibuffer-local-must-match-map "\t" 'minibuffer-complete)
(define-key minibuffer-local-filename-completion-map "\t" 'minibuffer-complete)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; movement

(global-set-key "\M-z"          'pager-row-up)
(global-set-key "\C-z"          'pager-row-down)
(global-set-key [home]          'beginning-of-line)
(global-set-key [end]           'end-of-line)
(global-set-key [C-home]        'beginning-of-line)
(global-set-key [C-end]         'end-of-line)
(global-set-key [C-left]        'backward-word)
(global-set-key [C-up]          'previous-line)
(global-set-key [C-down]        'next-line)
(global-set-key [C-kp-up]       'previous-line)
(global-set-key [C-kp-down]     'next-line)
(global-set-key [C-kp-left]     'backward-word)
;; (global-set-key "\C-v"       'pager-page-down)
;; (global-set-key "\M-v"       'pager-page-up)
;; (global-set-key [next]       'pager-page-down)
;; (global-set-key [prior]      'pager-page-up)

;; these are turned off because some people use them to reindent a
;; line - see indent-for-tab-command above
;;
;;(global-set-key [C-right]     'forward-word)
;;(global-set-key [C-kp-right]  'forward-word)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mistakes

(global-set-key "\C-xf"     'find-file) 
(global-set-key "\C-x\C-f"  'find-file)
(global-set-key "\C-xs"     'save-buffer)
(global-set-key "\C-x\C-s"  'save-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compilation

(global-set-key [M-up]      'previous-error)
(global-set-key [M-down]    'next-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell

(global-set-key-override "\t" 'comint-dynamic-complete 'shell-mode)
(global-set-key-override "\C-c\C-c" 'comint-interrupt-subjob 'shell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu
(when window-system
  (global-set-key [C-down-mouse-3] 'imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window specific settings.

(setq ring-bell-function (function (lambda ())))

(defvar my-font nil)

(when window-system
  (blink-cursor-mode 1)
  (set-scroll-bar-mode nil)
  (setq mouse-wheel-mode t
        visible-bell t)
  (tool-bar-mode 0)
  (transient-mark-mode 0)

  (setq window-position 'center)
  (setq window-columns 100)
  (setq window-fudge '(0 0 0 0))
  (if is-win32 (setq window-fudge '(0 0 0 55)))
  (if is-win32 (setq my-font (window-build-font "Courier New" 9)))

  (window-set-frame-default 'auto-raise nil)
  (window-set-frame-default 'cursor-type 'box)
  (window-set-frame-default 'scroll-bar-width 12)

  ;; frame title
  (setq frame-title-format
        (concat "Emacs@"
                (if (string-match "^\\([^.]+\\)\..+" system-name)
                    (match-string 1 system-name)
                  system-name)
                " - %f")))

(when (or window-system (not is-win32))
  (setq font-lock-verbose 2048)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; amd's color theme - turned off by default

(defun color-theme-amd ()
  (color-theme-install
   '(color-theme-amd
     (;(background-color . "black")
      (foreground-color . "white")
      (cursor-color     . "yellow")
      (background-mode  . dark))
     
     (default      ((t (nil))))
     (fringe       ((t (                    :background "grey20"))))
     (modeline     ((t (:foreground "white" :background "darkslateblue"))))
     (region       ((t (                    :background "midnight blue"))))
     (highlight    ((t (                    :background "#13385b"))))
     
     (minibuffer-prompt            ((t (:foreground "gold" :bold t))))
     
     (font-lock-builtin-face       ((t (:foreground "cornflower blue"))))
     (font-lock-comment-face       ((t (:foreground "green"))))
     (font-lock-doc-face           ((t (:foreground "green"))))
     (font-lock-constant-face      ((t (:foreground "gold"))))
     (font-lock-function-name-face ((t (:foreground "goldenrod" :bold t))))
     (font-lock-keyword-face       ((t (:foreground "DeepSkyBlue1"))))
     (font-lock-string-face        ((t (:foreground "red"))))
     (font-lock-type-face          ((t (:foreground "CadetBlue1" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
     (font-lock-warning-face       ((t (:foreground "Pink"))))

     ))
  (set-face-foreground 'mode-line "white")
  (set-face-background 'mode-line "darkslateblue"))

(defun color-theme-amd-win32 ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       nil
       nil
       (font-lock-keyword-face       ((t (:foreground "cornflower blue"))))
       (font-lock-string-face        ((t (:foreground "tomato"))))
       (font-lock-warning-face       ((t (:foreground "cornflower blue"))))
       ))))

(defun color-theme-amd-linux ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       ((background-color . "black"))
       nil
       (font-lock-string-face        ((t (:foreground "tomato"))))
       ))))

(defun color-theme-amd-linux-nw ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       nil
       nil
       (font-lock-function-name-face ((t (:bold nil))))
       (font-lock-type-face          ((t (:foreground "cyan" :bold nil))))
       ))))

(defun turn-on-color-theme-amd ()
  "Turn on amd's colors."
  (interactive)
  (when (or window-system (not is-win32))
    (require 'color-theme)
    (cond
     (is-win32      (color-theme-amd-win32))
     (window-system (color-theme-amd-linux))
     (t             (color-theme-amd-linux-nw)))))

(defun color-theme-solarized-nw ()
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-solarized-dark
       ((background-color . "black"))))))
