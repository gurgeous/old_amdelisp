;;
;; Emacs preferences file - contains personal preferences.
;; copy this into your ~/.emacs to personalize.
;;

(setq auto-revert-interval 2
      auto-save-list-file-prefix nil
      backup-by-copying t
      backup-inhibited t
      backward-delete-char-untabify-method 'hungry
      column-number-mode t
      comint-input-ring-size 99
      completion-ignore-case t
      fill-column 60
      font-lock-maximum-decoration t
      font-lock-verbose 2048
      gc-cons-threshold 200000
      inhibit-startup-message t
      initial-scratch-message nil
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-time 1
      line-move-visual nil
      line-number-display-limit 3000000
      line-number-display-limit-width 4000
      line-number-mode t
      major-mode 'text-mode
      message-log-max 200
      mouse-wheel-mode t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      require-final-newline nil
      save-abbrevs nil
      sort-fold-case t
      tab-width 4
      tags-add-tables t
      tramp-mode nil
      truncate-partial-width-windows nil
      visible-bell t
      w32-use-full-screen-buffer nil
      x-select-enable-clipboard t)

;; confirm kill emacs
(add-hook 'kill-emacs-query-functions 'confirm-kill-emacs)

;; what's the clipboard format?
(set-clipboard-coding-system 'utf-8)

;; turn off auto-raise
(add-to-list 'default-frame-alist '(auto-raise . nil))

;; frame title
(setq frame-title-format "Emacs - %f")

;; cleanup make output
(setenv "TERM" "emacs")

;; quiet, please
(setq ring-bell-function (function (lambda ())))

;; tabs
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)

;; turn on some disabled stuff that we like
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region   'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global modes

(auto-image-file-mode t)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode 0)
(when window-system
  (global-hl-line-mode 1)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys

(global-set-key "\C-\\"       'advertised-undo)
(global-set-key "\C-c\C-c"    'comment-region)
(global-set-key "\C-c\C-u"    'uncomment-region)
(global-set-key "\C-m"        'newline-and-indent)
(global-set-key "\C-x\C-b"    'electric-buffer-list)
(global-set-key "\M-g"        'goto-line)
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [C-kp-right]  'indent-for-tab-command)
(global-set-key [C-right]     'indent-for-tab-command)

;; mini-buffer
(define-key minibuffer-local-map                     "\t" 'hippie-expand)
(define-key minibuffer-local-must-match-map          "\t" 'minibuffer-complete)
(define-key minibuffer-local-filename-completion-map "\t" 'minibuffer-complete)

;; movement
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

;; mistakes
(global-set-key "\C-xf"         'find-file)
(global-set-key "\C-x\C-f"      'find-file)
(global-set-key "\C-xs"         'save-buffer)
(global-set-key "\C-x\C-s"      'save-buffer)

;; compilation
(global-set-key [M-up]          'previous-error)
(global-set-key [M-down]        'next-error)

; turn off suspend-frame. I never, ever want this!
(global-set-key "\C-x\C-z"      nil)

; magit
(global-set-key (kbd "C-x g") 'magit-status)

; fixes for specific keys
(when (not is-win32)
  ;; http://www.cs.cmu.edu/cgi-bin/info2www?(emacs)Keyboard%20Translations
  (keyboard-translate ?\C-h ?\C-?)
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems
  (setq x-alt-keysym 'meta))
