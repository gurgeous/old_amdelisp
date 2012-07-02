;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confirm before killing emacs?

(defvar confirm-before-kill-emacs nil)
(defvar *my-kill-query* (lambda ()
                          (if confirm-before-kill-emacs
                              (y-or-n-p "Really kill Emacs? ") t)))
(if (not (memq *my-kill-query* kill-emacs-query-functions))
    (setq kill-emacs-query-functions
          (cons *my-kill-query* kill-emacs-query-functions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file helpers

(defun parent-directory (path)
  (let ((parent (file-name-directory path)))
    (if (null parent) ""
      (substring parent 0 (1- (length parent))))))
  
(defun find-in-path (path name)
  (cond ((or (= (length path) 0)
             (string-match "^[a-zA-Z]:$" path))
         nil)
        (t
         (let ((file (concat (file-name-directory path) name)))
           (if (file-exists-p file)
               file
             (find-in-path (parent-directory path) name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags

(defun find-tag-non-interactive ()
  "Find the tag at point."
  (interactive)
  (require 'etags)
  (let* ((tag (funcall (or find-tag-default-function
                           (get major-mode 'find-tag-default-function)
                           'find-tag-default)))
         (line (if (looking-at "[A-Za-z]+.java:\\([0-9]+\\)")
                   (string-to-number (match-string 1))
                 nil)))
    (switch-to-buffer (find-tag-noselect tag))
    (when line
      (goto-line line))))

(defvar tags-stack nil)

(defun tags-stack-push (el)
  (setq tags-stack (cons el tags-stack)))

(defun tags-stack-pop ()
  (let ((el (car tags-stack)))    
    (setq tags-stack (cdr tags-stack))
    el))

(defadvice find-tag-noselect (before push-tag activate)  
  (or (ad-get-arg 1)
      (tags-stack-push (cons (current-buffer) (point)))))

(defadvice tags-search (before push-tag activate)
  (tags-stack-push (cons (current-buffer) (point))))

(defun tags-return ()
  "Pop to the spot we were at before looking up the last tag."  
  (interactive) 
  (let* ((el (tags-stack-pop))  
         (buffer (car el))
         (point  (cdr el)))
    (if buffer (switch-to-buffer buffer))
    (if point (goto-char point))))

(defun tags-search-tags-table ()
  "Walk directories, looking for a TAGS file. If we find one, visit it."
  (interactive)
  (let ((tags (find-in-path (concat default-directory "gub") "TAGS")))
    (if tags
        (progn
          (visit-tags-table tags)
          (message "Loaded %s." tags))
      (error "Could not find TAGS in current path."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some useful editing functions

(defun to-column (c &optional text)
  (interactive "nColumn: ")
  (if (null text)
      (setq text " "))
  (if (< (current-column) c)
      (while (< (current-column) c)
        (insert text))))

(defvar current-num 0)
(defun insert-next-num ()
  (interactive)
  (insert (format "%d" current-num))
  (setq current-num (+ current-num 1)))

(defun hungry-delete ()
  "Delete character and consecutive whitespace before point"  
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t")
    (if (/= (point) here)
        (delete-region (point) here)
      (delete-backward-char 1))))

(defun hungry-delete-ws ()
  "Delete consecutive whitespace before point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t\n")
    (if (/= (point) here)
        (delete-region (point) here))))

(defun hungry-delete-forward ()
  "Delete character and consecutive whitespace after point"  
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point))
      (delete-backward-char 1))))

(defun hungry-delete-forward-ws ()
  "Delete consecutive whitespace after point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabs

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(setq hippie-expand-ignore-buffers
      (append hippie-expand-ignore-buffers '("TAGS")))

(defun clever-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

(defun tab-2 ()
  (interactive)
  (save-excursion
    (setq tab-width 2)
    (untabify (point-min) (point-max))))

(defun tab-4 ()
  (interactive)
  (save-excursion
    (setq tab-width 4)
    (untabify (point-min) (point-max))))

(defun tab-8 ()
  (interactive)
  (save-excursion
    (setq tab-width 8)
    (untabify (point-min) (point-max))))

;;(defalias 'print 'ps-print-buffer-with-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting global key bindings

(defvar mode-list
  '(emacs-lisp-mode java-mode lisp-interaction-mode lisp-mode makefile-mode
                    perl-mode python-mode sgml-mode shell-mode shell-script-mode tetris-mode
                    c-mode-common text-mode fundamental-mode sql-mode sql-interactive-mode
                    generic-mode gud-mode bat-generic-mode properties-generic-mode p4-buffer-mode
                    nxml-mode markdown-mode)
  "List of all the modes that these key bindings should apply to.")

(defvar the-cc-modes '(c-mode c++-mode objc-mode csharp-mode java-mode idl-mode pike-mode)
  "List of the modes which are 'subclasses' of cc-mode")

(defun global-set-key-override (keys func &optional mode)
  (if (null mode)
      (global-set-key keys func))
  (if (null mode) 
      (global-set-key keys func)) 
  (global-set-key-override0 keys func mode))

(defun global-set-key-override0 (keys func &optional mode)
  (let* ((the-mode (if (null mode) 'global-mode mode))
         (bindings (get 'global-key-overrides the-mode))
         (binding (assoc keys bindings)))
    (if (or (null bindings) (null binding))
        (setq bindings (cons (cons keys func) bindings))
      (setcdr binding func))
    (put 'global-key-overrides the-mode bindings))
  t)

(defun global-bindings-override-hook ()
  "Function that's called for the various major modes to override bindings."
  (message (format "Applying bindings for %s" major-mode))

  ;; first map global bindings
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides 'global-mode))
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides major-mode))

  ;; check to see if the major-mode is a subclass of the cc-modes, and
  ;; if so, invoke the binding overrides defined for c-mode-common
  (when (memq major-mode the-cc-modes)
    ;;(message "Applying common bindings for %s" major-mode)
    (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
          (get 'global-key-overrides 'c-common-mode))))

;; Add our hook to all the defined hooks in 'mode-list'.
(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  'global-bindings-override-hook))
      mode-list)
(add-hook 'find-file-hooks 'global-bindings-override-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rebuild autoloads

(eval-when-compile (require 'autoload))
(defun rebuild-autoloads-dir (dir)
  (dolist (file (directory-files dir))
    (let ((path (format "%s/%s" dir file))
          (excludes '("." "..")))
      (cond
                                        ; ignore some files
       ((or (member file excludes)
            (string-match "loaddefs.el$" file)))
       ((file-directory-p path) (rebuild-autoloads-dir path))
       ((string-match ".el$" file)
        (message "generate-file-autoloads(%s)..." file)
        (generate-file-autoloads path))))))

(defun rebuild-autoloads ()
  (interactive)
  (require 'autoload)
  (let* ((source-directory AMDELISP)
         (autoloads-file
          (expand-file-name generated-autoload-file
                            (expand-file-name "lisp"
                                              source-directory))))
    (save-excursion
      (message "rebuild-autoload : %s" autoloads-file)
      (set-buffer (find-file-noselect autoloads-file))
      (rebuild-autoloads-dir source-directory)
      (message "rebuild-autoload : %s - Done." autoloads-file))))
 
(provide 'added)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nice way to increase/decrese left margin

(defun expand-region-to-whole-lines ()
  "Expand the region to make it encompass whole lines.
If the region is not active, activate the current line."
  (if (not mark-active)
      ;; Create region from current line
      (progn 
        (beginning-of-line)
        (set-mark (point))
        (end-of-line))
    ;; The mark is active, expand region
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (beginning-of-line)
      (set-mark (point))
      (goto-char end)
      (unless (bolp) (end-of-line)))))

(defun my-increase ()
  "Increase left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (save-excursion
      (expand-region-to-whole-lines)
      (increase-left-margin (region-beginning) (region-end) nil))))

(defun my-decrease ()
  "Decrease left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (save-excursion
      (expand-region-to-whole-lines)
      (decrease-left-margin (region-beginning) (region-end) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill buffers matching a regex

(defun kill-buffers (regexp)
  (interactive "sKill buffers with file names matching this regex: ")
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (when (and file-name
                 (string-match regexp file-name))
        (message "Killing %s" file-name)
        (kill-buffer buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch-to-buffer, but don't create

(defun switch-to-buffer-nocreate ()
  "Switch to a buffer but don't create one if it doesn't exist."
  (interactive)
  (switch-to-buffer (read-buffer "Switch to buffer " (other-buffer) "t")))
