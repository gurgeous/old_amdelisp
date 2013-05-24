;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags

(eval-when-compile (require 'find-file-in-project))
(defun tags-search-tags-table ()
  "Walk directories, looking for a TAGS file. If we find one, visit it."
  (interactive)
  (let ((tags (locate-dominating-file (concat default-directory "gub") "TAGS")))
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


(defun uniq-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniq-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniq-region-lines (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippie/tabs

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(setq hippie-expand-ignore-buffers
      (append hippie-expand-ignore-buffers '("TAGS")))

(defun my-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

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
            (string-match "autoloads.el$" file)))
       ((file-directory-p path) (rebuild-autoloads-dir path))
       ((string-match ".el$" file)
        (message "generate-file-autoloads(%s)..." file)
        (generate-file-autoloads path))))))

(defun rebuild-autoloads ()
  (interactive)
  (require 'autoload)
  (let* ((autoloads (format "%s/autoloads.el" amd-elisp)))
    (save-excursion
      (with-current-buffer (find-file-noselect autoloads)
        (delete-region (point-min) (point-max))
        (rebuild-autoloads-dir amd-elisp)
        (save-buffer))
      (message "rebuild-autoload : %s - Done." autoloads))))

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
;; confirm that we should kill emacs, but only if we're in a window
;; and have a few files open.

(defun confirm-kill-emacs ()
  (cond
   ((and
     window-system
     (let ((files 0))
       (dolist (buf (buffer-list))
         (if (buffer-file-name buf)
             (setq files (+ files 1))))
       (message "FILES %d" files)
       (> files 5)))
    (yes-or-no-p "Really exit Emacs? "))
   (t t)))

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

(provide 'added)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow local bindings to be specified for certain modes
(setq mode-bindings-hashtable (make-hash-table))
(defun ensure-key-bindings (mode &optional bindings)
  (when (null bindings)
    (setq bindings mode)
    (setq mode '*all*))
  (puthash mode bindings mode-bindings-hashtable))

(defvar the-cc-modes '(c-mode c++-mode objc-mode csharp-mode java-mode idl-mode pike-mode)
  "List of the modes which are 'subclasses' of cc-mode")

(defun ensure-key-bindings-hook()
  (let* ((mode (intern (substring (symbol-name major-mode) 0 (- (length (symbol-name major-mode)) 5))))
         (is-c-mode (memq major-mode the-cc-modes))
         (keymaps (append (list (current-local-map)) (current-minor-mode-maps)))
         )
    (dolist (keymap keymaps)
      (if keymap (mapc (lambda (binding-name)
                         (let ((bindings (gethash binding-name mode-bindings-hashtable)))
                           (when bindings
                             (mapc (lambda (binding)
                                     (define-key keymap (car binding) (cadr binding)))
                                   bindings))))
                       (list mode (and is-c-mode 'c-modes) '*all*))
        ))))

(add-hook 'find-file-hook 'ensure-key-bindings-hook)
