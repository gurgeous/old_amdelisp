;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confirm before killing emacs?

(defvar confirm-before-kill-emacs nil)
(defvar *my-kill-query* (lambda ()
                          (if confirm-before-kill-emacs
                              (y-or-n-p "Really kill Emacs? ") t)))
(if (not (memq *my-kill-query* kill-emacs-query-functions))
    (setq kill-emacs-query-functions
          (cons *my-kill-query* kill-emacs-query-functions)))

(defun string-replace (string from to)
  (mapconcat 'identity (split-string string from) to))

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

(defun find-topmost-in-path (path name)
  "Return the topmost directory containing file NAME in the given
  PATH"
  (let ((dir (file-name-directory path)))
    (cond ((not (file-exists-p (concat dir name)))
           nil)
          ((and (> (length path) 0)
                (not (string-match "^[a-zA-Z]:$" path)))
           (or (find-topmost-in-path (parent-directory path) name)
               dir))
          (t dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cycle through buffers using Ctrl-tab
;; (from avh)

(setq switch-buf-list nil)
(setq switch-buf-timer nil)

(defun switch-buf-candidates (list)
  (if (null list)
      nil
    (let* ((buf (car list)) (name (buffer-name buf)) (file-name (buffer-file-name buf)))
      (if (or (string-match "[ *]" (substring (buffer-name (car list)) 0 1))
              (null file-name)
              (file-directory-p file-name))
          (switch-buf-candidates (cdr list))
        (cons buf (switch-buf-candidates (cdr list)))))))

(defun switch-buf-clear (val)
  (switch-to-buffer (current-buffer))
  (message "")
  (setq switch-buf-list nil)
  (setq switch-buf-timer nil))

(defun switch-buf ()
  "Switch to a previous non-scratch, non-directory buffer."
  (interactive)
  (if (timerp switch-buf-timer)
      (cancel-timer switch-buf-timer))
  (when (null switch-buf-list)
    (setq switch-buf-list (switch-buf-candidates (buffer-list))))
  (when (not (eq '() switch-buf-list))
    (setq switch-buf-list (append (cdr switch-buf-list) (list (car switch-buf-list))))
    (switch-to-buffer (car switch-buf-list) t)
    (message "Selecting buffer: %s" (buffer-name (car switch-buf-list)))
    (setq switch-buf-timer (add-timeout 1 'switch-buf-clear nil))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alignment helpers

(defun align-comments (start end)
  "Align comments"
  (let ((left 0) (end (copy-marker end)))
    ;; compute left
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (save-excursion
        (while (< (point) end)
          (search-forward ";") (setq left (max left (current-column)))
          (forward-line)))
      ;; move lines
      (save-excursion
        (while (< (point) end)
          (search-forward "//") (backward-char 2) (hungry-delete) (to-column left) (insert " ")
          (forward-line))))))

(defun align-returns (start end)
  "Align return statements"
  (interactive "r")
  (let ((left 0) (end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line))
      ;; compute left
      (save-excursion
        (while (< (point) end)
          (search-forward "return")
          (backward-word 1) (hungry-delete) (insert " ")
          (setq left (max left (current-column)))
          (forward-line)))
      ;; move lines
      (save-excursion
        (while (< (point) end)
          (search-forward "return") (backward-word 1) (to-column left)
          (forward-line))))))

(defun align-equals (start end)
  "Align equal statements"
  (interactive "r")
  (let ((left 0) (end (copy-marker end)))
    ;; compute left
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (save-excursion
        (while (< (point) end)
          (search-forward "=")
          (setq left (max left (current-column)))
          (forward-line)))
      ;; move lines
      (save-excursion
        (while (< (point) end)
          (search-forward "=") (backward-char) (hungry-delete) (to-column left)
          (forward-line))))))

(defun align-accessors (start end)
  "Align return statements"
  (interactive "r")
  (let ((left 0) (len 0) (end (copy-marker end)) cur)
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line))
      (save-excursion
        (while (< (point) end)
          (search-forward "{") (backward-char) (hungry-delete) (insert " ")
          (setq cur (current-column)
                left (max left cur))
          (search-forward "}") (backward-char) (hungry-delete) (insert " ")
          (setq len (max len (- (current-column) cur)))
          (forward-line)))
      (save-excursion
        (while (< (point) end)
          (search-forward "{") (backward-char) (to-column left)
          (search-forward "}") (backward-char) (to-column (+ left len))
          (forward-line))))))

(defun renumber (start end)
  "Align equal statements"
  (interactive "r")
  (let ((left 0) (num 1) (end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (search-forward "=")
        (forward-word 1)
        (backward-word 1)
        (kill-word 1)
        (insert (format "%d" num))
        (setq num (+ num 1))))))

(defun align (start end)
  "Align inlined braces"
  (interactive "r")
  (let ((left 0) cleft (width 0) right)
    ;; compute left
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (search-forward "{")
        (backward-char 1)
        (hungry-delete)
        (insert " ")
        (setq left (max left (current-column)))
        (forward-line 1)))
    ;; compute width
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (search-forward "{")
        (setq cleft (current-column))
        (search-forward "}")
        (backward-char 1)
        (hungry-delete)
        (insert " ")
        (setq width (max width (- (current-column) cleft)))
        (forward-line 1)))
    (setq right (+ left width 1))
    ;; move lines
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (search-forward "{")
        (backward-char 1)
        (to-column left)
        (search-forward "}")
        (backward-char 1)
        (to-column right)
        (forward-line 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colors

(defun color-invert-frame ()
  (interactive)
  (color-invert-frame-part 'foreground-color)
  (color-invert-frame-part 'background-color)
  (color-invert-frame-part 'cursor-color)
  (color-invert-face-part 'modeline t)
  (color-invert-face-part 'modeline nil)
  (color-invert-face-part 'region t)
  (color-invert-face-part 'font-lock-builtin-face)
  (color-invert-face-part 'font-lock-keyword-face)
  (color-invert-face-part 'font-lock-comment-face)
  (color-invert-face-part 'font-lock-constant-face)
  (color-invert-face-part 'font-lock-function-name-face)
  (color-invert-face-part 'font-lock-variable-name-face)  
  (color-invert-face-part 'font-lock-string-face)
  (color-invert-face-part 'font-lock-type-face))

(defun color-lookup (cstring)
  "turn a color (black or #ffffff) into a hex string"
  (apply #'format "#%02x%02x%02x"
         (mapc (lambda (x) (/ x 256))
                 (x-color-values cstring))))

(defun color-hex-to-rgb (hex)
  (string-match "^#\\([0-9a-f][0-9a-f]\\)\\([0-9a-f][0-9a-f]\\)\\([0-9a-f][0-9a-f]\\)$"
                (downcase hex))
  (list
   (string-to-number (match-string 1 hex) 16)
   (string-to-number (match-string 2 hex) 16)
   (string-to-number (match-string 3 hex) 16)))

(defun color-get-intensity (cstring)
  (let* ((rgb (color-hex-to-rgb (color-lookup cstring)))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb)))
    (/ (+ r g b) 3)))

(defun color-invert (cstring)
  (apply #'format "#%02x%02x%02x"  
         (mapc (lambda (x) (- 255 x))
                 (color-hex-to-rgb (color-lookup cstring)))))

                                        ; invert part of a face
(defun color-invert-face-part (face &optional bg-p)
  (if bg-p
      (set-face-background face (color-invert (face-background face)))
    (set-face-foreground face (color-invert (face-foreground face)))))

                                        ; invert part of a frame
(defun color-invert-frame-part (param)
  (let ((cstring (frame-parameter nil param)))
    (modify-frame-parameters (selected-frame)
                             (list (cons param (color-invert cstring))))
    (frame-set-background-mode (selected-frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers

;; Get an empty buffer with the specified name.
(defun get-empty-buffer (name)
  
  (let ((b (get-buffer-create name)))
    (save-excursion
      (set-buffer b)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max)))
    b))

(defun revert-some-buffers (&optional list)
  "For each file buffer in LIST, revert it. If it is modified,
ask first. LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           (not (string-equal name "TAGS"))
           (/= (aref name 0) ? )
           (buffer-file-name buffer)
           (if (buffer-modified-p buffer)
               (y-or-n-p
                (format "Buffer %s HAS BEEN EDITED.  Revert? "
                        name))
             t)
           (with-current-buffer buffer
             (message "Reverting file %s..." (buffer-file-name buffer))
             (revert-buffer t t)
             (message "Reverting file %s...done" (buffer-file-name buffer)))))
    (setq list (cdr list))))

(defun p4-revert-some-buffers ()
  "Finds all buffers that are not read-only but whose underlying files
are, and automatically reverts them.  The user is consulted before
reverting a buffer that is modified."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list))
          rbuffers b fname)
      (while (not (null buffers))
        (setq b (car buffers))
        (setq buffers (cdr buffers))
        (set-buffer b)
        (setq fname (buffer-file-name))
        (if (and (not buffer-read-only)
                 (not (null fname))
                 (not (file-writable-p fname)))
            (push b rbuffers)))
      (if (not (null rbuffers))
          (revert-some-buffers rbuffers)))))


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
;; java

(defun repackage ()
  "Fix the package statement for this java file."
  (interactive)
  (if (stringp (buffer-file-name))
      (let* ((full (buffer-file-name))
             (match (string-match "\\(src/java\\|classes\\)/\\(.*\\)/.*\\.java$" full))
             found
             package)
        (if (not (null match))
            (progn
              (setq package (substring full (match-beginning 2) (match-end 2)))
              (setq package (subst-char-in-string ?/ ?. package))
              (setq package (format "package %s;" package))
              (save-excursion
                (goto-char (point-min))
                ;; look for a package line. replace it.
                (while (and (re-search-forward "^package [^;]*;" nil t) (not found))
                  (setq found t)
                  (replace-match package nil nil))
                (if (not found)
                    (progn
                      ;; add it
                      (while (looking-at "//")
                        (forward-line 1))
                      (insert (format "\n%s\n\n" package))))))
          (error "Could not locate src/ or classes/ in path")))
    (error "%s: not a file." (buffer-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpp

;; these are copied from doxygen.el, with one minor change

(defcustom doxymacs-void-types
  "void"
  "String with void-kind variable types.  Extend this string if there
are typedefs of void.  Example: \"void tVOID\"."
  :type 'string
  :group 'doxymacs)

(defun doxymacs-extract-args-list (args-string)
  "Extracts the arguments from the given list (given as a string)."
  (cond
   ;; arg list is empty
   ((string-match "\\`[ \t\n]*\\'" args-string)
    nil)
   ;; argument list consists of one word
   ((string-match "\\`[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*\\'" args-string)
    ;; ... extract this word
    (let ((arg (substring args-string (match-beginning 1) (match-end 1))))
      ;; if this arg is a void type return nil
      (if (string-match (regexp-quote arg) doxymacs-void-types)
          nil
        ;; else return arg
        (list arg))))
   ;; else split the string and extact var names from args
   (t
    (doxymacs-extract-args-list-helper
     (doxymacs-save-split args-string)))))


(defun doxymacs-save-split (args-string)
  "Splits a declaration list as string and returns list of single
declarations."
  (let ((comma-pos (string-match "," args-string))
        (paren-pos (string-match "(" args-string)))
    (cond
     ;; no comma in string found
     ((null comma-pos)     (list args-string))
     ;; comma but no parenthethes: split-string is save
     ((null paren-pos)     (split-string args-string ","))
     ;; comma first then parenthesis
     ((< comma-pos paren-pos)
      (cons (substring args-string 0 comma-pos)
            (doxymacs-save-split (substring args-string (1+ comma-pos)))))
     ;; parenthesis first then comma. there must exist a closing parenthesis
     (t
      ;; cut off the (...) part
      (save-excursion
        ;; create temporary buffer
        (set-buffer (get-buffer-create "*doxymacs-scratch*"))
        (erase-buffer)
        (insert args-string)
        (goto-char (point-min))
        (search-forward "(")
        (prog1
            (let ((depth 1)
                  (exit)
                  (comma-found))
              (while (not exit)
                ;; step through buffer
                (forward-char 1)
                (cond
                 ;; end of buffer: exit
                 ((= (point) (point-max)) (setq exit t))
                 ;; decrease depth counter
                 ((looking-at ")")        (setq depth (1- depth)))
                 ;; increase depth counter
                 ((looking-at "(")        (setq depth (1+ depth)))
                 ;; comma at depth 0, thats it!
                 ((and (looking-at ",") (= 0 depth))
                  (setq exit t)
                  (setq comma-found t))))
              (if (not comma-found)
                  ;; whole string is one arg
                  (list (buffer-substring 1 (point)))
                ;; else split at comma ...
                (cons (buffer-substring 1 (point))
                      ;; and split rest of declaration list
                      (doxymacs-save-split
                       (buffer-substring (1+ (point)) (point-max))))))
          (kill-buffer (current-buffer))))))))


;; This regexp fails if the opt. parentheses
;; contain another level of parentheses.  E.g. for:
;; int f(int (*g)(int (*h)()))
(defun doxymacs-extract-args-list-helper (args-list)
  "Recursively get names of arguments."
  (if args-list
      (if (string-match
           (concat
            "\\("
            "([ \t\n]*\\*[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*)" ; (*varname)
            "\\|" ;; or

            ;; amd - include type
            "\\([a-zA-Z0-9_]+\\)[ \t\n]*[*&]?[ \t\n]*\\([a-zA-Z0-9_]+\\)" ; type [*%] name
            ;;            "\\*?[ \t\n]*\\([a-zA-Z0-9_]+\\)"            ; opt. *, varname
        
            "\\)"
            "[ \t\n]*"                  ; opt. spaces
            "\\(\\[[ \t\n]*[a-zA-Z0-9_]*[ \t\n]*\\]\\|" ; opt. array bounds
            "([^()]*)\\)?"              ; or opt. func args
            "[ \t\n]*"                  ; opt. spaces
            "\\(=[ \t\n]*[^ \t\n]+[ \t\n]*\\)?" ; optional assignment
            "[ \t\n]*\\'"               ; end
            ) (car args-list))
          (cons
           (cond
            ;; var name in: (*name)
            ((match-beginning 2)
             (substring (car args-list) (match-beginning 2) (match-end 2)))
            ;; var name in: *name

            ;; amd - build list with type
            ((match-beginning 3)
             (cons 
              (substring (car args-list) (match-beginning 3) (match-end 3))
              (substring (car args-list) (match-beginning 4) (match-end 4))))
        
            ;; no match: return complete declaration
            (t
             (car args-list)))
           (doxymacs-extract-args-list-helper (cdr args-list)))
        ;; else there is no match
        nil)))

(defun doxymacs-core-string (s)
  "Returns the argument string with leading and trailing blank
and new-line characters cut off."
  (string-match "\\`[ \t\n]*\\(.*?\\)[ \t\n]*\\'" s)
  (if (match-beginning 1)
      (substring s (match-beginning 1) (match-end 1))
    s))

(defun doxymacs-find-next-func ()
  "Returns a list describing next function declaration, or nil if not found."
  (interactive)
  (save-excursion
    (if (re-search-forward
         (concat
          ;; return type
          "\\(\\(const[ \t\n]+\\)?[a-zA-Z0-9_]+[ \t\n*&]+\\)?"

          ;; name
          "\\(\\([a-zA-Z0-9_~:<,>*&]\\|\\([ \t\n]+::[ \t\n]+\\)\\)+"
          "\\(o?perator[ \t\n]*.[^(]*\\)?\\)[ \t\n]*("
          ) nil t)

        (let* ((func (buffer-substring (match-beginning 3) (match-end 3)))
               (args (buffer-substring (point) (progn
                                                 (backward-char 1)
                                                 (forward-list)
                                                 (backward-char 1)
                                                 (point))))
               (ret (cond
                     ;; Return type specified
                     ((match-beginning 1)
                      (buffer-substring (match-beginning 1) (match-end 1)))
                     ;;Constructor/destructor
                     ((string-match
                       "^\\([a-zA-Z0-9_<,>:*&]+\\)[ \t\n]*::[ \t\n]*~?\\1$"
                       func) "void")
                     ;;Constructor in class decl.
                     ((save-match-data
                        (re-search-backward
                         (concat
                          "class[ \t\n]+" (regexp-quote func) "[ \t\n]*{")
                         nil t))
                      "void")
                     ;;Destructor in class decl.
                     ((save-match-data
                        (and (string-match "^~\\([a-zA-Z0-9_]+\\)$" func)
                             (save-match-data
                               (re-search-backward
                                (concat
                                 "class[ \t\n]+" (regexp-quote
                                                  (match-string 1 func))
                                 "[ \t\n]*{") nil t))))
                      "void")
                     ;;Default
                     (t "int"))))
          (list (cons 'func func)
                (cons 'args (doxymacs-extract-args-list args))
                (cons 'return (doxymacs-core-string ret))))
      nil)))

(defvar cpp-trace-formats
  '(("int" "%d" "%s")
    ("char" "%s" "%s")
    ("float" "%f" "%s")
    ("bool" "%s" "%s ? \"true\" : \"false\"")
    ("size_t" "%d" "%s")))

(defun cpp-trace-param-formatf (param)
  (let* ((type (car param))
         (name (cdr param))
         (fmt (assoc type cpp-trace-formats)))
    (insert (concat (if fmt (nth 1 fmt) "%p") ", "))))

(defun cpp-trace-param-name (param)
  (let* ((type (car param))
         (name (cdr param))
         (fmt (assoc type cpp-trace-formats)))
    (insert (format (concat ", " (if fmt (nth 2 fmt) "%s")) name))))

(defun cpp-trace-method ()
  "Insert a trace println for a method."
  (interactive)
  (beginning-of-line)
  (let* ((method (doxymacs-find-next-func))
         (func (cdr (assoc 'func method)))
         (args (cdr (assoc 'args method))))
    (search-forward "{")
    (newline-and-indent)
    (insert (format "fprintf(stderr, \"%s(" func))
    (mapc 'cpp-trace-param-formatf args)
    (when (> (length args) 0)
      (delete-char -2))
    (insert ")\\n\"")
    (mapc 'cpp-trace-param-name args)
    (insert ");")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clever indent

(eval-when-compile (require 'cc-mode))
(defun backward-up-list-indent ()
  (interactive) (save-excursion (backward-up-list) (c-indent-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; glimpse

(defun glimpse (words)
  (interactive "sSearch glimpse for: ")
  (let ((null-device nil))
    (grep (format "glimpse -iny \"%s\"" words))))

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
;; rebuild autoloads

(defun set-frame-name (name)
  "Set the name of the selected frame to NAME.
When called interactively, prompt for the name of the frame.
The frame name is displayed on the modeline if the terminal displays only
one frame, otherwise the name is displayed on the frame's caption bar."
  (interactive "sFrame name: ")
  (setq frame-title-format (concat name " - %f")))

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
