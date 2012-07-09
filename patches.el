;;
;; Fix the auto-revert-handler so that if the system time is very
;; close to the new modified time for a file, skip it on this
;; iteration. This should fix race conditions when a file is changed
;; multiple times within the same second.
;;
;; (from jp)
;; 

(eval-when-compile (require 'autorevert))
(eval-after-load 'autorevert
  '(progn
     (defun file-change-too-close-for-comfort ()
       (let* ((file-time-raw (nth 5 (file-attributes (buffer-file-name))))
              (file-time (+ (lsh (nth 0 file-time-raw) 16) (nth 1 file-time-raw)))
              (current-time (+ (lsh (nth 0 (current-time)) 16) (nth 1 (current-time)))))
         (and (eq current-time file-time)
              (message "%s: postpone revert" (buffer-name))
              t)))

     (defun auto-revert-handler ()
       "Revert current buffer, if appropriate.
This is an internal function used by Auto-Revert Mode."
       (when (or auto-revert-tail-mode (not (buffer-modified-p)))
         (let* ((buffer (current-buffer)) size
                (revert
                 (or (and buffer-file-name
                          (file-readable-p buffer-file-name)
                          (if auto-revert-tail-mode
                              ;; Tramp caches the file attributes.  Setting
                              ;; `tramp-cache-inhibit' forces Tramp to
                              ;; reread the values.
                              (let ((tramp-cache-inhibit-cache t))
                                (/= auto-revert-tail-pos
                                    (setq size
                                          (nth 7 (file-attributes
                                                  buffer-file-name)))))
                            (and (not (file-remote-p buffer-file-name))
                                 (not (verify-visited-file-modtime buffer))
                                 (not (file-change-too-close-for-comfort)))))
                     (and (or auto-revert-mode
                              global-auto-revert-non-file-buffers)
                          revert-buffer-function
                          (boundp 'buffer-stale-function)
                          (functionp buffer-stale-function)
                          (funcall buffer-stale-function t))))
                eob eoblist)
           (when revert
             (when (and auto-revert-verbose
                        (not (eq revert 'fast)))
               (message "Reverting buffer `%s'." (buffer-name)))
             ;; If point (or a window point) is at the end of the buffer,
             ;; we want to keep it at the end after reverting.  This allows
             ;; to tail a file.
             (when buffer-file-name
               (setq eob (eobp))
               (walk-windows
                #'(lambda (window)
                    (and (eq (window-buffer window) buffer)
                         (= (window-point window) (point-max))
                         (push window eoblist)))
                'no-mini t))
             (if auto-revert-tail-mode
                 (auto-revert-tail-handler size)
               ;; Bind buffer-read-only in case user has done C-x C-q,
               ;; so as not to forget that.  This gives undesirable results
               ;; when the file's mode changes, but that is less common.
               (let ((buffer-read-only buffer-read-only))
                 (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)))
             (when buffer-file-name
               (when eob (goto-char (point-max)))
               (dolist (window eoblist)
                 (set-window-point window (point-max)))))
           ;; `preserve-modes' avoids changing the (minor) modes.  But we
           ;; do want to reset the mode for VC, so we do it manually.
           (when (or revert auto-revert-check-vc-info)
             (vc-find-file-hook)))))
     ))

;;
;; Modify ruby-mode to indent this way:
;;
;; def gub
;;   x ||= begin
;;     big_expensive_function
;;   end
;; end
;;
;; Instead of this way:
;;
;; def gub
;;   x ||= begin
;;           big_expensive_function
;;         end
;; end
;;

(eval-when-compile (require 'ruby-mode))
(eval-after-load 'ruby-mode
  '(defun ruby-calculate-indent (&optional parse-start)
     "Returns the proper indentation level of the current line."
     ;; TODO: Document body
     (save-excursion
       (beginning-of-line)
       (let ((indent-point (point))
             (case-fold-search nil)
             state bol eol begin op-end
             (paren (progn (skip-syntax-forward " ")
                           (and (char-after) (matching-paren (char-after)))))
             (indent 0))
         (if parse-start
             (goto-char parse-start)
           (ruby-beginning-of-indent)
           (setq parse-start (point)))
         (back-to-indentation)
         (setq indent (current-column))
         (setq state (ruby-parse-region parse-start indent-point))
         (cond
          ((nth 0 state)                   ; within string
           (setq indent nil))              ;  do nothing
          ((car (nth 1 state))             ; in paren
           (goto-char (setq begin (cdr (nth 1 state))))
           (let ((deep (ruby-deep-indent-paren-p (car (nth 1 state)))))
             (if deep
                 (cond ((and (eq deep t) (eq (car (nth 1 state)) paren))
                        (skip-syntax-backward " ")
                        (setq indent (1- (current-column))))
                       ((let ((s (ruby-parse-region (point) indent-point)))
                          (and (nth 2 s) (> (nth 2 s) 0)
                               (or (goto-char (cdr (nth 1 s))) t)))
                        (forward-word -1)
                        (setq indent (ruby-indent-size (current-column) (nth 2 state))))
                       (t
                        (setq indent (current-column))
                        (cond ((eq deep 'space))
                              (paren (setq indent (1- indent)))
                              (t (setq indent (ruby-indent-size (1- indent) 1))))))
               (if (nth 3 state) (goto-char (nth 3 state))
                 (goto-char parse-start) (back-to-indentation))
               (setq indent (ruby-indent-size (current-column) (nth 2 state))))
             (and (eq (car (nth 1 state)) paren)
                  (ruby-deep-indent-paren-p (matching-paren paren))
                  (search-backward (char-to-string paren))
                  (setq indent (current-column)))))
          ((and (nth 2 state) (> (nth 2 state) 0)) ; in nest
           (if (null (cdr (nth 1 state)))
               (error "invalid nest"))
           (goto-char (cdr (nth 1 state)))
           (forward-word -1)               ; skip back a keyword
           (setq begin (point))
           (cond
            ((looking-at "do\\>[^_]")      ; iter block is a special case
             (if (nth 3 state) (goto-char (nth 3 state))
               (goto-char parse-start) (back-to-indentation))
             (setq indent (ruby-indent-size (current-column) (nth 2 state))))
            (t
                                        ; amd: indent current+2 instead of col+2
                                        ;(setq indent (+ (current-column) ruby-indent-level)))))
             (setq indent (+ (ruby-current-indentation) ruby-indent-level)))))
          
          ((and (nth 2 state) (< (nth 2 state) 0)) ; in negative nest
           (setq indent (ruby-indent-size (current-column) (nth 2 state)))))
         (when indent
           (goto-char indent-point)
           (end-of-line)
           (setq eol (point))
           (beginning-of-line)
           (cond
            ((and (not (ruby-deep-indent-paren-p paren))
                  (re-search-forward ruby-negative eol t))
             (and (not (eq ?_ (char-after (match-end 0))))
                  (setq indent (- indent ruby-indent-level))))
            ((and
              (save-excursion
                (beginning-of-line)
                (not (bobp)))
              (or (ruby-deep-indent-paren-p t)
                  (null (car (nth 1 state)))))
             ;; goto beginning of non-empty no-comment line
             (let (end done)
               (while (not done)
                 (skip-chars-backward " \t\n")
                 (setq end (point))
                 (beginning-of-line)
                 (if (re-search-forward "^\\s *#" end t)
                     (beginning-of-line)
                   (setq done t))))
             (setq bol (point))
             (end-of-line)
             ;; skip the comment at the end
             (skip-chars-backward " \t")
             (let (end (pos (point)))
               (beginning-of-line)
               (while (and (re-search-forward "#" pos t)
                           (setq end (1- (point)))
                           (or (ruby-special-char-p end)
                               (and (setq state (ruby-parse-region parse-start end))
                                    (nth 0 state))))
                 (setq end nil))
               (goto-char (or end pos))
               (skip-chars-backward " \t")
               (setq begin (if (and end (nth 0 state)) pos (cdr (nth 1 state))))
               (setq state (ruby-parse-region parse-start (point))))
             (or (bobp) (forward-char -1))
             (and
              (or (and (looking-at ruby-symbol-re)
                       (skip-chars-backward ruby-symbol-chars)
                       (looking-at (concat "\\<\\(" ruby-block-hanging-re "\\)\\>"))
                       (not (eq (point) (nth 3 state)))
                       (save-excursion
                         (goto-char (match-end 0))
                         (not (looking-at "[a-z_]"))))
                  (and (looking-at ruby-operator-re)
                       (not (ruby-special-char-p))
                       ;; operator at the end of line
                       (let ((c (char-after (point))))
                         (and
                          ;;                     (or (null begin)
                          ;;                         (save-excursion
                          ;;                           (goto-char begin)
                          ;;                           (skip-chars-forward " \t")
                          ;;                           (not (or (eolp) (looking-at "#")
                          ;;                                    (and (eq (car (nth 1 state)) ?{)
                          ;;                                         (looking-at "|"))))))
                          (or (not (eq ?/ c))
                              (null (nth 0 (ruby-parse-region (or begin parse-start) (point)))))
                          (or (not (eq ?| (char-after (point))))
                              (save-excursion
                                (or (eolp) (forward-char -1))
                                (cond
                                 ((search-backward "|" nil t)
                                  (skip-chars-backward " \t\n")
                                  (and (not (eolp))
                                       (progn
                                         (forward-char -1)
                                         (not (looking-at "{")))
                                       (progn
                                         (forward-word -1)
                                         (not (looking-at "do\\>[^_]")))))
                                 (t t))))
                          (not (eq ?, c))
                          (setq op-end t)))))
              (setq indent
                    (cond
                     ((and
                       (null op-end)
                       (not (looking-at (concat "\\<\\(" ruby-block-hanging-re "\\)\\>")))
                       (eq (ruby-deep-indent-paren-p t) 'space)
                       (not (bobp)))
                      (widen)
                      (goto-char (or begin parse-start))
                      (skip-syntax-forward " ")
                      (current-column))
                     ((car (nth 1 state)) indent)
                     (t
                      (+ indent ruby-indent-level))))))))
         (goto-char indent-point)
         (beginning-of-line)
         (skip-syntax-forward " ")
         (if (looking-at "\\.[^.]")
             (+ indent ruby-indent-level)
           indent)))))


;;
;; Change find-file-in-project to use the previous project root if
;; none is found.
;; 

(eval-when-compile (require 'find-file-in-project))
(eval-after-load 'find-file-in-project
  '(progn
     (defvar ffip-project-root-previous nil)
     (defalias 'ffip-project-root-original (symbol-function 'ffip-project-root))
     (defun ffip-project-root ()
       (let ((project-root (ffip-project-root-original)))
         (if project-root
             (setq ffip-project-root-previous project-root)
           (setq project-root ffip-project-root-previous))
         project-root))
     ))

;;
;; Sadly, the old version of haml-mode requires this but ruby-mode
;; doesn't define it in emacs 24.
;;

(eval-when-compile (require 'ruby-mode))
(eval-after-load 'ruby-mode
  '(defconst ruby-font-lock-syntactic-keywords
     `( ;; #{ }, #$hoge, #@foo are not comments
       ("\\(#\\)[{$@]" 1 (1 . nil))
       ;; the last $', $", $` in the respective string is not variable
       ;; the last ?', ?", ?` in the respective string is not ascii code
       ("\\(^\\|[\[ \t\n<+\(,=]\\)\\(['\"`]\\)\\(\\\\.\\|\\2\\|[^'\"`\n\\\\]\\)*?\\\\?[?$]\\(\\2\\)"
        (2 (7 . nil))
        (4 (7 . nil)))
       ;; $' $" $` .... are variables
       ;; ?' ?" ?` are ascii codes
       ("\\(^\\|[^\\\\]\\)\\(\\\\\\\\\\)*[?$]\\([#\"'`]\\)" 3 (1 . nil))
       ;; regexps
       ("\\(^\\|[[=(,~?:;<>]\\|\\(^\\|\\s \\)\\(if\\|elsif\\|unless\\|while\\|until\\|when\\|and\\|or\\|&&\\|||\\)\\|g?sub!?\\|scan\\|split!?\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
        (4 (7 . ?/))
        (6 (7 . ?/)))
       ("^=en\\(d\\)\\_>" 1 "!")
       ;; General delimited string.
       ("\\(^\\|[[ \t\n<+(,=]\\)\\(%[xrqQwW]?\\([^<[{(a-zA-Z0-9 \n]\\)[^\n\\\\]*\\(\\\\.[^\n\\\\]*\\)*\\(\\3\\)\\)"
        (3 "\"")
        (5 "\""))
       ("^\\(=\\)begin\\_>" 1 (ruby-comment-beg-syntax)))
     "Syntactic keywords for Ruby mode.  See `font-lock-syntactic-keywords'.")
)

;;
;; indent-relative - only do tab-to-tab if we can't leave it alone
;;

(defun indent-relative (&optional unindented-ok)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
The following line shows the indentation points in this line.
    ^         ^    ^     ^   ^           ^      ^  ^    ^
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead, unless
this command is invoked with a numeric argument, in which case it
does nothing.

See also `indent-relative-maybe'."
  (interactive "P")
  (if (and abbrev-mode
           (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
        indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
          (let ((end (save-excursion (forward-line 1) (point))))
            (move-to-column start-column)
            ;; Is start-column inside a tab on this line?
            (if (> (current-column) start-column)
                (backward-char 1))
            (or (looking-at "[ \t]")
                unindented-ok
                (skip-chars-forward "^ \t" end))
            (skip-chars-forward " \t" end)
            (or (= (point) end) (setq indent (current-column))))))
    (if indent
        (let ((opoint (point-marker)))
          (indent-to indent 0)
          (if (> opoint (point))
              (goto-char opoint))
          (move-marker opoint nil))
      ;; amd
      (if (not unindented-ok)
          (tab-to-tab-stop)))))
