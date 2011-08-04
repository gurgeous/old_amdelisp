;;; svn.el - run subversion on the current buffer

(defvar svn-buffer "*SVN*" "SVN Buffer")

;;;###autoload
(defun svn-add ()
  "Add current file to svn."
  (interactive)
  (let ((path (svn-path)))
    (svn-exec (list "add" path))))

;;;###autoload
(defun svn-rm ()
  "Remove current file from svn."
  (interactive)
  (let ((path (svn-path)))
    (when (and (yes-or-no-p "Really remove? ")  
               (svn-exec (list "rm" path)))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun svn-revert (&rest force)
  "Revert current file."  
  (interactive)
  (let ((path (svn-path)))
    (when (and (or force (yes-or-no-p "Really revert changes? "))
               (svn-exec (list "revert" path)))
      (flet ((message (x) nil))
        (revert-buffer t t)))))

;;;###autoload
(defun svn-diff ()
  "Show diff for current file."
  (interactive)
  (when (svn-exec (list "diff" "--diff-cmd"
                        (if is-win32
                            (format "%s/diff" (getenv "TOOLS"))
                          "/usr/bin/diff")
                        (svn-path))
                  t)
    (save-excursion
      (set-buffer svn-buffer)
      (diff-mode))))

;;;###autoload
(defun svn-ediff ()
  "Show ediff for current file."
  (interactive)
  (let* ((cur-buffer-file-name (buffer-file-name))
         (cur-buffer (buffer-name))
         (svn-buf-name "svn-base")
         (svn-process-buf-name "*svn-process*")
         (cleanup `(lambda ()
                     (let ((buf (get-buffer svn-buf-name)))
                       (when (not (null buf))
                         (let ((win (get-buffer-window buf t)))
                           (when win (delete-window win))
                           (kill-buffer buf)))))))
    (find-file svn-buf-name)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (svn-run-svn nil t 'cat (append (list "cat") (list cur-buffer-file-name)))
    (insert-buffer-substring svn-process-buf-name)
    (save-buffer)
    (ediff-buffers cur-buffer
                   svn-buf-name
                   (list (lambda ()
                           (add-hook 'ediff-after-quit-hook-internal
                                     cleanup
                                     nil
                                     'local))))))
;;;###autoload
(defun svn-resolved ()
  "Mark the current file as resolved."
  (interactive)
  (svn-exec (list "resolved" (svn-path)) t))

;;;###autoload
(defun svn-info ()
  "Display the subversion info about the current file."
  (interactive)
  (svn-exec (list "info" (svn-path)) t))

;;;###autoload
(defun svn-log ()
  "Show log for current file."
  (interactive)
  (svn-exec (list "log" "--limit" "50" (svn-path)) t))

;;;###autoload
(defun svn-blame ()
  "Show blame for current file."
  (interactive)
  (let ((old-major-mode major-mode))
    (svn-exec (list "blame" (svn-path)) t)
    (set-buffer (get-buffer-create svn-buffer))
    ;; comment out the blame info
    (goto-char (point-min))    
    (while (re-search-forward "^\\s-+\\w+\\s-+\\w+" nil t)
      (replace-match "/*\\&*/" nil nil))
    ;; deal with existing comments
    (goto-char (point-min))    
    (while (re-search-forward "\\(^.+\\*/\\s-+\\)\\*[^/]" nil t)
      (replace-match "\\1//" nil))
    ;; apply the major-mode of the buffer we are blaming
    (funcall old-major-mode)))
  
;;;###autoload
;; (defun svn-update ()
;;   "Update svn project, starting at the top."
;;   (interactive)
;;   (save-excursion
;;     (let ((buf (svn-get-buffer))
;;    (dir (file-name-directory (svn-root))))
;;       (svn-show-buffer t)
;;       (set-buffer buf)
;;       (setq default-directory dir)
;;       (insert "cd " dir "\nsvn update\n\n")
;;       (svn-update-mode)
;;       (let* ((default-directory dir)
;;       (proc (start-process "svn" buf "svn" "update")))
;;  (set-process-sentinel proc 'svn-update-sentinel)))))

;;
;; helpers
;;

(defun svn-exec (args &optional show)
  "Run svn into *SVN*, with args."
  (save-excursion
    (let ((buf (svn-get-buffer)))
      (set-buffer buf)
      (let ((result (apply 'call-process "svn" nil buf nil args)))
        (message (buffer-substring
                  (point-min)
                  (save-excursion
                    (goto-char (point-min))
                    (end-of-line)
                    (point))))
        (when (and result show)
          (svn-show-buffer))
        result))))

(defun svn-get-buffer ()
  "Get a new buffer for svn output."
  (save-excursion
    (when (and (get-buffer-window svn-buffer)
               (not (one-window-p)))
      (delete-window (get-buffer-window svn-buffer)))
    (get-buffer-create svn-buffer)
    (kill-buffer svn-buffer)
    (let ((buf (get-buffer-create svn-buffer)))
      (set-buffer buf)
      (bury-buffer buf)
      (fundamental-mode)
      buf)))

(defun svn-show-buffer (&optional max)
  "Show the svn buffer."
  (save-selected-window
    (require 'compile)
    (pop-to-buffer svn-buffer t t)
    (compilation-set-window-height (selected-window))
    (goto-char (if max (point-max) (point-min)))))

(defun svn-path ()
  "Return the true path for the current buffer."
  (when (not buffer-file-name)
    (error "Buffer does not seem to be associated with any file"))
  (file-truename (expand-file-name buffer-file-name)))

;;;###autoload
(defun svn-root ()
  "Return the topmost project in the given path"
  (find-topmost-in-path (file-name-directory (svn-path)) ".svn"))

;;
;; svn-update helpers
;;

(defun svn-update-sentinel (proc msg)
  "Sentinel that watches the svn update process."
  (let ((buffer (process-buffer proc)))
    (when (memq (process-status proc) '(signal exit))
      (if (null (buffer-name buffer))
          (set-process-buffer proc nil)
        (let ((obuf (current-buffer)))
          (unwind-protect
              (progn
                (set-buffer buffer)
                ;; highlight conflicts at the bottom
                (let (conflicts)
                  (goto-char (point-min))
                  (while (re-search-forward "^C  .+" nil t)
                    (push (match-string 0) conflicts))
                  (when conflicts
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (mapc (lambda (x)
                            (insert (format "%s\n" x)))
                          conflicts)))
                (insert "\n\n")
                (svn-create-all-links)
                (goto-char (point-max))
                (delete-process proc))
            (set-buffer obuf))))
      )))

(defun svn-create-all-links ()
  "Turn update lines into links"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^D]  \\(.+\\)" nil t)
      (svn-create-link (match-beginning 1)
                       (match-end 1)
                       (list (cons 'filename (match-string 1)))))))

(defun svn-create-link (start end props)
  "Create a specific link"
  (let ((ext (make-overlay start end)))
    (dolist (i (append (list (cons 'face 'underline)
                             (cons 'mouse-face 'highlight))
                       props))
      (overlay-put ext (car i) (cdr i)))))

(defun svn-clicked (event)
  "Handle mouse clicks"
  (interactive "e")
  (let ((win (posn-window (event-end event)))
        (pnt (posn-point (event-start event))))
    (select-window win)
    (goto-char pnt)
    (svn-commands pnt)))

(defun svn-commands (pnt)
  "Handle enter / mouse click."
  (interactive "d")
  (let ((filename (get-char-property pnt 'filename)))
    (cond (filename
           (find-file filename))
          (t
           (error "There is no file at that cursor location!")))))
  
(defvar svn-big-face 'Info-title-1-face)

(define-generic-mode 'svn-update-mode
  nil
  nil
  (list
   (list "^C  .+" 0 'svn-big-face)
   (list " revision \\([0-9]+\\)." 1 'font-lock-keyword-face))
  nil
  (list (lambda ()
          (let ((map (make-sparse-keymap)))
            (define-key map [down-mouse-1] 'svn-clicked)
            (define-key map [return] 'svn-commands)
            (use-local-map map)))
        )
  "Generic mode for svn update buffers.")

(provide 'svn)
