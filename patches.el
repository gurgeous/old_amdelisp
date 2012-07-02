;;
;; Fix the auto-revert-handler so that if the system time is very
;; close to the new modified time for a file, skip it on this
;; iteration. This should fix race conditions when a file is changed
;; multiple times within the same second.
;;
;; (from jp)
;; 

(eval-after-load "autorevert"
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

