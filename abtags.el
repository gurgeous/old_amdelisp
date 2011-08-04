;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; abtags.el -- implements the infamous prj system's keybindings using etags
;;
;; Copyright 2000-2004, Adam B. Feder (ab@alum.mit.edu)
;; Feb 2005 - edits by amd@gurge.com
;;
;; You have the right to use and modify this elisp.  If you modify it,
;; please send your modifications to ab@alum.mit.edu so that your
;; improvements can be incorporated in future versions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'etags)

;;
;; helpers
;;

(defun abtags-basename-sans-extension (path)
  "Removes the filename extension"
  (setq path (file-name-nondirectory path))
  (if (string-match "\\([^.]+\\)\\." path)
      (concat (match-string 1 path) ".")
    path))

(defun abtags-get-current-line ()
  "Returns the current line as a string."
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
          (end (progn (end-of-line)       (point))))
      (buffer-substring beg end))))

(defun abtags-list-buffer-choices (basename)
  "Find open buffers that match basename."
  (let (result)
    (dolist (buffer (buffer-list))
      (let ((name (buffer-file-name buffer)))
        (when (and (not (null name))
                   (string= (abtags-basename-sans-extension name) basename))
          (push name result))))
    result))

(defun abtags-get-subexpr (desc str)
  (let ((section (car (cdr desc))))
    (if (string-match (car desc) str)
        (substring str (match-beginning section) (match-end section))
      nil)))

(defvar abtags-include-regexp-alist 
  (list '("uses[ \t]+\"\\([^\"]+\\)\"" 1)
        '("^#include[ \t]+[<\"]\\([^>\"]+\\)[>\"]" 1)
        '("^source.*/\\([^/]+\\)[ \t]*$" 1)
        '("^import[ \t]+[<\"]\\([^>\"]+\\)[>\"]" 1)))

;;
;; the cache, and code to invalidate / rebuild the cache
;;

(defvar abtags-cache-tags-name nil
  "The name of the tags file currently in use by abtags.")

(defvar abtags-cache-timestamp nil
  "The modification time of the current tags file.")

(defvar abtags-cache nil
  "Alist of basename . pathname for all files in TAGS.")

(defun abtags-refresh-cache ()
  "Ensures abtags-cache has the latest data."
  ;; is the cache invalid? did the user load a different TAGS file?
  (when (or (null abtags-cache-tags-name)
            (not (string= abtags-cache-tags-name tags-file-name))
            (not (equal (nth 5 (file-attributes abtags-cache-tags-name))
                        abtags-cache-timestamp)))
    ;; this prevents a stray message from hiding our "rebuilding" message
    (load "cl-extra")
    (save-excursion
      (when tags-file-name
        (set-buffer (get-file-buffer tags-file-name))
        (setq abtags-cache nil
              tags-table-files nil))
      (visit-tags-table-buffer tags-file-name)
      (message "abtags : rebuilding...")
      (let* ((files (tags-table-files))
             (len (1- (length files))))
        (loop for i from 0 to len do
              (let* ((path (expand-file-name (nth i files))))
                (setq abtags-cache (cons (cons (file-name-nondirectory path) path) abtags-cache))
                (when (equal (mod i 500) 0)
                  (message "abtags : rebuilding... %2d%%" (truncate (* (/ (float i) len) 100))))
                )))
      (message "abtags : done."))
    (setq abtags-cache-tags-name tags-file-name)
    (setq abtags-cache-timestamp (nth 5 (file-attributes abtags-cache-tags-name)))))

(defun abtags-find-basenames (basename)
  "Find files that match basename."
  (let ((files (all-completions basename abtags-cache))
        result)
    (dolist (i abtags-cache)
      (when (member (car i) files)
        (push (cdr i) result)))
    result))

(defun abtags-truenames= (a b)
  "Returns true if a and b have the same file-truename."
  (string= (file-truename a) (file-truename b)))

;;
;; entry points
;;

;;;###autoload
(defun abtags-find-file ()
  "Prompts the user to select a filename from among those in the tags file.
   Visits the selected file."
  (interactive)
  (abtags-refresh-cache)
  (find-file
   (cdr (assoc
         (completing-read "Find file: " abtags-cache nil t)
         abtags-cache))))

;;;###autoload
(defun abtags-find-next-file ()
  "Prompts the user to select a filename from among those in the tags file.
   Visits the selected file."
  (interactive)
  (when (not (buffer-file-name))
    (error "Buffer is not a file."))
  (when tags-file-name
    (abtags-refresh-cache))
  (let* ((name (buffer-file-name))
         (basename (abtags-basename-sans-extension name))
         ;; the list of candidates is the union of:
         ;;   1) files in TAGS that start with basename
         ;;   2) open buffers that start with basename
         (paths (union
                 (abtags-find-basenames basename)
                 (abtags-list-buffer-choices basename)
                 :test 'abtags-truenames=)))
    (when (= (length paths) 1)
      (error "This is the only file matching '%s*'" basename))
    ;; open the next candidate, use mod to wrap
    (find-file
     (file-truename
      (nth (mod (1+ (position name paths :test 'abtags-truenames=)) (length paths))
           paths)))))

;;;###autoload
(defun abtags-find-include ()
  "Follow a #include statement."
  (interactive)
  (let* (name
         (options abtags-include-regexp-alist))
    (abtags-refresh-cache)
    (while (and options (not name))
      (setq name (abtags-get-subexpr (car options) (abtags-get-current-line)))
      (setq options (cdr options)))
    (if name
        (find-file
         (file-truename
          (cdr (assoc (file-name-nondirectory name) abtags-cache))))
      (error "Not an include line."))))

;;
;; key bindings
;;

(defvar abtags-key-map)
(define-prefix-command 'abtags-key-map)
(define-key abtags-key-map "\M-f" 'abtags-find-file)
(define-key abtags-key-map "\M-o" 'abtags-find-next-file)
(define-key abtags-key-map "\M-i" 'abtags-find-include)

(defvar abtags-keymap-prefix "\M-z"
  "The key sequence to bind the abtags key map to, or nil if not to
  bind at all.")
(if (not (null abtags-keymap-prefix))
    (global-set-key abtags-keymap-prefix abtags-key-map))

(provide 'abtags)
