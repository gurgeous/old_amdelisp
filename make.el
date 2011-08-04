;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(eval-when-compile (require 'compile))

;;
;; some specific make commands. Feel free to override.
;;

(defvar make-ant "ant")
(defvar make-cs
  (format "%s\\Microsoft.Net\\Framework\\v1.0.3705\\csc.exe /nologo /debug+"
          (getenv "SystemRoot")))

(defvar make-jam "j")
(defvar make-build "build")
(defvar make-java "javac")
(defvar make-vc "nmake /nologo /s /f")
(defvar make-makefiles '("build.xml" "build.bat" "Makefile" "GNUmakefile" "Jamfile"))

;;
;; helpers
;;

(defun make-read-directory (prompt)
  "Read a directory name. The directory must exist and have the
  correct permissions."
  (let* ((dir (read-file-name prompt default-directory default-directory t)))
    (setq dir (expand-file-name (file-name-as-directory dir)))
    (if (not (file-directory-p dir))
        (if (file-exists-p dir)
            (error "%s is not a directory" dir)
          (error "%s: no such directory" dir)))
    (when (not (file-executable-p dir))
      (error "Cannot use %s:  Permission denied" dir))
    dir))

(defun make-find-makefile (dir)
  "Find a makefile in the given directory."
  (let* ((files (directory-files dir nil nil t))
         (makefile (car (intersection files make-makefiles :test 'string=))))
    (if makefile
        makefile
      (find-if (lambda (x) (string-match ".*\\.mak" x)) files))))

(defun make-guess-command (makefile)
  "Try to guess the make command given the name of the makefile."
  (cond
   ((eq makefile nil) nil)
   ((string= "build.bat"   makefile) (format "%s " make-build))
   ((string= "Jamfile"     makefile) (format "%s " make-jam))
   ((string= "build.xml"   makefile) "ant ")
   ((string-match ".*\\.mak" makefile) (format "%s %s " make-vc makefile))
   (t "make ")))

;;;###autoload
(defun make-doit (command &rest dir)
  "Compile in a given dir with the given command, preserving the
  existing command and directory. Kill the old compile if necessary."
  (require 'compile)
  (let ((compile-command command)
        (default-directory (or (car dir) default-directory))
        (last-buffer (get-buffer-create "*compilation*")))
    (if (get-buffer-process last-buffer)
        (error "Thou shalt not kill!"))
    (while (get-buffer-process last-buffer)
      (kill-compilation)
      (sit-for 1))
    (compile command)))

;;
;; entry points
;;

(defvar make-directory nil)
(defvar make-command nil)
(defvar make-command-history nil)

;;;###autoload
(defun make (&rest prompt)
  "Compile with make using the current values of make-directory and
  make-command.  If a prefix argument is specified the user will be
  prompted for the make command.  Otherwise, if either the directory
  or command is not specified, the user will be prompted then as
  well."

  ;; null out the command if a prefix argument was specified
  (if current-prefix-arg (setq make-command nil))

  ;; select directory if necessary
  (when (null make-directory)
    (setq make-directory (make-read-directory "Select make directory: ")))

  ;; select command if necessary
  (when (null make-command)
    (setq make-command
          (read-from-minibuffer
           (or (car prompt) "Select make command: ")
           (make-guess-command (make-find-makefile make-directory))
           nil nil 'make-command-history)))

  ;; now build!
  (make-doit make-command make-directory))

;;;###autoload
(defun make-remake ()
  "Select a directory & command, then make."
  (interactive)
  (setq make-directory nil
        make-command nil)
  (make))

;;;###autoload
(defun make-magic ()
  "Magically figure out how to compile the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond ((eq nil file)
           (make))
          ((string-match "\\.el$" file)
           (byte-compile-file file))
          ((string-match "\\.java$" file)
           (make-doit (format "%s %s" make-java file)))
          ((string-match "\\.cs$" file)
           (make-doit (format "%s %s" make-cs file)))
          (t (make)))))

;;;###autoload
(defun make-current-directory ()
  "Run make in the current directory."
  (interactive)
  (setq make-directory default-directory
        make-command nil)
  (make "Select make command for current directory: "))

(provide 'make)
