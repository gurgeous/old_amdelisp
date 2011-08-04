(add-to-list 'auto-mode-alist '("Makedefs\\." . makefile-mode))

(defun make-guess-command (makefile)
  "Try to guess the make command given the name of the makefile."
  (cond
   ((eq makefile nil) nil)
   ((string= "build.bat"   makefile) (format "%s " make-build))
   ((string= "Jamfile"     makefile) (format "%s " make-jam))
   ((string= "build.xml"   makefile) "ant ")
   ((string-match ".*\\.mak" makefile) (format "%s %s " make-vc makefile))
   ((string= "rules" makefile) "rules/build.sh -d development")   
   (t "make ")))
