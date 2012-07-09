;;; find-file-in-project.el --- Find files in a project quickly.

;; amd - from https://github.com/dburger/find-file-in-project

;; Copyright (C) 2006, 2007, 2008, 2009 Phil Hagelberg, Doug Alcorn, and Will Farrington

;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Git: git://github.com/wfarr/find-file-in-project.git
;; Version: 2.1
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library depends on GNU find.

;; This file provides a few methods for quickly finding any file in
;; a given project. Projects are defined in one of four ways.  For
;; the first, third, and fourth described methods, GNU find is used
;; to locate the files within the project.  The second described
;; method uses TAGS files to define the files for a project.  The
;; different methods of defining a project are presented here in the
;; order they will be attempted when `find-file-in-project' is
;; invoked.

;; The first uses the value of the variable `ffip-project-root' when
;; this value is non-nil.

;; The second method of defining a project is to use TAGS files if
;; `ffip-try-tags' is non-nil. It will look for an ancestor directory
;; of the current file containing a TAGS file.  If found it will
;; parse the TAGS file for project file names and will not use GNU
;; find to determine project file names.

;; The third uses `locate-dominating-file'. First, if the
;; `locate-dominating-file' function is bound, it assumes you are using
;; Emacs 23, in which case it will look for a `.dir-locals.el' file in
;; an ancestor directory of the current file. Otherwise it uses the
;;`project-local-variables' library, which looks for a `.emacs-project'
;; file.

;; The fourth method takes advantage of the prominence of version
;; control systems in projects to quickly identify the tree for a
;; project. It does so using `project.el' when available. `project.el'
;; is shipped in this tree, but for reasons of encouraging using
;; default Emacs behavior when and where possible, you will need to
;; manually require it in your Emacs configuration to make use of it.

;; The first, third, and fourth methods use GNU find to find the files
;; withn a project. By default, it looks only for files whose names match
;; `ffip-patterns', but it's understood that that variable will be
;; overridden locally. This can be done either with a mode hook:

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda (set (make-local-variable 'ffip-patterns)
;;                        '("*.rb" "*.html" "*.el" "*.js" "*.rhtml"))))

;; or by setting it in your .emacs-project/.dir-settings.el file, in
;; which case it will get set locally.

;; You can also be a bit more specific about what files you want to
;; find. For instance, in a Ruby on Rails project, you may be
;; interested in all .rb files that don't exist in the "vendor"
;; directory. In that case you could locally set `ffip-find-options'
;; to "" from within a hook or your .emacs-project file. The options
;; accepted in that variable are passed directly to the Unix `find'
;; command, so any valid arguments for that program are acceptable.

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; Recommended binding:
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

;;; TODO:

;; Performance testing with large projects
;; Switch to using a hash table if it's too slow
;; Add compatibility with BSD find (PDI; I can't virtualize OS X)

;;; Code:

(defvar ffip-patterns
  '("*.rb" "*.html" "*.el" "*.js" "*.rhtml")
  "List of patterns to look for with find-file-in-project.")

(defvar ffip-find-options
  ""
  "Extra options to pass to `find' when using find-file-in-project.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defvar ffip-project-file ".git"
  "What file should ffip look for to define a project?")

(defvar ffip-try-tags nil
  "If non-nil, ffip will first look for a TAGS file to determine project files.")

(defvar ffip-use-project-cache t
  "Whether or not to cache the project files between invocations.

When non-nil the method of determining project files will be used once
and on subsequent invocations the candidate project files will come
from the cache.  When nil the method of determining project files,
TAGS files or GNU find, will be used on each invocation.")

(defvar ffip-project-cache (make-hash-table :test 'equal)
  "Cache of project root to files in that project.

Used to remove the need to re-parse the TAGS files or re-run the find
command on subsequent invocations of ffip-project-files. To clear invoke
`ffip-clear-project-cache'")

(defun ffip-clear-project-cache ()
  "Clears the project cache."
  (interactive)
  (clrhash ffip-project-cache))

(defun ffip-project-files ()
  "Returns a list of all files in the project.

First determines the project root and checks `ffip-project-cache' for the
list of files under that key.  If already cached, the cached value is
returned.  Otherwise, the TAGS file or GNU find is used to determine the
list of files for the project, which is then cached and returned."
  (let ((project-root
         (or (ffip-project-root)
             (error "No project root found"))))
    (or (and ffip-use-project-cache
             (gethash project-root ffip-project-cache))
        (puthash project-root
                 (if (file-exists-p (concat project-root "TAGS"))
                     (ffip-project-files-from-tags project-root)
                   (ffip-project-files-from-find project-root))
                 ffip-project-cache))))

(defun ffip-project-files-from-tags (project-root)
  "Returns a list of all files in the TAGS file in PROJECT-ROOT"
  (save-excursion
    (visit-tags-table project-root)
    (visit-tags-table-buffer)
    (tags-table-files)))

(defun ffip-project-files-from-find (project-root)
  "Returns a list of all files in the project using GNU find."
  (interactive)
  (split-string (shell-command-to-string
                 (format "find %s -type f %s %s"
                         project-root
                         (ffip-join-patterns)
                         ffip-find-options))))

(defun ffip-project-files-map ()
  "Return an map from the filenames in the project to their paths.

The paths are stored in a list to handle the case of multiple files
with the same filename."
  (let ((file-map (make-hash-table :test 'equal)))
    (mapc (lambda (file)
            (let ((file-name (file-name-nondirectory file))
                  file-paths)
              (setq file-paths (or (gethash file-name file-map) (list)))
              (push file file-paths)
              (puthash file-name file-paths file-map)))
            (ffip-project-files))
    file-map))

;; TODO: Emacs has some built-in uniqueify functions; investigate using those.
(defun ffip-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
          (concat (car file-cons) ": "
                  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun ffip-join-patterns ()
  "Turn `ffip-paterns' into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
             ffip-patterns " -or "))

;;;###autoload
(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable."
  (interactive)
  (let* ((project-files-map (ffip-project-files-map))
         (project-file-names (ffip-map-keys project-files-map))
         (file-name (ffip-completing-read "Find file in project: " project-file-names))
         (file-paths (gethash file-name project-files-map))
         (file-path (if (> (length file-paths) 1)
                        (ffip-completing-read "Disambiguate: " file-paths)
                      (car file-paths))))
    (find-file file-path)))

(defun ffip-map-keys (map)
  "Return a list of all the keys in MAP."
  (let (keys)
    (maphash (lambda (k v) (push k keys)) map)
    keys))

(defun ffip-completing-read (prompt names)
  "Perform a completing read over NAMES prompted by PROMPT.

ido is used for the completing read if available."
  (if (and (boundp 'ido-mode) ido-mode)
      (ido-completing-read prompt names nil t)
    (completing-read prompt names nil t)))

(defun ffip-project-root ()
  "Return the root of the project.

Returns the first non-nil value among the variable `ffip-project-root',
the first ancestor directory containing a TAGS file if `ffip-try-tags'
is non-nil, and the value from `project-root' or
`ffip-locate-dominating-file'."
  (let ((project-root
         (or ffip-project-root
             (and ffip-try-tags
                  (ffip-locate-dominating-file default-directory "TAGS"))
             (if (featurep 'project) (project-root)
               ;; TODO: provide a list of files that can be fallen back upon
               (ffip-locate-dominating-file default-directory ffip-project-file)))))
    (or project-root
        (progn (message "No project was defined for the current file.")
               nil))))

;; Backport functionality to Emacs 22
(if (functionp 'locate-dominating-file)
    (defalias 'ffip-locate-dominating-file 'locate-dominating-file)
  (defun ffip-locate-dominating-file (file name)
    "Look up the project file in and above `file'."
    (let ((parent (file-truename (expand-file-name ".." file))))
      (cond ((string= file parent) nil)
            ((file-exists-p (concat file name)) file)
            (t (plv-find-project-file parent name))))))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
