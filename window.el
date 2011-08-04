;;;
;;; This file is only loaded when window-system is true.
;;;

(require 'cl)
(require 'font-lock)

(defun window-build-font (family points)
  "Given a font family and a point size, this function builds the magic
font string that emacs uses to represent that font. The mono-font-sizes
list is used to fill in the magic values for the font name."
  (let ((mono-font-sizes '((8 11 82) (9 12 90) (10 13 97)
                           (11 15 112) (12 16 120))))
    (format "-*-%s-normal-r-normal-normal-%s-%s-*-*-c-*-iso8859-15"
            family
            (nth 1 (assoc points mono-font-sizes))
            (nth 2 (assoc points mono-font-sizes)))))

;;; set or append a frame attribute in the default-frame-alist
(defun window-set-frame-default (key value)
  (let ((val (assoc key default-frame-alist)))
    (if (null val)
        (setcdr (last default-frame-alist) (list (cons key value)))
      (rplacd val value)))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function to setup the window properties

(defun window-setup ()
  (if is-win32 (set-frame-font my-font))
  (if display-time-format (display-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Position the main window at startup.
;;; Executed after .emacs is loaded in the after-init-hook.

(defvar window-position 'center)
(defvar window-columns 100)
(defvar window-fudge '(0 0 0 0))

(defun window-layout ()
  (window-setup)
  (when is-win32
    (window-set-frame-default 'font my-font))
  (when window-position 
    (let ((display-width  (x-display-pixel-width))
          (display-height (x-display-pixel-height))
          (left   (nth 0 window-fudge))
          (right  (nth 1 window-fudge))
          (top    (nth 2 window-fudge))
          (bottom (nth 3 window-fudge))
          (width (* (+ 2 window-columns) (frame-char-width)))
          )
      
      (window-set-frame-default
       'left
       (case window-position
         ('right (- display-width (+ width right 15)))
         ('left left)
         ('center (/ (- display-width width) 2))))
      (window-set-frame-default 'top top)
      (window-set-frame-default 'width window-columns)
      (window-set-frame-default 'height (/ (- display-height top bottom 25)
                                           (frame-char-height))))))
(add-hook 'after-init-hook 'window-layout)
