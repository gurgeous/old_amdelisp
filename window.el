;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Position the main window at startup.
;;; Executed after .emacs is loaded in the after-init-hook.

(defvar window-position 'right)
(defvar window-columns 100)
(defvar window-fudge '(0 12 0 55))

(defun window-layout ()
  (let ((display-width  (x-display-pixel-width))
        (display-height (x-display-pixel-height))
        (left   (nth 0 window-fudge))
        (right  (nth 1 window-fudge))
        (top    (nth 2 window-fudge))
        (bottom (nth 3 window-fudge))
        (width (* (+ 2 window-columns) (frame-char-width)))
        )

    (add-to-list 'default-frame-alist (cons 'left
                                            (case window-position
                                              ('right (- display-width (+ width right 15)))
                                              ('left left)
                                              ('center (/ (- display-width width) 2)))))
    (add-to-list 'default-frame-alist (cons 'top top))
    (add-to-list 'default-frame-alist (cons 'width window-columns)) 
    (add-to-list 'default-frame-alist (cons 'height (/ (- display-height top bottom 25)
                                                       (frame-char-height))))))
(add-hook 'after-init-hook 'window-layout)
