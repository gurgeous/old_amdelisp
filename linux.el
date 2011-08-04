;;;;;;;;;;;;;;;;;;;;;;;;; linux ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; numeric keypad (through putty, at least)
(define-key function-key-map "Or" [down])
(define-key function-key-map "Os" [next])
(define-key function-key-map "Ot" [left])
(define-key function-key-map "Ov" [right])
(define-key function-key-map "Ox" [up])
(define-key function-key-map "Oy" [prior])

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(when window-system
  (setf interprogram-paste-function 'x-cut-buffer-or-selection-value))
