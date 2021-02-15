;;; init-key.el --- key binding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; key setting
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)
  (w32-register-hot-key [s-])

  (setq w32-pass-apps-to-system nil)
  (define-key key-translation-map (kbd "<apps>") (kbd "C-\\"))
  )

;; undo-redo
(define-key global-map (kbd "C-?") 'undo-redo)

(provide 'init-key)
;;; init-key.el ends here
