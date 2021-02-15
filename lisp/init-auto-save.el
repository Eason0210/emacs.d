;;; init-auto-save.el --- support auto-save -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)

(setq auto-save-delete-trailing-whitespace t)

(add-hook 'multiple-cursors-mode-enabled-hook
          '(lambda ()
             (setq auto-save-delete-trailing-whitespace nil)))

(add-hook 'multiple-cursors-mode-disabled-hook
          '(lambda ()
             (setq auto-save-delete-trailing-whitespace t)))

(provide 'init-auto-save)
;;; init-auto-save.el ends here
