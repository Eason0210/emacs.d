;;; init-c.el --- Setup for c/c++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'modern-cpp-font-lock)
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook
   hook
   '(lambda ()
      (require 'cc-mode)
      (defun c-mode-style-setup ()
        (interactive)
        "Set up c-mode and related modes.
Includes support for Qt code (signal, slots and alikes)."

        ;; cpp font lock.
        (modern-c++-font-lock-global-mode t)
        ;; base-style
        (c-set-style "stroustrup")

        ;; c/c++ keywords and stuff ...
        ;; set up indenting correctly for new c/c++ kewords
        (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                       "\\|protected slot\\|private\\|private slot"
                                       "\\)\\>")
              c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                       "\\|public slots\\|protected slots\\|private slots"
                                       "\\)\\>[ \t]*:")))

      (c-mode-style-setup))))


(provide 'init-c)
;;; init-c.el ends here
