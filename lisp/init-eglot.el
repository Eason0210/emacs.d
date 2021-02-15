;;; init-eglot.el --- Configure for eglot  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'eglot)
  (when (maybe-require-package 'diminish)
    (diminish 'eglot))
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 ))
    (add-hook hook '(lambda ()
                      (eglot-ensure)
                      (add-to-list 'eglot-server-programs
                                   '((c++-mode c-mode) "clangd"))

                      (add-to-list 'eglot-server-programs
                                   '(python-mode . ("pyright-langserver" "--stdio")))

                      (add-to-list 'eglot-server-programs
                                   '(rustic-mode . ("rust-analyzer")))

                      ;; (require 'flycheck-eglot)
                      (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))
                      (setq eldoc-echo-area-use-multiline-p 1)))))

(provide 'init-eglot)
;;; init-eglot.el ends here
