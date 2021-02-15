;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (add-auto-mode 'gfm-mode "README\\.md\\'")

  (with-eval-after-load 'markdown-mode
    (setq markdown-command "multimarkdown"))

  (with-eval-after-load 'auto-save
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (setq-local auto-save-delete-trailing-whitespace nil)))))

(provide 'init-markdown)
;;; init-markdown.el ends here
