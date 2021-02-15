;;; init-go-translate.el --- go-translate setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; go-translate
(when (maybe-require-package 'go-translate)
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-buffer-follow-p t)
  (setq go-translate-inputs-function
        #'go-translate-inputs-current-or-prompt)
  (setq go-translate-token-current
        (cons 430675 2721866133))
  (define-key global-map (kbd "C-c s") 'go-translate-kill-ring-save)
  (define-key global-map (kbd "C-c t") 'go-translate))

(provide 'init-go-translate)
;;; init-go-translate.el ends here
