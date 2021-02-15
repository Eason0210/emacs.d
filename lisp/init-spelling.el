;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ispell)

(when (executable-find ispell-program-name)
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (setq ispell-personal-dictionary
        (expand-file-name "flyspell/.aspell.en.pws" "~/emacs-data/"))

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

    (when (maybe-require-package 'flyspell-correct)
      (define-key flyspell-mode-map (kbd "C-,") 'flyspell-correct-wrapper))))

(provide 'init-spelling)
;;; init-spelling.el ends here
