;;; init-rime.el --- Configuration for rime -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'rime)

  (when (eq system-type 'windows-nt)
    (setq rime-user-data-dir "~/emacs-data/rime")
    (setq rime-share-data-dir "~/emacs-data/rime/data")
    (setq rime-minibuffer-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :font "微软雅黑"
                :internal-border-width 10)))

  (when (eq system-type 'gnu/linux)
    (setq rime-minibuffer-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :font "Noto Sans Mono CJK SC"
                :internal-border-width 10)))

  (when (eq system-type 'darwin)
    (setq rime-librime-root  "~/emacs-data/librime/dist")
    (setq rime-user-data-dir "~/emacs-data/rime/")
    (setq rime-minibuffer-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :font "冬青黑体简体中文"
                :internal-border-width 10)))

  (setq default-input-method "rime"
        rime-show-candidate nil)

  (setq rime-show-preedit t)

  ;;代码中直接英文，注释和其他模式中根据断言选择输入模式
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-current-uppercase-letter-p
                                  rime-predicate-after-alphabet-char-p
                                  rime-predicate-punctuation-after-space-cc-p
                                  rime-predicate-punctuation-line-begin-p
                                  rime-predicate-after-ascii-char-p
                                  rime-predicate-space-after-cc-p))

  ;; keybinding
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  (global-set-key (kbd "s-m") 'rime-force-enable)
  (global-set-key (kbd "C-`") 'rime-send-keybinding)

  ;; switch the way to display candidate
  (defun rime-switch-candidate-display-style ()
    "Use minibuffer for candidate if current is nil."
    (interactive)
    (if (equal rime-show-candidate nil)
        (setq rime-show-candidate 'minibuffer)
      (setq rime-show-candidate nil))))


(provide 'init-rime)

;;; init-rime.el ends here
