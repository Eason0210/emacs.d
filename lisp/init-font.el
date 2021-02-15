;;; init-font.el --- Font configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)
  (setq fonts '("SF Mono" "冬青黑体简体中文"))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 16)))

(when (eq system-type 'windows-nt)
  (setq fonts '("Consolas" "微软雅黑"))
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 24)))

(when (eq system-type 'gnu/linux)
  (setq fonts '("SF Mono" "Noto Sans Mono CJK SC"))
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 24)))

(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family (car (cdr fonts))))))

(provide 'init-font)
;;; init-font.el ends here
