;;; init-mscl.el --- support mscl-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(autoload 'mscl-mode "mscl-mode" "Major mode for editing MSCL code." t)
(add-to-list 'auto-mode-alist '("\\.pwx?macro\\'" . mscl-mode))

(provide 'init-mscl)
;;; init-mscl.el ends here
