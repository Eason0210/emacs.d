;;; init-quickrun.el --- Configure for quickrun  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'quickrun)
  (define-key global-map (kbd "<f5>") 'quickrun))

(provide 'init-quickrun)
;;; init-quickrun.el ends here
