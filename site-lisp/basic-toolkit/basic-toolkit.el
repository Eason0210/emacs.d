;;; basic-toolkit.el --- Basic toolkit  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------------------------
;; Open the current file or dired marked files in external default program
;; ----------------------------------------------------------------------------

(defun open-file-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that."
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'"
                                  (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;; ----------------------------------------------------------------------------
;; Date and time
;; ----------------------------------------------------------------------------

(defun insert-standard-date-time ()
  "Insert standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun insert-standard-date ()
  "Insert standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))


(provide 'basic-toolkit)
;;; basic-toolkit.el ends here
