;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(progn ; `startup'
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  ;; (setq initial-buffer-choice t)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose nil)
  (setq use-package-compute-statistics nil)
  (require  'use-package))

;; Set up exec-path to help Emacs find programs
(use-package exec-path-from-shell
  :init
  (with-eval-after-load 'exec-path-from-shell
    (dolist (var '("GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
      (add-to-list 'exec-path-from-shell-variables var)))

  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(use-package dash)
(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ; `startup'
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))
(progn ; `ns-win'
  (when *is-a-mac*
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)))

;;; Long tail

(use-package diminish)

;;; Elisp helper functions and commands

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

(use-package scratch
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :hook (after-init . reapply-themes)
  :bind ("C-c t b" . sanityinc-tomorrow-themes-toggle)
  :custom
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.
  (custom-enabled-themes '(sanityinc-tomorrow-bright))
  :preface
  ;; Ensure that themes will be applied even if they have not been customized
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  ;; Toggle between light and dark
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-day))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
    (reapply-themes))

  (defun sanityinc-tomorrow-themes-toggle ()
    "Toggle between `sanityinc-tomorrow-bright' and `sanityinc-tomorrow-day'."
    (interactive)
    (if (eq (car custom-enabled-themes) 'sanityinc-tomorrow-bright)
        (light)
      (dark))))

(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)
  (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)
  :preface
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p))))

;;; GUI frames

(when *is-a-mac*
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode 1)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(progn ; `pixel-scroll'
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)))


;;; Dired mode

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-alh")
  (setq-default dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always))

(use-package diredfl
  :config
  (diredfl-global-mode 1))

;;; Isearch settings

;; Show number of matches while searching
(use-package anzu
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace-regexp))
  :config
  (setq anzu-mode-lighter ""))

(use-package isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)
              ("C-c C-o" . isearch-occur)
              ("\C-\M-w" . isearch-yank-symbol)
              ([(control return)] . sanityinc/isearch-exit-other-end))
  :config
  (setq isearch-motion-changes-direction t)
  (setq isearch-allow-motion t)
  :preface
  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (if sym
          (progn
            (setq isearch-regexp t
                  isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
        (ding)))
    (isearch-search-and-update))

  (defun sanityinc/isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end)))


;;; Configure uniquification of buffer names

;; Nicer naming of buffers for files with identical names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


;;; Ibuffer settings

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq-default ibuffer-show-empty-filter-groups nil)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 12 12 :left :elide)
                " "
                vc-relative-file)
          (mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 14 14 :left :elide)
                " "
                (vc-status 12 12 :left)
                " "
                vc-relative-file)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(use-package fullframe
  :after ibuffer
  :config
  (fullframe ibuffer ibuffer-quit))


;;; Configure FlyCheck global behavior

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :after flycheck)

;;; minibufer configuration
(use-package vertico
  :demand t
  :init
  (progn
    (setq vertico-cycle t))
  :config
  (progn
    (vertico-mode)))

(use-package orderless
  :demand t
  :hook (minibuffer-setup . sanityinc/use-orderless-in-minibuffer)
  :config
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :preface
  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless))))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t))


;; YASnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)


;; Completion with company
;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO: Default sort order should place [a-z] before punctuation

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (("M-C-/" . company-complete)
         :map company-mode-map
         ("M-/" . company-complete)
         ([remap completion-at-point] . company-complete)
         ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location)
         ("<tab>" . smarter-tab-to-complete))
  :init
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  :config
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  :preface
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current
 cursor position. If all failed, try to complete the common part with
 `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common))))))


(use-package consult
  :defer 0.5
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)  ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line) ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Configure `projectile' returns the project root directory.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))


(use-package consult-flycheck
  :defer t
  :after (consult flycheck))

(use-package marginalia
  :init (marginalia-mode))


;; Integration Embark with `vertico' and `consult'
;; The command `embark-dwim' executes the default action at point.
;; `embark-dwim' acts like `xref-find-definitions' on the symbol at point.
;; C-. can be seen as a right-click context menu at point and M-. acts like left-click.
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Use which key like a key menu prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (when-let ((win (get-buffer-window which-key--buffer
                                       'visible)))
      (quit-window 'kill-buffer win)
      (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
        (apply fn args))))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package avy
  :bind ("C-;" . avy-goto-char-timer))


;;; Settings for hippie-expand

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))


;;; Working with Windows within frames

;; Navigate window layouts with "M-N" and "M-P"
(use-package winner
  :defer 0.5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil))

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(bind-key "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(bind-key "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

;; Rearrange split windows
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(bind-key "C-x |" 'split-window-horizontally-instead)
(bind-key "C-x _" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(bind-key "<f7>" 'sanityinc/split-window)

;; Toggle to dedicated window
(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(bind-key "C-c <down>" 'sanityinc/toggle-current-window-dedication)


;;; Settings for tracking recent files

(use-package recentf
  :demand t
  :init
  (progn
    (setq recentf-max-saved-items 1000)
    (setq recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'"))))
  :config
  (progn
    (recentf-mode)))

;;; Save and restore editor sessions between restarts

;; Save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :config
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)

  (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)
  (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

  ;; Save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  (desktop-save-mode 1)
  :preface
  (defun sanityinc/time-subtract-millis (b a)
    (* 1000.0 (float-time (time-subtract b a))))

  (defun sanityinc/desktop-time-restore (orig &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig args)
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))

  (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig ver filename args)
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename)))))))

;; Restore histories and registers after saving

(use-package savehist
  :config (savehist-mode))

(use-package session
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file (locate-user-emacs-file ".session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8))

;;; Editing utils

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 column-number-mode t
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 ;; truncate-lines nil
 truncate-partial-width-windows nil
 tooltip-delay 1.5)

(bind-key "C-x x p" 'pop-to-mark-command)

(add-hook 'after-init-hook 'delete-selection-mode)


(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))


;; Huge files

(use-package so-long
  :config
  (progn
    (global-so-long-mode)))

(use-package vlf
  :defer t
  :preface
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (require 'ffap)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

;; A light will shine on top of cursor when window scrolls
(use-package beacon
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  :config
  (beacon-mode 1))


;; Show line number
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

;; Display buffer boundaries and fill column indicator
(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))


(progn ;; `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :diminish
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))


;; Zap *up* to char is a handy pair for zap-to-char
(bind-key "M-Z" 'zap-up-to-char)

;; Show matching parens
(use-package paren
  :config (show-paren-mode))

;; Handy key bindings
;; (bind-key "C-." 'set-mark-command)
(bind-key "C-x C-." 'pop-global-mark)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'kill-back-to-indentation)

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
(use-package move-dup
  :bind (
         ("C-c d" . move-dup-duplicate-down)
         ("C-c u" . move-dup-duplicate-up)
         ([M-up] . move-dup-move-lines-up)
         ([M-down] . move-dup-move-lines-down)))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :hook (after-init . whole-line-or-region-global-mode))

;; M-^ is inconvenient, so also bind M-j
(bind-key "M-j" 'join-line)

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

;; Display available keybindings
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))


;; Utilities for opening files with sudo
(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

;;; Whitespace

(setq-default show-trailing-whitespace nil)

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

(bind-key [remap just-one-space] 'cycle-spacing)

;; An unobtrusive way to trim spaces from end of line
(use-package ws-butler
  :diminish
  :hook (after-init . ws-butler-global-mode))


;;; Version control

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

;; Git SCM support
(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  :bind (([(meta f12)] . magit-status)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  (progn
    (setq magit-diff-refine-hunk t)
    (setq magit-branch-prefer-remote-upstream '("master"))
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")))
    (setq magit-module-sections-nested nil)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-no-confirm '(amend-published))
    (setq magit-revision-insert-related-refs nil)
    (setq magit-revision-show-gravatars t)
    (setq magit-clone-set-remote.pushDefault t))
  :config
  (progn
    ;; Enable magit-clean
    (put 'magit-clean 'disabled nil)

    ;; Add modules in magit status buffer:
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)

    ;; Only show the module sections I'm interested in
    (with-eval-after-load "magit-submodule"
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

    (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
      '("x" "Absorb changes" magit-commit-absorb))))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package magit-todos
  :defer t
  :after magit)

;; Github integration
(use-package forge
  :defer t
  :after magit)

(use-package code-review
  :defer t
  :after magit
  :bind (:map magit-mode-map
              ("C-c r" . code-review-forge-pr-at-point)))

;;; Use Projectile for navigation within projects

(use-package projectile
  :hook (after-init . projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden")))

(use-package ibuffer-projectile
  :after ibuffer projectile)


;;; Helpers for M-x compile

(use-package compile
  :bind ([f6] . recompile)
  :config
  (setq-default compilation-scroll-output t))

;;Configuration for quickrun
(use-package quickrun
  :bind (("<f5>" . quickrun)
         ("C-<f5>" . quickrun-shell))
  :config
  (quickrun-add-command "c++/c1z"
                        '((:command . "g++")
                          (:exec    . ("%c -std=c++1z %o -o %e %s"
                                       "%e %a"))
                          (:remove  . ("%e")))
                        :default "c++"))


;;; Org-mode config

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("C-c i a" . org-id-get-create)
         ("C-c e d" . org-export-docx)
         :map sanityinc/org-global-prefix-map
         ("j" . org-clock-goto)
         ("l" . org-clock-in-last)
         ("i" . org-clock-in)
         ("o" . org-clock-out)
         ("b" . org-mark-ring-goto)
         :map org-src-mode-map
         ;; I prefer C-c C-c over C-c ' (more consistent)
         ("C-c C-c" . org-edit-src-exit))
  :bind-keymap ("C-c o" . sanityinc/org-global-prefix-map)
  :config
  ;; Various preferences
  (setq org-modules nil                 ; Faster loading
        org-log-done 'time
        org-fontify-done-headline nil
        org-edit-timestamp-down-means-later t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-tags-column 80
        org-hide-emphasis-markers t)


  ;; Lots of stuff from http://doc.norang.ca/org-mode.html

  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (add-hook
                 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

  ;; Directories settings
  (setq org-directory "~/org/agenda/")
  (setq org-default-notes-file (concat org-directory "inbox.org"))

  (setq org-agenda-files (quote ("~/org/agenda")))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

  ;; Capturing
  (setq org-capture-templates
        `(("t" "todo" entry (file "") ; "" => `org-default-notes-file'
           "* NEXT %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))


  ;; Refiling

  (setq org-refile-use-cache nil)

  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; Exclude DONE state tasks from refile targets
  (defun sanityinc/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

  (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  ;; Targets start with the file name - allows creating level 1 tasks
  ;;(setq org-refile-use-outline-path (quote file))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)


  ;; To-do settings
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
        org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        (quote (("NEXT" :inherit warning)
                ("PROJECT" :inherit font-lock-string-face))))


  ;; Agenda views

  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


  (let ((active-project-match "-INBOX/PROJECT"))

    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))

    (setq org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          org-agenda-sorting-strategy
          '((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
          `(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            ("g" "GTD"
             ((agenda "" nil)
              (tags "INBOX"
                    ((org-agenda-overriding-header "Inbox")
                     (org-tags-match-list-sublevels nil)))
              (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled 'future)))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(todo-state-down effort-up category-keep))))
              (tags-todo ,active-project-match
                         ((org-agenda-overriding-header "Projects")
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX/-NEXT"
                         ((org-agenda-overriding-header "Orphaned Tasks")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                  (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "/WAITING"
                         ((org-agenda-overriding-header "Waiting")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "/DELEGATED"
                         ((org-agenda-overriding-header "Delegated")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "On Hold")
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              ;; (tags-todo "-NEXT"
              ;;            ((org-agenda-overriding-header "All other TODOs")
              ;;             (org-match-list-sublevels t)))
              )))))


  (add-hook 'org-agenda-mode-hook 'hl-line-mode)


  ;; Org clock

  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (with-eval-after-load 'org-clock
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


  ;; Archiving

  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archive")


  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (haskell . nil)))

  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))

  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block )
  :preface
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")
  ;; Export to docx
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template/template.docx"
                                           user-emacs-directory)))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file))
      (message "Convert finish: %s" docx-file))))


;; Writing mode similar to the famous Writeroom editor for OS X
(use-package writeroom-mode
  :hook (org-mode . prose-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

;; Roam
(when (and (executable-find "sqlite3") (executable-find "cc"))
  (use-package org-roam
    :diminish
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n r" . org-roam-ref-find)
           ("C-c n s" . org-roam-db-sync))
    :init
    (setq org-roam-directory (file-truename "~/.org/org-roam")
          org-roam-db-location "~/.org/org-roam.db"
          org-roam-db-gc-threshold most-positive-fixnum
          org-roam-v2-ack t)

    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory t))
    :config
    (org-roam-db-autosync-enable)
    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-direction)
                   (direction . right)
                   (window-width . 0.33)
                   (window-height . fit-window-to-buffer)))))


;;; Working with crontabs

(use-package crontab-mode
  :mode "\\.?cron\\(tab\\)?\\'")


;;; Edit Textile markup

(use-package textile-mode
  :mode "\\.textile\\'")


;;; Markdown support

(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))


;;; Web configurations

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default js-indent-level 2)
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (with-eval-after-load 'js2-mode
    (sanityinc/major-mode-lighter 'js2-mode "JS2")
    (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2")))

;; JSON mode
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :bind (:map json-mode-map
              ("C-c C-f" . json-reformat-region))
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

;; LESS
(use-package skewer-less
  :hook (less-css-mode . skewer-less-mode))


(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))


;;;; Programming languages support

;;; C/C++ Mode

(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :defer t)

;;; Haskell mode

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-f" . ormolu-buffer))
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package dante
  :after (haskell-mode flycheck)
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(use-package reformatter
  :after haskell-mode
  :config
  (reformatter-define hindent
    :program "hindent"
    :lighter " Hin")

  (defalias 'hindent-mode 'hindent-on-save-mode)

  (reformatter-define ormolu
    :program "ormolu"
    :lighter " Orm"))


;;; Python mode
(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun python-doctest-on-current-buffer ()
    "Run doctest on current Python buffer."
    (interactive)
    (shell-command (format "python -m doctest -v %s" (file-name-nondirectory (buffer-file-name))))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (read-only-mode +1)
    (keymap-local-set "q" 'quit-window)))

(use-package pyvenv
  :after python
  :hook (python-mode . pyvenv-mode)
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env (if (eq system-type 'windows-nt)
                                                     "scripts/python"
                                                   "bin/python"))))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))


;;; Support Yaml files

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . goto-address-prog-mode))


;;; Lua mode

(use-package lua-mode
  :mode "\\.lua\\'")


;;; Support for the Nix package manager

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :after nix-mode
  :bind (:map nix-mode-map
              ("C-c C-f" . nixpkgs-fmt)))

;;; Support MSCL mode

(use-package mscl-mode
  :mode "\\.pwx?macro\\'")

;;; Support for the Rust language
(use-package rust-mode
  :mode "\\.rs\\'")


;;; Languages Server Protocol(LSP)

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))


;;; Configure paredit structured editing

(use-package paredit
  :diminish paredit-mode " Par"
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ;; ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ;; ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
             (unbind-key [M-up] paredit-mode-map)
             (unbind-key [M-down] paredit-mode-map)
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;;; Emacs lisp settings, and common config for other lisps

;; Make C-x C-e run 'eval-region if the region is active

(use-package lisp-mode
  :bind (([remap eval-expression] . pp-eval-expression)
         :map emacs-lisp-mode-map
         ("C-x C-e" . sanityinc/eval-last-sexp-or-region)
         ("C-c C-e" . pp-eval-expression)
         ("C-c C-l" . sanityinc/load-this-file))
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "ELisp")))
         (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly))
  :config
  (setq-default debugger-bury-or-kill 'kill)
  (setq-default initial-scratch-message
                (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

  ;; Make C-x C-e run 'eval-region if the region is active
  (defun sanityinc/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))

  (defun sanityinc/make-read-only (expression out-buffer-name)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1))))
  (advice-add 'pp-display-expression :after 'sanityinc/make-read-only)

  ;; C-c C-l to load buffer or file
  (defun sanityinc/load-this-file ()
    "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
    (interactive)
    (let ((load-path (cons default-directory load-path))
          (file (buffer-file-name)))
      (if file
          (progn
            (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
            (load-file (buffer-file-name))
            (message "Loaded %s" file))
        (eval-buffer)
        (message "Evaluated %s" (current-buffer)))))

  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))

  ;;respawn the scratch buffer when it's killed
  (use-package immortal-scratch
    :after lisp-mode
    :hook (after-init . immortal-scratch-mode))

  ;; Extras for theme editing
  (use-package highlight-quoted
    :hook (emacs-lisp-mode . highlight-quoted-mode)))

(use-package rainbow-mode
  :diminish
  :hook ((emacs-lisp-mode . sanityinc/enable-rainbow-mode-if-theme)
         (help-mode . rainbow-mode))
  :preface
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode))))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package ert
  :bind (:map ert-results-mode-map
              ("g" . ert-results-rerun-all-tests)))

(use-package flycheck-package
  :after (flycheck elisp-mode)
  :config
  (flycheck-package-setup))

;; Enable desired features for all lisp modes

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(enable-paredit-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


;;; Spell check settings

(use-package flyspell
  :diminish
  :if (and (executable-find "aspell") *spell-check-support-enabled*)
  ;; Add spell-checking in comments for all programming language modes
  :hook ((prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case")
        ispell-personal-dictionary
        (expand-file-name "en_US.personal" "~/.config/aspell/")))

;; Correcting words with flyspell via completing-read
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

;;; Miscellaneous config

(progn ;; `map-ynp'
  (defalias 'yes-or-no-p #'y-or-n-p))

(use-package goto-addr
  :hook (prog-mode . goto-address-prog-mode)
  :config
  (setq goto-address-mail-face 'link))

(use-package shift-number
  :bind (("C-c +" . shift-number-up)
         ("C-c -" . shift-number-down)))

;; Auto save
(use-package super-save
  :diminish
  :defer 0.5
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (super-save-mode 1))

;;; Support code and region folding

(use-package origami
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes))
  :config
  (origami-mode 1))


;;; Toggle system input method automatically
(use-package sis
  :bind ("C-<f9>" . sis-switch)
  :config
  (when (eq system-type 'windows-nt)
    (sis-ism-lazyman-config "1033" "2052" 'im-select))
  (when *is-a-mac*
    (sis-ism-lazyman-config "com.apple.keylayout.ABC" "com.apple.inputmethod.SCIM.ITABC"))

  (setq sis-other-cursor-color "orange")
  (sis-global-cursor-color-mode t)

  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t))

;;; Dictionaries

(use-package go-translate
  :bind (("C-c t g" . gts-do-translate)
         ("C-c t p" . go-translate-at-point)
         ("C-c t s" . go-translate-save-kill-ring))
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine) (gts-google-engine))
         :render (gts-buffer-render)))

  ;; Pick directly and use Google RPC API to translate
  (defun go-translate-at-point ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-rpc-engine)
                    :render (gts-buffer-render))))

  ;; Pick directly and add the results into kill-ring
  (defun go-translate-save-kill-ring ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-engine
                              :parser (gts-google-summary-parser))
                    :render (gts-kill-ring-render)))))


(progn ; `faces'
  (defvar font-list
    (cond
     ((eq system-type 'darwin)
      '(("SF Mono" . 130) ("Monaco" . 130) ("Menlo" . 130)))
     ((eq system-type 'windows-nt)
      '(("SF Mono" . 110) ("Consolas" . 120) ("Cascadia Mono" . 110)))
     (t
      '(("SF Mono" . 110) ("Consolas" . 120) ("Cascadia Mono" . 110))))
    "List of fonts and sizes.  The first one available will be used.")

  (defun font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))

  (defun change-font ()
    "Set English font from the `font-list'."
    (interactive)
    (let* (available-fonts font-name font-size)
      (dolist (font font-list
                    (setq available-fonts (nreverse available-fonts)))
        (when (font-installed-p (car font))
          (push font available-fonts)))
      (if (not available-fonts)
          (message "No fonts from the chosen set are available")
        (if (called-interactively-p 'interactive)
            (let* ((chosen (assoc-string
                            (completing-read "What font to use? " available-fonts nil t)
                            available-fonts)))
              (setq font-name (car chosen)
                    font-size (read-number "Font size: " (cdr chosen))))
          (setq font-name (caar available-fonts)
                font-size (cdar available-fonts)))
        (set-face-attribute 'default nil :font font-name :height font-size))))

  (when (display-graphic-p)
    (change-font)

    (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Noto Color Emoji"))
      (if (font-installed-p font)
          (set-fontset-font t 'unicode font nil 'prepend)))

    (dolist (font '("Microsoft Yahei" "Hiragino Sans GB" "Noto Sans Mono CJK SC"))
      (if (font-installed-p font)
          (set-fontset-font t '(#x4e00 . #x9fff) font)))))


(use-package dash
  :config (global-dash-fontify-mode 1))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))


;;; Unofficial client for Telegram platform
(when *is-a-mac*
  (use-package telega
    :bind (("C-c t t" . telega)
           ("C-c t f" . telega-buffer-file-send))
    :hook (telega-chat-mode . goto-address-mode)
    :config
    (setq telega-proxies
          (list
           '(:server "127.0.0.1" :port 1080 :enable t
                     :type (:@type "proxyTypeSocks5"))))))


;;; Tequila worms

(progn ; `startup'
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;; personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; Configure default locale

(progn ; `charset'
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8)))


;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
