(setq inhibit-startup-message t      ; removes startup splash
      visible-bell nil               ; disable screen flashing at end of file
      column-number-mode t)          ; display column number 

(tool-bar-mode -1)                   ; disable the toolbar
(menu-bar-mode -1)                   ; disable the menu bar
(tooltip-mode -1)                    ; disable tooltips
(scroll-bar-mode -1)                 ; disable visible scrollbar
(global-display-line-numbers-mode 1) ; displays line numbers in all files      

;; eventually Disable line numbers for some modes here...

(global-auto-revert-mode 1)          ; autoloads changes to files

(set-face-attribute 'default nil :font "Fira Code")

(require 'package)
(setq-default load-prefer-newer t)

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
	("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA" . 5)
	("MELPA" . 1)))

(when (version< emacs-version "27.0") (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package org
  :config
  (setq org-ellipsis " ▾"             ; change ellipsis
	org-hide-emphasis-markers t)  ; hide formatting for markdown

  (setq org-agenda-files
	'("~/Documents/todo.org")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t    ; center dashboard
	dashboard-set-init-info nil)  ; hide package and load time info
  )
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ; set new frames from emacsclient -c to dashboard

(use-package dracula-theme)

(load-theme 'dracula t)

;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)
