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

;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(set-face-attribute 'default nil :font "Fira Code")

;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package diminish)

(use-package org
  :hook (org-mode . visual-line-mode)
  :diminish org-indent-mode
  :config
  ;; looks
  (setq org-ellipsis " ▾"             ; change ellipsis
        org-hide-emphasis-markers t)  ; hide formatting for markdown
  (setq org-startup-indented t)       ; use indent-mode (I think) in all org-mode

  ;; files and org settings
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))  ; associate .org with org-mode
  (setq org-directory "~/Documents/org")    ; set org files - only used for some interactive prompting to choose an org file when capturing note I think
  (setq org-agenda-files'("~/Documents/org")) ; Direrctory or list of files for org-agenda
  ; maybe set archive location have to figure out how I want to archive..
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  ;; settings
  (setq org-log-done 'time) ; Add completion time to DONE items.
  (setq org-return-follows-link t)) ; enter opens links in org
  ; maybe use org-log-done 'note to require a note on finishing..

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)) ; could try org bullets mode 1

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package projectile
  :init (setq projectile-project-search-path '("~/Projects/"))
  :config (projectile-mode) ;launch mode here
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ; set new frames from emacsclient -c to dashboard
  :config
  (dashboard-setup-startup-hook) ; start dashboard
  (setq dashboard-center-content t    ; center dashboard
        dashboard-set-init-info nil)  ; hide package and load time info
  ; set dashboard items
  (setq dashboard-items '((recents  . 5)
                      (agenda . 5)
                      (projects . 5)
                      (bookmarks . 5)))
  )

(use-package which-key
  :init (which-key-mode) ; turn on which-key
  :diminish which-key-mode)

(use-package dracula-theme)

(load-theme 'dracula t)

;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)
