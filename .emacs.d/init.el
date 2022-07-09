(setq inhibit-startup-message t      ; removes startup splash
      visible-bell nil               ; disable screen flashing at end of file
      column-number-mode t)          ; display column number 

(tool-bar-mode -1)                   ; disable the toolbar
(menu-bar-mode -1)                   ; disable the menu bar
(tooltip-mode -1)                    ; disable tooltips
(scroll-bar-mode -1)                 ; disable visible scrollbar
;(global-display-line-numbers-mode 1) ; displays line numbers in all files

;; eventually Disable line numbers for some modes here...

(global-auto-revert-mode 1)          ; autoloads changes to files

;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(set-face-attribute 'default nil :font "Fira Code")

;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; set zoom commands
(global-set-key (kbd "C-=") 'text-scale-inAcrease)
(global-set-key (kbd "C--") 'text-scale-decrease)

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
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package delight)

(use-package counsel
  :config
  (ivy-mode 1))
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)

(use-package ivy-rich
  :after ivy     ;not sure if I need or not
  :init
  (ivy-rich-mode 1))

(use-package pdf-tools
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward)) ;uses isearch instead of ivy-search in pdf-mode as it breaks it
  :config
  (setq-default pdf-view-display-size 'fit-page))
(pdf-tools-install)

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  ;; looks
  (setq org-ellipsis " ▾"             ; change ellipsis
        org-hide-emphasis-markers t)  ; hide formatting for markdown
  ;(setq org-startup-indented t)       ; use indent-mode (I think) in all org-mode

  ;; files and org settings
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))  ; associate .org with org-mode
  (setq org-directory "~/Documents/org")    ; set org files - only used for some interactive prompting to choose an org file when capturing note I think
  (setq org-agenda-files'("~/Documents/org/scratchpad.org")) ; Directory or list of files for org-agenda
  ; maybe set archive location have to figure out how I want to archive..
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  ;; settings
  (setq org-log-done 'time) ; Add completion time to DONE items.
  (setq org-return-follows-link t) ; enter opens links in org
  (setq org-capture-bookmark nil)) ;prevent org capture from adding to bookmarks list

  ; maybe use org-log-done 'note to require a note on finishing..

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)) ; could try org bullets mode 1

(use-package org-noter
  :config
  (setq org-noter-always-create-frame nil))  ; prevent noter from making a new frame

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package projectile
  :init (setq projectile-project-search-path '("~/Projects/"))
  :config
  (projectile-mode) ;launch mode here
  (setq projectile-completion-system 'ivy)
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
  :config
  (setq which-key-idle-delay 0.5))

(use-package dracula-theme)

(load-theme 'dracula t)

;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)
