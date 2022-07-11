(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package emacs
  :ensure nil
  :preface
  ;; intuitive split settings
  (defun noncog/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun noncog/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  ;; basic interface configuration
  (set-face-attribute 'default nil :font "Fira Code") ; set font
  (tool-bar-mode -1)                                  ; disable the toolbar
  (menu-bar-mode -1)                                  ; disable the menu bar
  (tooltip-mode -1)                                   ; disable tooltips
  (scroll-bar-mode -1)                                ; disable visible scrollbar
  (line-number-mode 1)                                ; enable line number in minibuffer lighter
  (column-number-mode 1)                              ; enable column number in minibuffer lighter
  (setq visible-bell nil)                             ; disable screen flashing at end of file
  (setq inhibit-startup-screen t)                     ; removes startup splash-screen/message
  ;; behavior
  (global-auto-revert-mode 1)                         ; autoloads changes to files
  (setq-default load-prefer-newer t)                  ; loads newer versions of packages
  (delete-selection-mode 1)                           ; replaces active region by typing
  (setq show-parens-delay 0)                          ; remove delay of showing parenthesis
  (show-paren-mode 1)                                 ; show matching parenthesis
  ;; smooth scrolling
  (setq scroll-step 1)                                ; scroll window one line at a time
  (setq scroll-conservatively 101)                    ; never recenter point
  (setq auto-window-vscroll nil)                      ; default to above scroll settings per window, never change them
  ;; line numbers for specific modes                  ; to find mode-hooks, when in mode/file do C-h v major-mode RET. Or C-h m.
  (dolist (mode '(sh-mode-hook
		  conf-mode-hook))                      ; all conf-modes inherit this property. no-info on this variable, but works. 
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
  ;; global keybinds
  (global-set-key (kbd "C-=") 'text-scale-increase)   ; set zoom in
  (global-set-key (kbd "C--") 'text-scale-decrease)   ; set zoom out
  ;; custom intuitive split settings
  (global-set-key (kbd "C-x 2") #'noncog/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'noncog/split-and-follow-vertically)
  )

(use-package delight)

(use-package org
  :hook
  ((org-mode . visual-line-mode)                      ; enable line wrapping
   (org-mode . org-indent-mode))                      ; enable virtual indents and hide leading stars for readability
  :config
  ;; looks
  (setq org-ellipsis " ▾")                            ; set custom ellipsis
  (setq org-hide-emphasis-markers t)                  ; hide formatting for markup
  ;; settings
  (setq org-log-done 'time)                           ; add completion time to DONE items.
  (setq org-return-follows-link t)                    ; enter opens links in org
  (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks list
  (setq org-edit-src-content-indentation 0)           ; prevent adding spaces/indents to source code blocks
  ;; directories and files
  (setq org-directory "~/Documents/org")              ; set org file directory - only used for some prompt for capturing
  (setq org-agenda-files'("~/Documents/org"))         ; set org agenda directory or list of files to query
  ;; global keybinds
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  ;; hiding minibuffer lighters
  :delight
  (org-indent-mode nil org-indent)                    ; hide indent-mode 
  (visual-line-mode nil emacs))                       ; hide visual-line-mode

(use-package org-bullets
  :after org                                          ; ensures org-bullets package is loaded after org package
  :hook (org-mode . org-bullets-mode))                ; start bullets-mode after org-mode

(use-package org-noter
  :config
  ;; settings
  (setq org-noter-always-create-frame nil)            ; prevent noter from making a new frame
  )

(use-package counsel
  :config
  ;; settings
  (setq ivy-count-format "(%d/%d) ")
  ; global keybinds
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (global-set-key (kbd "C-c b") 'counsel-bookmark)
  (global-set-key (kbd "C-c d") 'counsel-descbinds)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  ;; start mode
  (ivy-mode 1)
  ;; hiding minibuffer lighters
  :delight ivy-mode                                   ; hide ivy-mode
  )

(use-package ivy-rich
  :after ivy                                          ; ensures ivy-rich package is loaded after ivy
  :config
  ;; start mode
  (ivy-rich-mode 1)
  )

(use-package pdf-tools
  ;; mode keybind
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward)) ; use isearch instead of ivy-search in pdf-mode as it breaks it
  :config
  ;; settings
  (setq-default pdf-view-display-size 'fit-page)      ; fit pdf to window
  )
;; start mode
(pdf-tools-install)                                   ; fails to start inside of :config, put here instead.

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; settings
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-completion-system 'ivy)
  ;; start mode
  (projectile-mode)
  ;; hiding minibuffer lighters
  :delight '(:eval (concat " " (projectile-project-name)))) ; use custom projectile minibuffer lighter with project name

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ; set new frames from emacsclient -c to dashboard
  :config
  ;; settings
  (setq dashboard-center-content t                    ; center dashboard
        dashboard-set-init-info nil)                  ; hide package and load time info
  (setq dashboard-items '((recents  . 5)              ; customize dashboard items
                          (agenda . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  ;; start mode
  (dashboard-setup-startup-hook)
  ;; hiding minibuffer lighters
  :delight dashboard-mode                             ; hide dashboard-mode
  )

(use-package which-key
  :config
  ;; settings
  (setq which-key-idle-delay 0.5)
  ;; start mode
  (which-key-mode)
  ;; hiding minibuffer lighters
  :delight which-key-mode                             ; hide which-key-mode
  )

(use-package dracula-theme
  :config
  (load-theme 'dracula t)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dracula-theme which-key dashboard projectile pdf-tools ivy-rich counsel org-noter org-bullets org delight use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
