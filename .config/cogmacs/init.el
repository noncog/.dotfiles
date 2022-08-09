(require 'package)

;; set sources
(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; output progress
(setq use-package-verbose t)

(defun noncog/split-and-follow-horizontally ()
  "Intuitively splits the window below and focuses it."
  (interactive)
  (split-window-below)
  (other-window 1))
(defun noncog/split-and-follow-vertically ()
  "Intuitively split window right and focuses it."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun noncog/no-delete-windows (oldfun &rest args)
  "A function for use with advice to prevent other functions from using delete-other-window."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore)) (apply oldfun args)))

;; file and a sentinel variables
(setq noncog/brain-file "/home/jake/documents/org/brain.org")
(setq noncog/brain-visible nil)

(defun noncog/toggle-brain ()
  "A function for toggling the view of the your chosen file in a side window."
  (interactive)
  (if noncog/brain-visible
      (progn ;; visible
	(delete-window (get-buffer-window (get-file-buffer noncog/brain-file)))
	(setq noncog/brain-visible nil))
      (progn ;; not visible
	(let ((buffer (find-file-noselect noncog/brain-file)))
	  (let ((display-buffer-mark-dedicated t))
	    (display-buffer-in-side-window buffer '((side . right) (window-width . 60) (no-delete-other-windows . t)))))
	(select-window (get-buffer-window (get-file-buffer noncog/brain-file)))
	(setq noncog/brain-visible t))))

(defun noncog/reload-init-file ()
  "A function to reload Emacs while it's running."
  (interactive)
  (load-file "~/.config/cogmacs/init.el")
  (princ "init.el reloaded."))

(use-package emacs
  :ensure nil
  :config
  (global-auto-revert-mode 1)                         ; autoloads changes to files
  (setq-default load-prefer-newer t)                  ; loads newer versions of packages
  (delete-selection-mode 1)                           ; replaces active region by typing or pasting
  (setq show-parens-delay 0)                          ; remove delay of showing parenthesis
  (show-paren-mode 1)                                 ; show matching parenthesis
  (setq help-window-select t)                         ; auto select help windows when created
  (setq backup-directory-alist                        ; move backup files to folder instead of littering
        '(("." . "~/.config/cogmacs/file-backups")))
  (setq scroll-step 1)                                ; scroll window one line at a time
  (setq scroll-conservatively 101)                    ; value above 100 removes half page jump
  (setq auto-window-vscroll nil)                      ; default to above scroll settings per window, never change them
  (setq scroll-preserve-screen-position 'always)      ; keeps point at position while scrolling
  (dolist (mode '(sh-mode-hook
  		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
  (add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("*Warning*" display-buffer-at-bottom))
  ;; zoom in/out
  (global-set-key (kbd "C-=") 'text-scale-increase)   
  (global-set-key (kbd "C--") 'text-scale-decrease)   
  ;; intuitive window splitting
  (global-set-key (kbd "C-x 2") #'noncog/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'noncog/split-and-follow-vertically)
  ;; custom window focus/move keybinds
  (global-set-key (kbd "C-M-i") 'windmove-up)
  (global-set-key (kbd "C-M-j") 'windmove-left)
  (global-set-key (kbd "C-M-k") 'windmove-down)
  (global-set-key (kbd "C-M-l") 'windmove-right)
  ;; scroll buffer around point
  (global-set-key (kbd "M-p") #'scroll-down-line)
  (global-set-key (kbd "M-n") #'scroll-up-line)
  ;; org 'brain' buffer viewer
  (global-set-key (kbd "C-c t") #'noncog/toggle-brain)
  ;; reload emacs
  (global-set-key (kbd "C-c r") #'noncog/reload-init-file)
  ) ;; end use-package block

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(set-cursor-color "#ff79c6")                          ; set cursor color
(set-face-attribute 'default nil :font "Fira Code")   ; set font
(tool-bar-mode -1)                                    ; disable the toolbar
(menu-bar-mode -1)                                    ; disable the menu bar
(tooltip-mode -1)                                     ; disable tooltips
(scroll-bar-mode 1)                                   ; enable visible scrollbar
(line-number-mode 1)                                  ; enable line number in minibuffer lighter
(column-number-mode 1)                                ; enable column number in minibuffer lighter
(setq visible-bell nil)                               ; disable screen flashing at end of file
(setq inhibit-startup-screen t)                       ; removes startup splash-screen/message

(use-package delight
  :ensure t)

(defun noncog/org-show-current-heading-tidily ()
  (interactive) 
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun noncog/org-heading-size ()
  "set org headings to same size - theme and face independent"
  (dolist (face '(org-level-1
		  org-level-2
		  org-level-3
		  org-level-4
		  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(use-package org
  :ensure t
  :hook
  ((org-mode . visual-line-mode)                      ; enable line wrapping
   (org-mode . org-indent-mode)                       ; enable virtual indents and hide leading stars for readability
   (org-mode . noncog/org-heading-size))              ; enable custom org-heading-size
  :config
  (setq org-ellipsis " ▾")                            ; set custom ellipsis
  (setq org-hide-emphasis-markers t)                  ; hide formatting for markup
  (setq org-edit-src-content-indentation 0)           ; prevent adding spaces/indents to source code blocks
  (setq org-agenda-start-with-log-mode t)             ; show 'completed' done items in agenda
  (setq org-log-done 'time)                           ; add completion time to DONE items.
  (setq org-log-into-drawer t)                        ; puts log times into a drawer to hide them
  (setq org-return-follows-link t)                    ; enter opens links in org
  (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks list
  (setq org-use-fast-todo-selection 'expert)          ; prevent org-todo from modifying windows
  (setq org-directory "~/documents/org")              
  (setq org-agenda-files '("~/documents/org"))        
  (setq org-default-notes-file "~/documents/org/notes.org")
  ;; todo states                                                            ; ! timestamp 
  (setq org-todo-keywords                                                   ; @ note      
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@/!)"))) ; / settings to use when leaving state: ! or @
  ;; org-note
  (advice-add 'org-add-log-note :around #'noncog/no-delete-windows)
  (add-to-list 'display-buffer-alist '("*Org Note*" (display-buffer-below-selected) (window-height . 10))) ; display buffer in this window
  ;; org-capture
  (with-eval-after-load "org-capture" (advice-add 'org-capture-place-template :around 'noncog-no-delete-windows)) ; prevent from hiding other windows
  (add-to-list 'display-buffer-alist '("CAPTURE-.note" (display-buffer-at-bottom) (window-height . 15)))          ; display buffer at bottom
  (add-to-list 'display-buffer-alist '("*Org Select*" (display-buffer-at-bottom) (window-height . 15)))           ; display buffer at bottom
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c C-h") #'noncog/org-show-current-heading-tidily)
  :delight
  (org-indent-mode nil org-indent)                    ; hide indent-mode 
  (visual-line-mode nil emacs))                       ; hide visual-line-mode

(use-package org-bullets
  :ensure t
  :after org                                          ; ensures org-bullets package is loaded after org package
  :hook (org-mode . org-bullets-mode)                 ; start bullets-mode after org-mode
  :config
  (setq org-bullets-bullet-list '("Ͻ" "✖" "➖" "❯" "○" "➡" "➕" "⬤"))
  )

(use-package org-noter
  :ensure t
  :config
  ;; settings
  (setq org-noter-always-create-frame nil)            ; prevent noter from making a new frame
  )

(use-package counsel
  :ensure t
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
  :delight ivy-mode                                   ; hide ivy-mode
  )

(use-package ivy-rich
  :ensure t
  :after ivy                                          ; ensures ivy-rich package is loaded after ivy
  :config
  ;; settings
  (setq ivy-initial-inputs-alist nil)                 ; removes ^ from ivy, means can search any words in commands
  ;; start mode
  (ivy-rich-mode 1)
  )

(use-package ace-window
  :ensure t
  :config
  ;; settings
  (setq aw-background nil)                            ; removes black overlay
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))        ; switch from number keys to home row keys for window-ids
  ;; keybinds
  (global-set-key (kbd "M-o") 'ace-window)
  )

(use-package pulsar
  :ensure t
  :config
  ;; settings
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.095)
  (setq pulsar-iterations 12)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  ;; start mode
  (pulsar-global-mode 1)
  )

(use-package pdf-tools
  :ensure t
  ;; mode keybind
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward)) ; use isearch instead of ivy-search in pdf-mode as it breaks it
  :config
  ;; settings
  (setq-default pdf-view-display-size 'fit-page)      ; fit pdf to window
  )
;; start mode
(pdf-tools-install)                                   ; fails to start inside of :config, put here instead.

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; settings
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-completion-system 'ivy)
  ;; start mode
  (projectile-mode)
  :delight '(:eval (concat " " (projectile-project-name)))) ; use custom projectile minibuffer lighter with project name

(use-package dashboard
  :ensure t
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
  :delight dashboard-mode                             ; hide dashboard-mode
  )

(use-package which-key
  :ensure t
  :config
  ;; settings
  (setq which-key-popup-type 'minibuffer)             ; set window mode
  (setq which-key-idle-delay 0.1) 
  ;; start mode
  (which-key-mode)
  :delight which-key-mode                             ; hide which-key-mode
  )
