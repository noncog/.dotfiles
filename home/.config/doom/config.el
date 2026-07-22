;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Initial Setup

;; Set user variables.
(when (string= user-login-name "noncog")
  (setq user-full-name "noncog"
        user-mail-address "noncog@github.com"))

;; Extend user variables.
(defconst user-home-directory (getenv "HOME")
  "Filepath of the user's home directory.")

(defconst user-system-name (string-remove-suffix ".local" (system-name))
  "Name of the user's system.")

;; Setup per-system settings using a basic wrapper around alist.
(defvar system-settings-list nil
  "An associative list holding per-system settings for variables.")

(defun system-settings-get (setting)
  "Return the first value found for `SETTING' in `system-settings-list'."
  (alist-get setting system-settings-list))

;; Update load-path to include subdirectories of the Doom config directory.
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Initialize macOS setup.
(when (featurep :system 'macos)
  ;; Unify macOS keybinds (based on current OS keybind settings) with
  ;; standard Linux layout. Keep your Linux muscle memory on a Mac.
  (setq mac-command-modifier 'control ; Maps Command -> Control
        ns-command-modifier 'control
        mac-control-modifier 'meta    ; Maps Control -> Alt (Meta)
        ns-control-modifier 'meta
        mac-option-modifier 'super   ; Maps Option -> Super
        ns-option-modifier 'super)
  ;; Update the default Bash shell to a newer one from Homebrew.
  ;; NOTE: This value is currently hard-coded. Instead, could search for
  ;;       HOMEBREW_PREFIX variable and use the expanded path if it exists.
  (when (file-exists-p "/opt/homebrew/bin/bash")
    (setq shell-file-name "/opt/homebrew/bin/bash"))
  ;; Setup the frame for rounded corners with no title, if available.
  (when (>= emacs-major-version 29)
    (add-to-list 'default-frame-alist '(undecorated-round . t))))

;;; Interface

(use-package doom-ui
  :defer t
  :init
  (setq doom-theme 'modus-vivendi-deuteranopia)
  (setq doom-font (font-spec :family "JetBrains Mono" :size 11.0)
        doom-big-font (font-spec :family "JetBrains Mono" :size 14.0)))

(use-package display-line-numbers
  :defer t
  :config
  (setq display-line-numbers-type 'visual
        display-line-numbers-grow-only t)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h))

(global-subword-mode 1)           ; Enable iterating through camelcase words.
(setq-default x-stretch-cursor t) ; Show cursor (point) as wide as glyph under it.

;; Remap default global keybinds.

;; Move 'toggle' binds from 'help' to 'toggle' menu.
;; Disable frame fullscreen keybinds. Breaks on different window managers.
(map! "C-s-f" nil
      :leader
      "h T" nil
      "t T" #'doom/toggle-profiler
      "h t" nil
      "t t" #'load-theme
      "t F" nil
      "t o" #'doom/set-frame-opacity)

;; Configure Evil

(use-package evil-vars
  :defer t
  :config
  (setq evil-want-fine-undo t          ; Enable granular undo for evil operations.
        evil-split-window-below t      ; Emulate i3 horizontal window splits.
        evil-vsplit-window-right t     ; Emulate i3 vertical window splits.
        evil-kill-on-visual-paste nil) ; Prevent paste from adding to kill ring. Allows multiple pastes.
  (setq-default evil-scroll-count 10)) ; Reduce scroll count instead of scroll by pages.

(use-package evil-collection
  :defer t
  :config
  (setq evil-collection-calendar-want-org-bindings t))

;; Unify internal/external window management.
;; Allows use of same keybinds across window managers and Emacs windows.

(use-package windman
  :config
  (setq windman-command "i3-msg "
        windman-focus-command "focus"
        windman-move-command "move"
        windman-direction-left "left"
        windman-direction-right "right"
        windman-direction-up "up"
        windman-direction-down "down")
  (map!
   "C-s-h" #'windman-resize-win-left
   "C-s-j" #'windman-resize-win-down
   "C-s-k" #'windman-resize-win-up
   "C-s-l" #'windman-resize-win-right
   :map global-map
         "s-h" #'windman-focus-win-left
         "s-j" #'windman-focus-win-down
         "s-k" #'windman-focus-win-up
         "s-l" #'windman-focus-win-right
         "s-H" #'windman-move-win-left
         "s-J" #'windman-move-win-down
         "s-K" #'windman-move-win-up
         "s-L" #'windman-move-win-right
         "s-=" #'balance-windows
         "s-v" #'evil-window-vsplit
         "s-s" #'evil-window-split
         "s-Q" #'evil-quit
   :leader "w h" #'windman-focus-win-left
            "w j" #'windman-focus-win-down
            "w k" #'windman-focus-win-up
            "w l" #'windman-focus-win-right
            "w H" #'windman-move-win-left
            "w J" #'windman-move-win-down
            "w K" #'windman-move-win-up
            "w L" #'windman-move-win-right))

(use-package vertico
  :defer t
  :config
  ;; Add Vim scroll binds to minibuffer.
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

(use-package dired
  :defer t
  :config
  ;; TODO: Investigate how dired handles overwrites.
  ;;       - Where are the backups/
  ;;       - Can we prompt instead?
  (setq dired-backup-overwrite t
        ;; TODO: Investigate if dired-backup-overwrite is loaded in time from dired-aux.
        ;; Move files to trash instead of deleting.
        ;; Dired wronged me with careless keybind use.
        ;; TODO: Perhaps try trashed, for integration into evil and gui.
        ;; See: magit-delete-by-moving-to-trash, evil-collection-trashed-setup
        delete-by-moving-to-trash t
        remote-file-name-inhibit-delete-by-moving-to-trash nil))

(use-package autorevert
  :defer t
  :config
  ;; Enable auto-revert in all buffers to show file changes on disk.
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose t)
  (global-auto-revert-mode 1))

;;; Lookup

;; Add or remove lookup providers I don't agree with or won't use.
;; NOTE: Overwrites any conditional providers added from modules.
(setq +lookup-provider-url-alist
      '(("Doom issues"       "https://github.com/orgs/doomemacs/projects/2/views/30?filterQuery=%s")
        ;;("Doom discourse"    "https://discourse.doomemacs.org/search?q=%s")
        ;;("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
        ;;("Google images"     "https://www.google.com/images?q=%s")
        ;;("Google maps"       "https://maps.google.com/maps?q=%s")
        ("Kagi"              "https://kagi.com/search?q=%s")
        ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
        ("Debian Package" "https://packages.debian.org/search?keywords=%s")
        ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("DevDocs.io"        "https://devdocs.io/#q=%s")
        ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
        ("StackExchange"     "https://stackexchange.com/search?q=%s")
        ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
        ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
        ("Internet archive"  "https://web.archive.org/web/*/%s")
        ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
        ;; FIXME: %3 breaks string expansion for this url/provider.
        ;; ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s&title=Special%3ASearch&wprov=acrw1")
        ("AUR" "https://aur.archlinux.org/packages?O=0&K=%s")))

;;; Project Management

(use-package magit
  :defer t
  :config
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/development/projects" . 1) ; My projects.
          ("~/development/source" . 1))) ; Other's code.
  ;; Load my custom commit linter.
  (require 'magit-lint))

(use-package projectile
  :defer t
  :config
  (setq projectile-auto-discover nil
        projectile-track-known-projects-automatically nil
        projectile-project-search-path
        '(("~/.dotfiles" . 0)
          ("~/development/projects" . 1)  ; My projects.
          ("~/development/source" . 1)))) ; Other's code.


;;; Load "modules".

(load "+notes.el")
(load "+ai.el")

(use-package vterm
  :defer t
  :init
  ;; Update Bash version on macOS if available.
  (when (and (featurep :system 'macos)
             (file-exists-p "/opt/homebrew/bin/bash"))
    (setq vterm-shell "/opt/homebrew/bin/bash"))
  :config
  ;; Keybinds
  ;; - Fix M-backspace keybind on macOS.
  (evil-define-key* 'insert vterm-mode-map (kbd "<M-backspace>") #'vterm-send-meta-backspace))
