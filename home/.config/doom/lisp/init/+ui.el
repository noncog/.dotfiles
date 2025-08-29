;;; +ui.el --- My personal ui configuration -*- lexical-binding: t; -*-

;; Initialize global behavior.

;; NOTE From autorevert.el, more settings available.
(global-auto-revert-mode 1)       ; Revert buffer to show file changes on disk.
(global-subword-mode 1)           ; Enable iterating through camelcase words.
(setq-default x-stretch-cursor t) ; Show cursor (point) as wide as glyph under it.
;;; Navigation

(setq +lookup-provider-url-alist
      '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("Debian Package" "https://packages.debian.org/search?keywords=%s")
        ("Github" "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("StackOverflow" "https://stackoverflow.com/search?q=%s")
        ("Doom Issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
        ("Internet archive" "https://web.archive.org/web/*/%s")
        ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
        ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
        ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s&title=Special%3ASearch&wprov=acrw1")
        ("AUR" "https://aur.archlinux.org/packages?O=0&K=%s")))

(use-package! vertico
  :defer t
  :config
  ;; Add Vim scroll binds to minibuffer.
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

(use-package! avy
  :defer t
  :config
  (setq avy-all-windows t))

;; TODO Verify all.
(use-package! evil-vars
  :defer t
  :config
  (setq evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-kill-on-visual-paste nil) ; TODO Add note.
  (setq-default evil-scroll-count 10)) ; Reduce scroll count instead of scroll by pages.

;; Unify internal and external window management keybinds.
(use-package! windman
  :config
  (setq windman-command "i3-msg "
        windman-focus-command "focus"
        windman-move-command "move"
        windman-direction-left "left"
        windman-direction-right "right"
        windman-direction-up "up"
        windman-direction-down "down")
  ;; I use super as my window manager modifier key.
  ;; These binds allow the window manager windowing keybinds to work within Emacs.
  (map!
   "C-s-h" #'windman-resize-win-left
   "C-s-j" #'windman-resize-win-down
   "C-s-k" #'windman-resize-win-up
   "C-s-l" #'windman-resize-win-right
   (:map global-map
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
         "s-Q" #'evil-quit)
   (:leader "w h" #'windman-focus-win-left
            "w j" #'windman-focus-win-down
            "w k" #'windman-focus-win-up
            "w l" #'windman-focus-win-right
            "w H" #'windman-move-win-left
            "w J" #'windman-move-win-down
            "w K" #'windman-move-win-up
            "w L" #'windman-move-win-right)))

;; Remap some global keybinds I don't agree with.
;; TODO:
;; - [ ] Test "t F"
;; - [ ] Test "C-s-f"
;; - [ ] Test "t o"
;; - [ ] Comment about fullscreen toggling bind.
(map! "C-s-f" nil
      :leader
      "h T" nil
      "t T" #'doom/toggle-profiler
      "h t" nil
      "t t" #'load-theme
      "t F" nil
      "t o" #'doom/set-frame-opacity)

;; Initialize appearance.

(use-package! doom-ui
  :config
  (setq doom-theme 'doom-one)
  ;; Avoid setting font on macOS until a performant one can be found.
  (when (and (doom-font-exists-p "JetBrains Mono")
             (featurep :system 'linux))
    (setq doom-font (font-spec :family "JetBrains Mono" :size 15.0)
          doom-big-font (font-spec :family "JetBrains Mono" :size 16.0))))

(use-package! doom-modeline
  :defer t
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon t
        doom-modeline-buffer-file-true-name t
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon nil
        doom-modeline-vcs-max-length 60
        auto-revert-check-vc-info t))

(use-package! display-line-numbers
  :defer t
  :config
  (setq display-line-numbers-type 'visual
        display-line-numbers-grow-only t)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h))
