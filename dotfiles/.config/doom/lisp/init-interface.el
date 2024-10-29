;;; init-interface.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-interface
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'init-evil)

(global-subword-mode 1)     ;; Allow iterating through CamelCase words.
(global-auto-revert-mode 1) ;; Auto-load file changes.

(use-package! helpful
  ;; An alternative to the built-in Emacs help that provides much more contextual information.
  :defer t
  :config
  ;; TODO: Fix, not working.
  ;; Doom centers the page when you visit a soure file, often I can't see the info behind the helpful window.
  ;; Removes the old advice and updates it to display the page at the top of the screen.
  (advice-remove 'helpful--navigate #'+popup--helpful-open-in-origin-window-a)
  (defadvice! my/+popup--helpful-open-in-origin-window-a (button)
    "Open links in non-popup, originating window rather than helpful's window."
    :override #'helpful--navigate
    (let ((path (substring-no-properties (button-get button 'path)))
          enable-local-variables
          origin)
      (save-popups!
       (find-file path)
       (when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
         (goto-char pos))
       (setq origin (selected-window))
       (recenter 3)) ; Added argument 0 to cause recenter to top of screen.
      (select-window origin))))

(when (modulep! :tools lookup)
  ;; I don't use most of Doom's lookup providers, so I'll set my own.
  (setq +lookup-provider-url-alist
        '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
          ("Github" "https://github.com/search?ref=simplesearch&q=%s")
          ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
          ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
          ("StackOverflow" "https://stackoverflow.com/search?q=%s")
          ("Doom Issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
          ("Internet archive" "https://web.archive.org/web/*/%s")
          ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
          ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
          ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s&title=Special%3ASearch&wprov=acrw1")
          ("AUR" "https://aur.archlinux.org/packages?O=0&K=%s"))))

(use-package! avy
  ;; A package for jumping to visible text using a char-based decision tree.
  ;; Quickly jump to anything you see on your screen!
  :defer t
  :config
  (setq avy-all-windows t)) ;; Enable jumping in all visible windows.

;;; -- Completion

(use-package! vertico
  ;; Provides a performant and compatible vertical completion UI for Emacs.
  ;; It's mostly used for completions in the minibuffer.
  :defer t
  :config
  ;; Map scrolling keybinds while in Vertico.
  ;; TODO: Investigate vertico-scroll-up and vertico-scroll-down functions.
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

;; TODO: Add the following:
;; - Persp mode
;; - Marginalia
;; (use-package! marginalia
;; Marginalia adds additional information to the minibuffer completions based on category.
;; :defer t)
;; - Corfu
;; (use-package! corfu
;; Corfu enhances in-buffer completion with a small completion popup.
;; Corfu is the minimalistic in-buffer completion counterpart of the Vertico minibuffer UI.
;; :defer t)
;; - Orderless
;; (use-package! orderless
;; This package provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of the
;; components in any order. Each component can match in any one of several ways.
;; :defer t)
;; (use-package! embark
;; Embark makes it easy to choose a command to run based on what is near point, both
;; during a minibuffer completion session and in normal buffers.
;; :defer t)
;; - Consult
;; (use-package! consult
;; Consult provides search and navigation commands based on the Emacs completion function completing-read.
;; :defer t)

(provide 'init-interface)
;;; init-interface.el ends here
