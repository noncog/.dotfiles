;;; init-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-window
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! wm
  ;; This is a personal package in development integrating Emacs and window managers.
  ;; See: ./lisp/wm.el
  :config
  ;; I use super as my window manager modifier key.
  ;; These binds allow the window manager windowing keybinds to work within Emacs.
  (map! "s-h" #'wm-focus-win-left
        "s-j" #'wm-focus-win-down
        "s-k" #'wm-focus-win-up
        "s-l" #'wm-focus-win-right
        "s-H" #'wm-move-win-left
        "s-J" #'wm-move-win-down
        "s-K" #'wm-move-win-up
        "s-L" #'wm-move-win-right
        "s-=" #'balance-windows
        "s-v" #'evil-window-vsplit
        "s-s" #'evil-window-split
        "s-Q" #'evil-quit)
  ;; These binds allow you to use Doom's window management keybinds from Emacs to windows outside of it.
  (map! :leader "w h" #'wm-focus-win-left
        "w j" #'wm-focus-win-down
        "w k" #'wm-focus-win-up
        "w l" #'wm-focus-win-right
        "w H" #'wm-move-win-left
        "w J" #'wm-move-win-down
        "w K" #'wm-move-win-up
        "w L" #'wm-move-win-right)
  ;; These binds allow you to resize windows directionally.
  ;; These echo the window manager functionality but are not fully integrated yet.
  (map! "C-s-h" #'wm-resize-win-left
        "C-s-j" #'wm-resize-win-down
        "C-s-k" #'wm-resize-win-up
        "C-s-l" #'wm-resize-win-right))

(provide 'init-window)
;;; init-window.el ends here
