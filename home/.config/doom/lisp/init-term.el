;;; init-term.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-term
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! vterm
  ;; The most capable Emacs terminal emulator with unrivaled performance and compatibility.
  :defer t
  :init
  ;; I currently prefer Bash over Zsh while on macOS.
  (when (and (featurep :system 'macos)
             (file-exists-p "/opt/homebrew/bin/bash"))
    (setq vterm-shell "/opt/homebrew/bin/bash"))
  :config
  ;; Add M-backspace bind.
  ;; TODO: Check if only a macOS issue.
  (evil-define-key* 'insert vterm-mode-map
    (kbd "<M-backspace>") #'vterm-send-meta-backspace))

(provide 'init-term)
;;; init-term.el ends here
