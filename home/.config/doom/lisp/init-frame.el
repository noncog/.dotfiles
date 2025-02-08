;;; init-frame.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-frame
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(when (featurep :system 'macos)
  (progn
    ;; Start Emacs with maximized frame instead of floating window.
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (when (>= emacs-major-version 29)
      ;; On macOS when using Emacs 29+, use rounded window corners with no title bar.
      (add-to-list 'default-frame-alist '(undecorated-round . t))
      ;; On macOS with Yabai, `doom-big-font-mode' caused the frame to resize and lose window management.
      ;; TODO: Consider checking for the presence of Yabai.
      (setq frame-inhibit-implied-resize '(font font-backend tab-bar-lines)))))


(provide 'init-frame)
;;; init-frame.el ends here
