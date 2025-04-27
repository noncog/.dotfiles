;;; system-settings.el --- Per-system settings alist library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 noncog
;;
;; Author: noncog
;; Maintainer: noncog
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/system-settings.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A simple library to use an alist to store and retrieve per-system settings.
;;
;; Inspired by David Wilson.
;;
;;; Code:

(defvar system-settings-list nil
  "An associative list holding per-system settings for variables.")

(defun system-settings-get (setting)
  "Return the first value found for `SETTING' in `system-settings-list'."
  (alist-get setting system-settings-list))

(provide 'system-settings)
;;; system-settings.el ends here
