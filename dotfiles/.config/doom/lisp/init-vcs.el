;;; init-vcs.el --- Configures Magit -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/init-vcs.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  My configuration of the Magit package.
;;
;;; Code:

(use-package! magit
  ;; A powerful Git interface for Emacs.
  ;; "It fills the glaring gap between the Git command-line interface and various GUIs,
  ;; letting you perform trivial as well as elaborate version control tasks with just
  ;; a couple of mnemonic key presses."
  :defer t
  :init
  ;; TODO: Update to use encryption. Currently not really used or automatically set up.
  (setq auth-sources '("~/.authinfo"))
  :config
  ;; Define directories to search for projects when not already in one.
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/Projects" . 2)
          ("~/Code" . 1)))
  (require 'magit-lint))

(provide 'init-vcs)
;;; init-vcs.el ends here
