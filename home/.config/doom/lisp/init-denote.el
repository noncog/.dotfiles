;;; init-denote.el --- Configures Denote -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/init-denote.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; My configuration of the Denote package.
;;
;; TODO: Add dired/other file type integration.
;; TODO: Make sure this denote-org-front-matter works as DATE is expected to be a keyword.
;;
;;; Code:

(use-package! denote
  ;; Denote is a simple note-taking tool for Emacs with an efficient file naming scheme.
  ;; It's based on the idea that notes should follow a predictable and descriptive file-naming scheme.
  :after org
  :init
  (setq denote-directory org-directory)
  :config
  ;; Integrate org-id and denote's file property format.
  (setq org-id-ts-format denote-id-format
        org-id-method 'ts)
  (setq denote-org-front-matter ":PROPERTIES:\n:ID: %4$s\n:DATE: %2$s\n:END:\n#+title: %1$s\n#+filetags: %3$s\n"))

(provide 'init-denote)
;;; init-denote.el ends here
