;;; init-org-modern.el --- Configures Org Modern -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: December 10, 2024
;; Modified: December 10, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/init-org-modern.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; My configuration of the Org-Modern package.
;;
;;; Code:

(use-package! org-modern
  :after org
  :config

  (setq org-modern-label-border 'auto
        org-modern-star nil
        ;; org-modern-replace-stars
        ;; org-modern-fold-stars
        ;; org-modern-hide-stars
        ;; TODO: Look into custom time strings for this.
        org-modern-timestamp t
        ;; TODO: Consider disabling
        org-modern-table nil
        ;; org-modern-table-vertical
        ;; org-modern-table-horizontal
        org-modern-priority nil
        ;; TODO: Look into
        org-modern-list nil
        org-modern-checkbox nil
        org-modern-horizontal-rule nil
        org-modern-todo t
        ;; org-modern-todo-faces
        ;; org-modern-tag-faces
        ;; org-modern-priority-faces
        org-modern-tag t
        org-modern-block-name t
        org-modern-block-fringe nil
        ;; TODO: Look into
        org-modern-keyword t
        org-modern-footnote nil
        org-modern-internal-target t
        org-modern-radio-target t
        ;; TODO: Look into
        org-modern-progress nil)
  ;; TODO: Consider disabling for agenda mode.
  ;; Enable the org-modern:
  ;; Option 1: Per buffer
  ;; (add-hook 'org-mode-hook #'org-modern-mode)
  ;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  ;; Option 2: Globally
  (global-org-modern-mode 1))


(provide 'init-org-modern)
;;; init-org-modern.el ends here
