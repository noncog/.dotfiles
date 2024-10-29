;;; init-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-evil
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; TODO: Check that these don't need :defer t
(use-package! evil-vars
  :config
  (setq-default evil-scroll-count 10) ;; Use line based scrolling instead full/half pages.
  (setq evil-kill-on-visual-paste nil ;; Prevent paste from adding to kill ring. Allows multiple pastes.
        evil-split-window-below t     ;; Focus windows after splitting.
        evil-vsplit-window-right t
        evil-want-fine-undo t))       ;; Enable granular undo for evil operations.

(use-package! evil-collection
  :config
  (setq evil-collection-calendar-want-org-bindings t))


(provide 'init-evil)
;;; init-evil.el ends here
