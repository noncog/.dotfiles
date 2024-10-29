;;; init-pdf.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-pdf
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! pdf-tools
  ;; A replacement PDF viewer for the default, DocView, in Emacs. It's required for org-noter.
  :defer t
  :config
  ;; Build if necessary.
  ;; TODO: Ensure this is correct.
  (pdf-tools-install t)
  ;; Prevent a crash on macOS.
  (when (featurep :system 'macos)
    (add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode)))

(provide 'init-pdf)
;;; init-pdf.el ends here
