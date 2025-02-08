;;; init-ui.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-ui
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;; TODO: Add system specific settings.
;;; Code:
(use-package! diff-hl
  :defer t
  :config
  (setq diff-hl-global-modes '(not image-mode pdf-view-mode)))

(use-package! doom-modeline
  :defer t
  :config
  (setq doom-modeline-buffer-state-icon nil))

(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . doom-themes-org-config)
  :init (setq doom-theme 'doom-one))

;; (use-package! doom-ui
;;   :config
;;   ;; TODO: Change to vibrant or dracula.
;;   (setq doom-theme 'doom-one)
;;         ;; ;; - `doom-font' -- the primary font to use
;;         ;; ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;;         ;; ;; - `doom-big-font' -- used for `doom-big-font-mode'; for presentations or streaming.
;;         ;; ;; - `doom-symbol-font' -- for symbols
;;         ;; ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;         ;; ;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;;         ;; ;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;;         ;; ;; refresh your font settings. If Emacs still can't find your font, it likely
;;         ;; ;; wasn't installed correctly. Font issues are rarely Doom issues!
;;         ;; doom-font (font-spec :family "JetBrains Mono" :size 12)
;;         ;; doom-big-font (font-spec :family "JetBrains Mono" :size 14))
;;   )

(use-package! doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :defer t
  :config
  ;; TODO: Investigate these settings.
  (setq doom-modeline-height 20
        doom-modeline-buffer-file-true-name t
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-vcs-max-length 60
        auto-revert-check-vc-info t
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t
        doom-modeline-persp-icon t))

(setq-default x-stretch-cursor t) ;; Draw block cursor as wide as the glyph under it.

(setq display-line-numbers-type 'visual) ;; Use visual line lumbers for relative movement for wrapped lines.


;; (use-package! visual-fill-column
;;   :custom
;;   (visual-fill-column-width 120)
;;   (visual-fill-column-center-text t)
;;   :hook (org-mode . visual-fill-column-mode)
;;   :config
;;   (setq +word-wrap-fill-style 'auto)
;;   (setq +word-wrap-extra-indent nil)
;;   ;; Center margins in frame regardless of text size.
;;   (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))

(provide 'init-ui)
;;; init-ui.el ends here
