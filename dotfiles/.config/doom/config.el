;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Core
(require 'system-settings)
(require 'init-keys)
(require 'init-interface)
(require 'init-ui)
(require 'init-window)
(require 'init-frame)

;; Utilities
(require 'init-project)
(require 'init-vcs)
(require 'init-term)
(require 'init-pdf)

;; Languages
(require 'init-org)
(require 'init-sh)

;; TODO:
;; Mail
;; RSS
;; Ebooks

;; Experiments
(defun sp-wrap-quote ()
  "Wrap following sexp in quotes."
  (interactive "*")
  (sp-wrap-with-pair "\""))
