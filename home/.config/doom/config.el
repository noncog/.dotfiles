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
;; set up ebooks
;; set up anki
;; set up org-gtd of sorts
;; set up org-agenda views and org-capture
;; set up book lists
;; set up clocking of activities.
;; set up email.
;;
;; Experiments
(defun sp-wrap-quote ()
  "Wrap following sexp in quotes."
  (interactive "*")
  (sp-wrap-with-pair "\""))
