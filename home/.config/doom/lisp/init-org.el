;;; init-org.el --- Configures Org -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/init-org.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; My configuration of Org and related packages.
;;
;;; Code:

(use-package! org
  ;; Org describes itself as, "a GNU Emacs major mode for keeping notes, authoring documents,
  ;; computational notebooks,literate programming, maintaining to-do lists, planning projects, and more
  ;; — in a fast and effective plain text system."
  :defer t
  :init
  ;; Variables
  (setq org-directory (system-settings-get 'notes-directory))
  ;; (add-hook 'org-mode-hook #'+word-wrap-mode)
  :config
  ;; Appearance
  (setq org-hide-emphasis-markers t                     ; Hide syntax for emphasis. (Use org-appear)
        org-pretty-entities t                           ; Show sub/superscript as UTF8.
        org-src-preserve-indentation t                  ;  Don't change whitespace in source blocks.
        org-ellipsis " ▾ "                              ; Use a custom ellipsis for folded headings.
        org-hide-leading-stars t                        ; Remove excess heading stars.
        org-hidden-keywords nil                         ; Can use to hide certain keywords.
        org-image-actual-width '(0.9)                   ; Use an in-buffer width closer to export's
        org-startup-with-inline-images t                ; Show images at startup.
        org-startup-with-latex-preview nil)             ; Show rendered LaTeX in the buffers.
  ;;(require 'init-org-modern)                            ; Load Org Modern for it's appearance customizations.
  ;; Behavior
  ;; - General
  (setq org-treat-insert-todo-heading-as-state-change t ; Adds logging (time created) to new todo headings.
        org-list-allow-alphabetical t                   ; Enable use of alphabet as list bullets.
        org-use-property-inheritance t                  ; Sub-headings inherit parent properties.
        org-return-follows-link t                       ; Pressing enter opens links.
        org-insert-heading-respect-content nil)         ; Insert heading here, not at end of list.
  ;; - Logging
  (setq org-log-done 'time                              ; Add completion time to DONE items.
        org-log-into-drawer t                           ; Log times into a drawer to hide them.
        org-log-reschedule t                            ; Log rescheduling of scheduled items.
        org-log-redeadline t                            ; Log rescheduling of deadline items.
        org-log-refile t                                ; Log when a heading is refiled.
        ;; org-log-repeat t
        org-log-states-order-reversed nil)              ; Log state changes chronologically. TODO: Check
  ;; - Habits
  (add-to-list 'org-modules 'org-habit t)               ; Enable repeated task tracking/graphing!
  ;; - Calendar
  (setq evil-collection-calendar-want-org-bindings t)   ; Enable evil keybinds.
  ;; - Tags
  (setq org-use-tag-inheritance t
        ;; org-tag-alist
        org-tag-persistent-alist '(("article" . ?a)
                                  ("video" . ?v)
                                  ("movie" . ?m)
                                  ("book" . ?b)
                                  ("game" . ?g)
                                  ("post" . ?p)
                                  ("security" . ?s)
                                  ("repo" . ?r)))
  ;; org-todo-keywords
  ;; - Refile
  ;; - Fixes
  ;; - Helpers
  (add-hook! 'org-mode-hook #'doom-disable-line-numbers-h))
  ;; Integrate org-roam and denote into org.
  ;(require 'init-denote)
  ;(require 'init-org-roam))

(use-package! org-appear
  ;; Makes invisible Org elements like emphasis markers magically appear.
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t              ; Show emphasis markup.
        org-appear-autosubmarkers t            ; Show sub/superscript
        org-appear-autoentities t              ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil               ; Shows Org links.
        org-appear-autokeywords nil            ; Shows hidden Org keywords.
        org-appear-inside-latex nil))          ; Show LaTeX code. Use Fragtog instead.

(use-package! org-noter
  :defer t
  :init
  (setq org-noter-notes-search-path (list (expand-file-name "references" org-directory)))
  :config
  (setq org-noter-separate-notes-from-heading t) ; Adds line between headings and notes.
  (setq org-noter-disable-narrowing t)           ; Let me see everything.
  (map! :leader :map (org-noter-doc-mode-map org-noter-notes-mode-map)
        :desc "Kill org noter session" "n k" #'org-noter-kill-session))

(provide 'init-org)
;;; init-org.el ends here
