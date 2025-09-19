;;; +org.el --- My personal org-mode configuration -*- lexical-binding: t; -*-

;; TODO: Add org-protocol setup functions.

(use-package! evil-collection
  :defer t
  :config
  (setq evil-collection-calendar-want-org-bindings t))

;; TODO: Add linking to info pages.
;; TODO: Update title and heading sizes.
;; TODO: See if logbook can be formatted better.
(use-package! org
  :defer t
  :init
  (setq org-directory (expand-file-name "~/documents/org/"))
  (defvar org-inbox-directory (expand-file-name "inbox/" org-directory)
    "A directory used to hold `org-capture' items.")
  (defvar org-inbox-file (expand-file-name (concat user-system-name ".org") org-inbox-directory)
    "Inbox file to use with `org-capture'. Uses system name to avoid sync conflicts.")
  :config
  (custom-set-faces! '(org-document-title :height 1.0))
  (add-to-list 'org-modules 'org-habit t)               ; Enable org-habit for tracking repeated actions.
  (add-to-list 'org-modules 'ol-man t)                  ; Enable links to man pages.
  (add-to-list 'org-modules 'ol-info t)                 ; Enable links to info pages.
  (setq org-default-notes-file org-inbox-file           ; Set default notes file to inbox file.
        org-hide-leading-stars t                        ; Hide leading heading stars.
        org-ellipsis " ▾ "                              ; Use UTF-8 to indicate a folded heading.
        org-hidden-keywords nil                         ; Don't hide any TODO keywords.
        org-image-actual-width '(0.9)                   ; Use an in-buffer image width closer to export's
        org-startup-with-inline-images t                ; Show images at startup.
        org-startup-with-latex-preview nil              ; Don't show LaTeX on startup.
        org-hide-emphasis-markers t                     ; Hide syntax for emphasis. (Use org-appear)
        org-src-preserve-indentation t                  ; Keep language specific indenting in source blocks.
        org-pretty-entities t                           ; Show sub/superscript as UTF8.
        org-property-format "%-10s %s"                  ; How property key/value pairs is formatted by `org-indent-line'.
        org-list-allow-alphabetical t                   ; Allow alphabet as lists.
        org-use-property-inheritance t                  ; Sub-headings inherit parent properties.
        org-imenu-depth 5                               ; Allow imenu to search deeply in org docs.
        org-return-follows-link t                       ; Allow return to open links.
        org-insert-heading-respect-content nil          ; Insert heading here, not at end of list.
        org-use-fast-todo-selection 'auto               ; Method to select TODO heading keywords.
        org-log-done 'time                              ; Add completion time to DONE items.
        org-log-into-drawer t                           ; Log times into a drawer to hide them.
        org-log-reschedule t                            ; Log rescheduling of scheduled items.
        org-log-redeadline t                            ; Log rescheduling of deadline items.
        org-log-states-order-reversed nil               ; Log times reverse chronologically.
        org-treat-insert-todo-heading-as-state-change t ; Enable logging on todo state change.
        ;; Add seconds to timestamps for ISO 8601 full compibility.
        ;; May produce errors in org parser, although claims to use ISO 8601.
        org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M:%S")
        org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)"))))

;; TODO: Ensure org-ts-regexp works for ISO-8601 with seconds included.
(use-package! org-id
  :defer t
  :config
  (setq org-id-locations-file (expand-file-name "data/org-ids" org-directory)
        org-id-track-globally t                         ; Track identifiers in all org files so id links always work.
        org-id-locations-file-relative t                ; Use relative references for cross-platform compatibility.
        org-id-method 'ts                               ; ISO-8601 timestamp format for identifiers.
        org-id-ts-format "%Y%m%dT%H%M%S"))

(use-package! org-file
  :after org
  :config
  (setq org-file-tag-agenda "agenda"
        org-file-agenda-tags '("refile")
        org-file-agenda-keywords '("TODO"))
  (add-to-list 'org-tags-exclude-from-inheritance "agenda"))

(use-package! org-refile
  :defer t
  :config
  (setq org-outline-path-complete-in-steps nil          ; TODO
        org-refile-use-outline-path 'file               ; TODO
        org-log-refile t                                ; Log when a heading is refiled.
        org-refile-allow-creating-parent-nodes 'confirm ; TODO
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-primary-file :maxlevel . 5)
                             (org-agenda-files :maxlevel . 3))))

;; TODO: Consider using org-todo with finalizer for logbook timestamps.
(use-package! org-capture
  :defer t
  :config
  ;; Declare helper functions.
  ;; NOTE: Does not work with multi-level heading captures.
  (defun org-capture-add-created-property ()
    "Add Create an ID and CREATED property for the current entry.
Intended for use with `:before-finalize' keyword in `org-capture-templates'."
    (when org-capture-mode
      (org-entry-put (point) "CREATED" (format-time-string org-id-ts-format))))
  ;; Configure package.
  (setq org-capture-templates-contexts nil              ; TODO
        org-capture-templates
        '(("t" "Task" entry
           (file org-inbox-file)
           "* TODO %?"
           :prepend t
           :before-finalize (org-capture-add-created-property)
           :empty-lines-before 1)
          ("n" "Note" entry
           (file org-inbox-file)
           "* NOTE %?"
           :prepend t
           :before-finalize (org-capture-add-created-property)
           :empty-lines-before 1))))

(load "+org-roam.el")

(use-package! org-habit
  :defer t
  :config
  (setq org-habit-show-habits-only-for-today t ; Only show habits in one section.
        ;; +org-habit-min-width                ; TODO
        ;; +org-habit-graph-padding            ; TODO
        ;; +org-habit-graph-window-ratio       ; TODO
        ;; org-habit-graph-column              ; TODO
        ;; org-habit-today-glyph               ; TODO
        ;; org-habit-completed-glyph           ; TODO
        ;; org-habit-show-done-always-green    ; TODO
        org-habit-show-all-today t))           ; Keep habits visible even if done.

;; TODO: Define org-roam-agenda-category
(use-package! org-agenda
  :defer t
  :config
  ;; Appearance
  (custom-set-faces!
    '(org-agenda-structure
      :height 1.3 :weight bold))
  (setq org-agenda-tags-column (+ 10 (* -1 70))         ; Attempt to format agenda width.
        org-agenda-start-with-log-mode t                ; Show 'completed' items in agenda.
        org-agenda-prefix-format
        '((agenda . " %i %-12(org-roam-agenda-category)%?-12t% s")
          (todo . " %i %-12(org-roam-agenda-category) ")
          (tags . " %i %-12(org-roam-agenda-category) ")
          (search . " %i %-12(org-roam-agenda-category) ")))
  ;; TODO: Possibly extend this for named agendas to appear in side window.
  (set-popup-rule! "^\\*Org Agenda\\*" :side 'right :vslot 1 :width 60 :modeline nil :select t :quit nil)
  (load "+org-agenda.el")
  (defun my/org-agenda ()
    "Open personal org-agenda template."
    (interactive)
    (org-agenda nil "o"))
  (map! :leader :desc "Org Agenda" "o a o" #'my/org-agenda))

;;; Extra Packages

(use-package! org-ql
  :defer t
  :after org)

(use-package! org-transclusion
  :after org)

(use-package! org-ql-search
  :defer t
  :after org
  :autoload org-dblock-write:org-ql)

(use-package! org-sidebar
  :after org
  :config
  (setq org-sidebar-side 'left
        org-sidebar-default-fns 'org-sidebar-tree-view-buffer
        org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source))

;;; Hacks

(load "+toc-org.el")
