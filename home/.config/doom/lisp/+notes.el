;;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :init
  (setq org-directory "~/documents/org/")
  ;; Define additional core variables.
  (defvar org-data-directory (expand-file-name "data/" org-directory)
    "A directory used to hold data files related to org.")
  ;; (defvar org-inbox-directory (expand-file-name "inbox/" org-directory)
  ;;   "A directory used to hold `org-capture' items.")
  (defvar org-inbox-file (expand-file-name "inbox.org" org-directory)
    "Inbox file to use with `org-capture'.")
  :config
  ;; Modules
  (add-to-list 'org-modules 'org-habit t)               ; Enable org-habit for tracking repeated actions.
  (add-to-list 'org-modules 'ol-man t)                  ; Enable links to man pages.
  (add-to-list 'org-modules 'ol-info t)                 ; Enable links to info pages.
  ;; Appearance
  (setq org-hide-leading-stars t                        ; Hide leading heading stars.
        org-ellipsis " ▾ ")                             ; Use UTF-8 to indicate a folded heading.
  ;; Task Management
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")))
  ;; Logging
  (setq org-log-into-drawer t                           ; Log times into a drawer to hide them.
        org-log-reschedule t                            ; Log rescheduling of scheduled items.
        org-log-redeadline t                            ; Log rescheduling of deadline items.
        org-log-states-order-reversed nil               ; Log times reverse chronologically.
        org-treat-insert-todo-heading-as-state-change nil ; Enable logging on `org-insert-todo-heading'.
        org-log-done 'time))                            ; Add completion time to DONE items.

(use-package org-id
  :defer t
  :config
  (defun +org-add-id-to-new-todo-headings ()
    "Add an org-id to a heading when it becomes a TODO heading for the first time."
    (when (and org-state (not (member org-state org-done-keywords)))
      (org-id-get-create)))
  (add-hook 'org-after-todo-state-change-hook #'+org-add-id-to-new-todo-headings)
  (setq org-id-locations-file (expand-file-name "org.id" org-data-directory)
        org-id-locations-file-relative t                ; Use relative references for cross-platform compatibility.
        org-id-track-globally t                         ; Track identifiers in all org files so id links always work.
        org-id-method 'ts                               ; Use timestamps for unique identifiers.
        org-id-ts-format "%Y%m%dT%H%M%S"))              ; ISO-8601 timestamp format for identifiers.


;; TODO: Setup fref to replace Denote.
(use-package! denote
  :defer t
  :after org
  :config
  ;; Prevent default configuration from creating directories.
  (setq denote-directory org-directory
        denote-dired-directories org-directory
        denote-org-front-matter
        ":PROPERTIES:\n:ID: %4$s\n:DATE: %2$s\n:END:\n#+title: %1$s\n#+filetags: %3$s\n"))

(use-package org-file
  :after org
  :config
  (defun my/org-file-rename-fn (filename title filetags id)
    "A function to rename org files."
    (ignore title filetags id)
    (when (functionp #'denote-rename-file-using-front-matter)
      (denote-rename-file-using-front-matter filename)))
  (setq org-file-tag-agenda "agenda"                    ; Tag added to mark a file as an org-agenda file.
        org-file-agenda-tags '("refile")                ; Tags a heading can have marking it for the agenda.
        org-file-update-agenda t                        ; Update agenda files on save.
        org-file-agenda-keywords '("TODO")              ; List of keywords considered for the agenda.
        org-file-rename-fn #'my/org-file-rename-fn)     ; Function for renaming org files.
  (add-to-list 'org-tags-exclude-from-inheritance "agenda"))

;; TODO: Fix issue with async nature db update and immediate org-file agenda update.
;; NOTE: Would need hook on vulpea-db-sync--message for final summary complete message.
(use-package vulpea
  :hook ((after-init . vulpea-db-autosync-mode))
  :init
  (setq-default vulpea-db-sync-directories (list org-directory)
        vulpea-db-location (expand-file-name "vulpea.db" org-data-directory)
        ;; FIXME: If directory does not exist, it won't create the file.
        vulpea-default-notes-directory (expand-file-name "notes/" org-directory)
        vulpea-db-index-heading-level t                 ; Index heading level notes.
        vulpea-db-exclude-archived t                    ; Prevent archived entries from polluting the database.
        vulpea-db-sync-scan-on-enable 'async            ; Automatically scan on enable.
        vulpea-db-exclude-property "IGNORE")            ; Don't index nodes with this property.
  :config
  ;; Agenda update.
  (defcustom vulpea-agenda-files-filter nil
    "Predicate keeping agenda notes, or nil to keep all.

Called with a `vulpea-note'; return non-nil to keep it.  Use it to hold
some notes (for example a cemetery) out of the agenda file list."
    :type '(choice (const :tag "Keep all" nil)
            (function :tag "Predicate"))
    :group 'vulpea-para)

  (defun vulpea-agenda-files ()
  "Return the file paths of notes tagged for the agenda.

These are the files that currently hold open work, which are the only
files `org-agenda' needs to scan.  When `vulpea-agenda-files-filter'
is set, notes it rejects are left out."
  (let ((notes (vulpea-db-query-by-tags-some (list org-file-tag-agenda))))
    (when vulpea-agenda-files-filter
      (setq notes (seq-filter vulpea-agenda-files-filter notes)))
    (seq-uniq (mapcar #'vulpea-note-path notes))))

  (setq org-file-agenda-files-fn #'vulpea-agenda-files)
  (org-file-update-mode 1)

  ;; Configure file templates.
  (defun my/vulpea-fix-slug (slug)
    "A function to format SLUG with dashes instead of underscores."
    (string-replace "_" "-" slug))
  (advice-add 'vulpea-title-to-slug :filter-return #'my/vulpea-fix-slug)
  (setq vulpea-buffer-alias-property "ALIASES"
        vulpea-create-default-template '(:file-name "${id}--${slug}.org" :head "#+created: %<[%Y-%m-%d]>")))

(use-package vulpea-ui
  :after vulpea)

(use-package vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (vulpea-journal-setup))


;; Setup "notes" keybinds. From: ~/.config/emacs/modules/config/default/+evil-bindings.el
(map! :leader
      (:prefix-map ("n" . "notes")
       :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
       :desc "Org agenda"                   "a" #'org-agenda
       (:when (modulep! :tools biblio)
         :desc "Bibliographic notes"        "b"
         (cond ((modulep! :completion vertico)  #'citar-open-notes)
               ((modulep! :completion ivy)      #'ivy-bibtex)
               ((modulep! :completion helm)     #'helm-bibtex)))
       :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"     "C" #'org-clock-cancel
       :desc "Notes directory"              "d" #'+default/browse-notes
       (:when (modulep! :lang org +noter)
         :desc "Org noter"                   "e" #'org-noter)
       :desc "Find file in notes"           "f" #'+default/find-in-notes
       :desc "Browse notes"                 "F" #'+default/browse-notes
       :desc "Org store link"               "l" #'org-store-link
       :desc "Tags search"                  "m" #'org-tags-view
       :desc "Org capture"                  "n" #'org-capture
       :desc "Goto capture"                 "N" #'org-capture-goto-target
       :desc "Active org-clock"             "o" #'org-clock-goto
       :desc "Todo list"                    "t" #'org-todo-list
       :desc "Search notes"                 "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
       :desc "View search"                  "v" #'org-search-view
       :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
       :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
       (:prefix ("r" . "roam")
        ;; :desc "Open random node"           "a" #'org-roam-node-random
        :desc "Find note"                  "f" #'vulpea-find
        ;; :desc "Find ref"                   "F" #'org-roam-ref-find
        ;; :desc "Show graph"                 "g" #'org-roam-graph
        :desc "Insert note"                "i" #'vulpea-insert
        ;; :desc "Capture to node"            "n" #'org-roam-capture
        ;; :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
        ;; :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
        ;; :desc "Sync database"              "s" #'org-roam-db-sync
        ;; (:prefix ("d" . "by date")
        ;;  :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
        ;;  :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
        ;;  :desc "Capture date"              "D" #'org-roam-dailies-capture-date
        ;;  :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
        ;;  :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
        ;;  :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
        ;;  :desc "Capture today"             "n" #'org-roam-dailies-capture-today
        ;;  :desc "Goto today"                "t" #'org-roam-dailies-goto-today
        ;;  :desc "Capture today"             "T" #'org-roam-dailies-capture-today
        ;;  :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
        ;;  :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
        ;;  :desc "Find directory"            "-" #'org-roam-dailies-find-directory)
        )
       (:when (modulep! :lang org +journal)
         (:prefix ("j" . "journal")
          :desc "New Entry"           "j" #'org-journal-new-entry
          :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
          :desc "Search Forever"      "s" #'org-journal-search-forever))))

(use-package org-noter
  :defer t
  :config
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil))

(defun my/insert-current-time ()
  "Insert current time string according to org-id-ts-format."
  (interactive)
  (insert (format-time-string org-id-ts-format)))
