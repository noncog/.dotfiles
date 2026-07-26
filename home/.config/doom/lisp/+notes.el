;;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :init
  (setq org-directory "~/documents/org/")
  ;; Define additional core variables.
  (defvar org-data-directory (expand-file-name "data/" org-directory)
    "A directory used to hold data files related to org.")
  (defvar org-inbox-directory (expand-file-name "inbox/" org-directory) ;; NOTE: Unused currently.
    "A directory used to hold `org-capture' items.")
  (defvar org-inbox-file (expand-file-name "inbox.org" org-directory)
    "Inbox file to use with `org-capture'.")
  :config
  (setq org-default-notes-file org-inbox-file)  ; Set default notes file to inbox file.
  ;; Modules
  (add-to-list 'org-modules 'org-habit t)       ; Enable org-habit for tracking repeated actions.
  (add-to-list 'org-modules 'ol-man t)          ; Enable links to man pages.
  (add-to-list 'org-modules 'ol-info t)         ; Enable links to info pages.
  ;; Appearance
  (setq org-hide-leading-stars t                ; Hide leading heading stars.
        org-ellipsis " ▾ "                      ; Use UTF-8 to indicate a folded heading.
        org-hidden-keywords nil                 ; Don't hide any TODO keywords.
        org-image-actual-width '(0.9)           ; Use an in-buffer image width closer to export's
        org-startup-with-inline-images t        ; Show images at startup.
        org-startup-with-latex-preview nil      ; Don't show LaTeX on startup.
        org-hide-emphasis-markers t             ; Hide syntax for emphasis. (Use org-appear)
        org-src-preserve-indentation t          ; Keep language specific indenting in source blocks.
        org-pretty-entities t)                  ; Show sub/superscript as UTF8.
  (setq org-property-format "%-10s %s")         ; TODO: Investigate if works or broken.
  ;; General Behavior
  (setq org-list-allow-alphabetical t           ; Use alphabet as lists.
        org-use-property-inheritance t          ; Sub-headings inherit parent properties.
        org-imenu-depth 6                       ; Allow imenu to search deeply in org docs.
        org-return-follows-link t               ; Allow return to open links.
        org-insert-heading-respect-content nil  ; Insert heading here, not at end of list. TODO: Investigate
        org-use-fast-todo-selection 'auto)      ; Method to select TODO heading keywords.
  ;; org-allow-promoting-top-level-subtree ; TODO: Investigate
  ;; Task Management
  (setq org-todo-keywords                       ; Only use sequence to denote state that
        '((sequence "TODO(t)" "|" "DONE(d!)"))  ; requires progress. Either do or done.
        org-todo-keyword-faces                  ; Control the colors of todo keywords.
        '(("TODO" . +org-todo-active)           ; - Removes Doom's opinionated defaults
          ("DONE" . +org-todo-cancel)))         ;   leftover.
  ;; Logging
  (setq org-log-into-drawer t                   ; Log times into a drawer to hide them.
        org-log-reschedule t                    ; Log rescheduling of scheduled items.
        org-log-redeadline t                    ; Log rescheduling of deadline items.
        org-log-states-order-reversed nil       ; Log times reverse chronologically.
        org-treat-insert-todo-heading-as-state-change nil
        org-log-done 'time))                            ; Add completion time to DONE items.

(use-package org-id
  :defer t
  :config
  ;; Helpers
  (defun +org-insert-id ()
    "Insert an org-id at point for current time formatted by `org-id-ts-format'."
    (interactive)
    (insert (format-time-string org-id-ts-format)))

  ;; Automatically assign new TODO headings an org-id.
  (defun +org-add-id-to-new-todo-headings ()
    "Add an org-id to a heading when it becomes a TODO heading for the first time."
    (when (and org-state (not (member org-state org-done-keywords)))
      (org-id-get-create)))
  (add-hook 'org-after-todo-state-change-hook #'+org-add-id-to-new-todo-headings)

  ;; Configure
  (setq org-id-locations-file (expand-file-name "org.id" org-data-directory)
        org-id-link-to-org-use-id t                     ; Storing a link to a file uses the org-id.
        org-id-locations-file-relative t                ; Use relative references for cross-platform compatibility.
        org-id-track-globally t                         ; Track identifiers in all org files so id links always work.
        org-id-method 'ts                               ; Use timestamps for unique identifiers.
        org-id-ts-format "%Y%m%dT%H%M%S"))              ; ISO-8601 timestamp format for identifiers.

;; TODO: Setup fref to replace Denote.
(use-package denote
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
  ;; TODO: Fix denote leaving fake renamed buffers around when say no.
  (defun my/org-file-rename-fn (filename title filetags id)
    "A function to rename org files."
    (ignore title filetags id)
    (when (and (equal vulpea-default-notes-directory
                      (file-name-parent-directory filename))
               (functionp #'denote-rename-file-using-front-matter))
      (denote-rename-file-using-front-matter filename)))
  (setq org-file-tag-agenda "agenda"                    ; Tag added to mark a file as an org-agenda file.
        org-file-agenda-tags '("refile")                ; Tags a heading can have marking it for the agenda.
        org-file-update-agenda t                        ; Update agenda files on save.
        org-file-agenda-keywords '("TODO")              ; List of keywords considered for the agenda.
        org-file-rename-fn #'my/org-file-rename-fn)     ; Function for renaming org files.
  (add-to-list 'org-tags-exclude-from-inheritance "agenda"))

;; TODO: Fix disparity between db async updates and org-file save-hook execution.
;;       - The save hook executes faster than the database update and immediately
;;         sets the `org-agenda-files' variables from the database before the db
;;         had updated its value to be included.
;;       - Would require a hook ran somewhere around `vulpea-db-sync--message'.
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
  ;; TODO: Fix this not activating correctly and setting variable to match..
  (org-file-update-mode 1)

  ;; Configure file templates.
  (defun my/vulpea-fix-slug (slug)
    "A function to format SLUG with dashes instead of underscores."
    (string-replace "_" "-" slug))

  (advice-add 'vulpea-title-to-slug :filter-return #'my/vulpea-fix-slug)

  (setq vulpea-buffer-alias-property "ALIASES"
        vulpea-create-default-template '(:file-name "${id}--${slug}.org" :head "#+created: %<[%Y-%m-%d]>"))

  ;; TODO: Add tags sorting functions.
  ;; TODO: Extend to support more tagging uses.
  (defvar vulpea-person-tag "person"
    "A filetag that marks a note as a person.")

  (defvar vulpea-tag-person-on-linked-task t
    "Add a `@PersonName' tag to a task when linking their vulpea note.

Used by `vulpea-insert-update-tags-h' to automatically add a `person'
tag of the form `@PersonName' when linking to their note under a task
heading. The tag format is automatically generated from the #title of the
linked note if it has the `person' tag.")

  (defun vulpea-insert-update-tags-h (note)
    "Update parent node tags when an vulpea node is inserted as an Org link.

Recieves the inserted note from `vulpea-insert-handle-functions' to
update the parent node tags according to the value of yet to be implemented
options.

Currently only supports adding a `person' tag of the form `@PersonName'
generated from the title of a node that has the `person' tag."
    (let ((filetags (vulpea-tags note)))
      (when (and vulpea-tag-person-on-linked-task
                 (seq-contains-p filetags vulpea-person-tag))
        (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (when (eq 'todo (org-element-property
                             :todo-type
                             (org-element-at-point)))
              (org-set-tags
               (seq-uniq (cons (concat "@" (s-replace " " "" (vulpea-note-title note))) (org-get-tags nil t))))))))))

  (add-hook 'vulpea-insert-handle-functions #'vulpea-insert-update-tags-h)

  ;; TODO: Setup vulpea-db-index-heading-level function that excludes file paths.
  ;; called by: vulpea-db--should-index-headings-p
  ;; vulpea-db--extract-heading-nodes
  ;; Use it to ignore bookmarks?

  ;; TODO: Review this: simply sets up basic filtering, setup better rules.
  (defun my/vulpea-find-candidates (&optional filter)
    "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
    (let ((notes (vulpea-db-query-by-tags-none '("archive"))))
      (if filter
          (-filter filter notes)
        notes)))

;;;###autoload
  (defun my/vulpea-insert-candidates (&optional filter)
    "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
    (let ((notes (vulpea-db-query-by-tags-none nil)))
      (if filter
          (-filter filter notes)
        notes)))

  (setq vulpea-find-default-candidates-source #'my/vulpea-find-candidates
        vulpea-insert-default-candidates-source #'my/vulpea-insert-candidates
        vulpea-find-default-filter (when vulpea-db-index-heading-level
                                     (lambda (note)
                                       (= (vulpea-note-level note) 0)))
        vulpea-insert-default-filter (when vulpea-db-index-heading-level
                                       (lambda (note)
                                         (= (vulpea-note-level note) 0))))
  ;; Experiments

  (defun my/org-agenda-file-p (note)
    "Return non-nil when NOTE is an agenda file.

An file-level note, tagged with `org-file-tag-agenda'."
    (and (= (vulpea-note-level note) 0)
         (vulpea-note-tagged-any-p note org-file-tag-agenda)))


  (defun my/org-agenda-files ()
    "Return all agenda notes."
    (seq-filter #'my/org-agenda-file-p
                (vulpea-db-query-by-tags-some (list org-file-tag-agenda))))


  (defun my/org-agenda-file-find (&optional other-window)
    "Select an agenda file and visit it.

With OTHER-WINDOW (a prefix argument), visit it in another window."
    (interactive "P")
    (vulpea-visit (vulpea-select-from "Area" (my/org-agenda-files)
                                      :require-match t)
                  other-window))

  )

(use-package vulpea-ui
  :after vulpea)

(use-package vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (vulpea-journal-setup)
  ;; Move journal directory up one layer.
  (setq vulpea-journal-default-template
        '(:file-name "../journal/%Y-%m-%d.org"
          :title "%Y-%m-%d %A"
          :tags ("journal")
          :head "#+created: %<[%Y-%m-%d]>")))

(use-package vulpea-para
  :after (vulpea vulpea-ui)
  :config
  (setq vulpea-para-people-tag "person")
  ;; (vulpea-para-setup-defaults)
  )

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
       (:prefix ("r" . "roam") ;; TODO: Change this. Will require refactor of all 'note' binds.
        :desc "Find agenda files"          "a" #'my/org-agenda-file-find
        :desc "Find note"                  "f" #'vulpea-find
        :desc "Insert note"                "i" #'vulpea-insert
        :desc "Toggle sidebar"             "r" #'vulpea-ui-sidebar-toggle
        (:prefix ("d" . "by date")
         :desc "Journal previous"          "b" #'vulpea-journal-previous
         :desc "Journal date"              "d" #'vulpea-journal-date
         :desc "Journal next"              "f" #'vulepa-journal-next
         :desc "Journal today"             "t" #'vulpea-journal-today))
       (:when (modulep! :lang org +journal)
         (:prefix ("j" . "journal")
          :desc "New Entry"           "j" #'org-journal-new-entry
          :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
          :desc "Search Forever"      "s" #'org-journal-search-forever))))

;; TODO: Integrate with vulpea/denote/citar/nov/org-remark, replacing org-roam integration.
(use-package org-noter
  :defer t
  :config
  (setq org-noter-notes-search-path (list vulpea-default-notes-directory)
        org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil))

(use-package org-url
  :config
  (org-url-add-title-formatter "https://emacs.stackexchange.com/" (org-url-replace-in-title " - Emacs Stack Exchange" ""))
  (org-url-add-title-formatter "https://stackoverflow.com" (org-url-replace-in-title " - Stack Overflow" ""))
  (org-url-add-title-formatter "https://github.com" (org-url-replace-in-title ":[ ].*$?" "")))

(use-package org-habit
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

(use-package org-capture
  :defer t
  :config
  ;; Declare helper functions.
  ;; TODO: Does not work with multi-level heading captures.
  (defun org-capture-add-created-property ()
    "Add Create an ID and CREATED property for the current entry.
Intended for use with `:before-finalize' keyword in `org-capture-templates'."
    (when org-capture-mode
      (org-entry-put (point) "CREATED" (format-time-string org-id-ts-format))))
  ;; Load org-bookmark helper lib.
  (require 'org-bookmark)
  (defvar org-bookmarks-file (expand-file-name "bookmarks.org" org-directory)
    "Bookmarks file to use with `org-capture'.")
  (setq org-bookmark-location-handlers
        '((org-bookmark-handler-file-heading org-bookmarks-file "Inbox")))
  ;; Configure package.
  (setq org-capture-templates-contexts nil
        org-capture-templates
        '(("t" "Task" entry
           (file+headline org-inbox-file "Tasks")
           "* TODO %?"
           :prepend t
           :before-finalize (org-id-get-create)
           :empty-lines-after 1)
          ("n" "Note" entry
           (file+headline org-inbox-file "Notes")
           "* %?"
           :prepend t
           :before-finalize (org-capture-add-created-property)
           :empty-lines-after 1)
          ("b" "Bookmark" entry
           ;; (file org-bookmarks-file)
           #'org-bookmark-capture
           "* %(org-bookmark-format-link)\n%?"
           :prepend t
           :before-finalize (org-id-get-create)
           :immediate-finish t
           :jump-to-captured t))))

(use-package org-agenda
  :defer t
  :init
  ;; Custom agenda launcher.
  (defun my/org-agenda ()
    "My custom agenda launcher."
    (interactive)
    (org-agenda nil "o"))
  (map! :leader :desc "My agenda" "o a o" #'my/org-agenda)
  ;; Appearance
  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-remove-empty-sections)
  :config
  (custom-set-faces!
    '(org-agenda-structure
      :height 1.3 :weight bold))               ; Increase title/header size.
  (setq my/agenda-width 70                     ; Set tags column in a convoluted way.
        org-agenda-tags-column (+ 10 (* -1 my/agenda-width))
        org-habit-show-habits-only-for-today t ; Only show habits in one section.
        org-habit-show-all-today t             ; Keep habits visible even if done.
        org-agenda-start-with-log-mode t)      ; Show 'completed' items in agenda.

  ;; Display in dedicated side-window.
  ;; TODO: Possibly extend this for named agendas to appear in the side window.
  (set-popup-rule! "^\\*Org Agenda\\*" :side 'right :vslot 1 :width 60 :modeline nil :select t :quit nil)

  ;; Helpers
  ;; - Removes empty agenda sections.
  ;; - Skip specific tags.
  ;; - Skip all other tags.
  (defun my/org-agenda-remove-empty-sections ()
    "A simple function to remove empty agenda sections. Scans for blank lines.
Blank sections defined by having two consecutive blank lines.
Not compatible with the block separator."
    (interactive)
    (setq buffer-read-only nil)
    ;; initializes variables and scans first line.
    (goto-char (point-min))
    (let* ((agenda-blank-line "[[:blank:]]*$")
           (content-line-count (if (looking-at-p agenda-blank-line) 0 1))
           (content-blank-line-count (if (looking-at-p agenda-blank-line) 1 0))
           (start-pos (point)))
      ;; step until the end of the buffer
      (while (not (eobp))
        (forward-line 1)
        (cond ;; delete region if previously found two blank lines
         ((when (> content-blank-line-count 1)
            (delete-region start-pos (point))
            (setq content-blank-line-count 0)
            (setq start-pos (point))))
         ;; if found a non-blank line
         ((not (looking-at-p agenda-blank-line))
          (setq content-line-count (1+ content-line-count))
          (setq start-pos (point))
          (setq content-blank-line-count 0))
         ;; if found a blank line
         ((looking-at-p agenda-blank-line)
          (setq content-blank-line-count (1+ content-blank-line-count)))))
      ;; final blank line check at end of file
      (when (> content-blank-line-count 1)
        (delete-region start-pos (point))
        (setq content-blank-line-count 0)))
    ;; return to top and finish
    (goto-char (point-min))
    (setq buffer-read-only t))

  (defun my/org-agenda-skip-tag (tag)
    "Skip trees with this tag."
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (current-headline (or (and (org-at-heading-p) (point))
                                 (save-excursion (org-back-to-heading)))))
      (if (member tag (org-get-tags current-headline))
          next-headline nil)))

  (defun my/org-agenda-skip-all-but-this-tag (tag)
    "Skip trees that are not this tag."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (re-search-forward (concat ":" tag ":") subtree-end t)
          nil          ; tag found, do not skip
        subtree-end))) ; tag not found, continue after end of subtree

  ;; Custom Agendas

  (setq org-agenda-custom-commands
        '(("o" "My Agenda" ((agenda
                             ""
                             ( ;; Today
                              (org-agenda-overriding-header "Today\n")
                              (org-agenda-overriding-header " Agenda\n")
                              (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                              (org-agenda-block-separator nil)
                              (org-agenda-format-date " %a, %b %-e")  ; american date format
                              (org-agenda-start-on-weekday nil)          ; start today
                              (org-agenda-start-day "+0d")               ; don't show previous days. Required to make org-agenda-later work.
                              (org-agenda-span 1)                        ; only show today
                              (org-scheduled-past-days 0)                ; don't show overdue
                              (org-deadline-warning-days 0)              ; don't show deadlines for the future
                              (org-agenda-time-leading-zero t)           ; unify times formatting
                              (org-agenda-remove-tags t)
                              (org-agenda-time-grid '((today remove-match) (800 1000 1200 1400 1600 1800 2000 2200) "" ""))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              ;; (org-agenda-prefix-format '((agenda . " %8:(org-roam-agenda-category) %-5t ")))
                              (org-agenda-dim-blocked-tasks nil)
                              ;; TODO: Fix inbox not-skipping... Since I no longer have that tag.
                              (org-agenda-skip-function '(my/org-agenda-skip-tag "inbox"))
                              (org-agenda-entry-types '(:timestamp :deadline :scheduled))
                              ))
                            (agenda
                             ""
                             ( ;; Next Three Days
                              (org-agenda-overriding-header "\nNext Three Days\n")
                              (org-agenda-overriding-header "")
                              (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                              (org-agenda-block-separator nil)
                              (org-agenda-format-date " %a, %b %-e")
                              (org-agenda-start-on-weekday nil)
                              (org-agenda-start-day "+1d")
                              (org-agenda-span 3)
                              (org-scheduled-past-days 0)
                              (org-deadline-warning-days 0)
                              (org-agenda-time-leading-zero t)
                              (org-agenda-skip-function '(or (my/org-agenda-skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
                              (org-agenda-entry-types '(:deadline :scheduled))
                              (org-agenda-time-grid '((daily weekly) () "" ""))
                              (org-agenda-prefix-format '((agenda . "  %?-9:c%t ")))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              (org-agenda-dim-blocked-tasks nil)
                              ))
                            (agenda
                             ""
                             ( ;; Upcoming Deadlines
                              (org-agenda-overriding-header "\n Coming Up\n")
                              (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                              (org-agenda-block-separator nil)
                              (org-agenda-format-date " %a, %b %-e")
                              (org-agenda-start-on-weekday nil)
                              (org-agenda-start-day "+4d")
                              (org-agenda-span 28)
                              (org-scheduled-past-days 0)
                              (org-deadline-warning-days 0)
                              (org-agenda-time-leading-zero t)
                              (org-agenda-time-grid nil)
                                        ;(org-agenda-prefix-format '((agenda . "  %?-5t %?-9:c")))
                              ;; (org-agenda-prefix-format '((agenda . " %8:(org-roam-agenda-category) %-5t ")))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              (org-agenda-skip-function '(or (my/org-agenda-skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
                              (org-agenda-entry-types '(:deadline :scheduled))
                              (org-agenda-show-all-dates nil)
                              (org-agenda-dim-blocked-tasks nil)
                              ))
                            (agenda
                             ""
                             ( ;; Past Due
                              (org-agenda-overriding-header "\n Past Due\n")
                              (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                              (org-agenda-block-separator nil)
                              (org-agenda-format-date " %a, %b %-e")
                              (org-agenda-start-on-weekday nil)
                              (org-agenda-start-day "-60d")
                              (org-agenda-span 60)
                              (org-scheduled-past-days 60)
                              (org-deadline-past-days 60)
                              (org-deadline-warning-days 0)
                              (org-agenda-time-leading-zero t)
                              (org-agenda-time-grid nil)
                              ;; (org-agenda-prefix-format '((agenda . "  %?-9:(org-roam-agenda-category)%t ")))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              (org-agenda-skip-function '(or (my/org-agenda-skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
                              (org-agenda-entry-types '(:deadline :scheduled))
                              (org-agenda-show-all-dates nil)
                              (org-agenda-dim-blocked-tasks nil)
                              ))
                            (todo
                             ""
                             ( ;; Important Tasks No Date
                              (org-agenda-overriding-header "\n Important Tasks - No Date\n")
                              (org-agenda-block-separator nil)
                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'notregexp "\\[\\#A\\]"))
                              (org-agenda-block-separator nil)
                              (org-agenda-time-grid nil)
                              ;; (org-agenda-prefix-format '((todo . "  %?:(org-roam-agenda-category) ")))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              (org-agenda-dim-blocked-tasks nil)
                              ))
                            (todo
                             ""
                             ( ;; Next
                              (org-agenda-overriding-header "\n Next\n")
                              (org-agenda-block-separator nil)
                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "STRT")))
                              (org-agenda-block-separator nil)
                              (org-agenda-time-grid nil)
                              ;; (org-agenda-prefix-format '((todo . "  %?:(org-roam-agenda-category) ")))
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              (org-agenda-dim-blocked-tasks nil)
                              ))
                            (tags-todo
                             "inbox"
                             ( ;; Inbox
                              (org-agenda-overriding-header (propertize "\n Inbox\n" 'help-echo "Effort: 'c e' Refile: 'SPC m r'")) ;; Adds mouse hover tooltip.
                                        ;(org-agenda-remove-tags t)
                              (org-agenda-block-separator nil)
                              (org-agenda-prefix-format "  %?-4e ")
                                        ;(org-agenda-todo-keyword-format "%-4s")
                              )))))))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-star nil
        org-modern-hide-stars nil
        org-modern-todo t
        org-modern-todo-faces nil
        org-modern-tag t
        org-modern-tag-faces nil
        org-modern-priority t
        org-modern-progress nil
        org-modern-timestamp t
        org-modern-block-name nil
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autokeywords nil         ; Don't show hidden todo-keywords.
        org-appear-autolinks nil            ; Don't expand link markup.
        org-appear-autoemphasis t           ; Show emphasis markup.
        org-appear-autosubmarkers t         ; Show sub/superscript
        org-appear-autoentities t           ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil            ; Shows Org links.
        org-appear-inside-latex nil))       ; Don't show inside latex.

(use-package org-refile
  :config
  (setq org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file
        org-log-refile t                       ; Log when a heading is refiled.
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((nil :maxlevel . 3)
                             ;; (org-agenda-primary-file :maxlevel . 5)
                             (org-agenda-files :maxlevel . 3))))
