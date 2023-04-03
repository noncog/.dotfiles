;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(setq org-directory "~/Projects/exocortex/")

(global-auto-revert-mode 1)

;;(advice-add 'evil-scroll-page-up :after #'evil-scroll-line-to-center)
;;(setq scroll-margin (* (frame-height) 2))
;;(setq scroll-conservatively 0)
;;(setq maximum-scroll-margin 0.5)
;;(setq scroll-preserve-screen-position t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(global-subword-mode 1)

(setq evil-want-fine-undo t)

;(server-start)

(setq doom-theme 'doom-dracula)

(setq doom-dracula-brighter-modeline t)

(setq doom-dracula-colorful-headers t)

(setq doom-font (font-spec :family "Jetbrains Mono" :size 12)
     doom-big-font (font-spec :family "Jetbrains Mono" :size 16)
     ;doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
     ;doom-unicode-font (font-spec :family "JuliaMono")
     ;doom-serif-font (font-spec :family "IBM Plex Mono" :size 10 :weight 'light))
     )

;(add-to-list 'default-frame-alist '(alpha . 93)) ; [0-100]

(setq display-line-numbers-type 'visual)

(display-time-mode 1)

(setq-default x-stretch-cursor t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when IS-MAC (setq mac-command-modifier 'control    ; Maps Control -> Command (macOS)
                    mac-control-modifier 'meta))    ; Maps Alt (Meta) -> Control (macOS)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
(map! :leader :desc "Pop up scratch buffer" "X" #'doom/open-scratch-buffer) ;Switch the scratch buffer and org-capture default keybind.
(map! :leader :desc "Brain.org" "b t" #'noncog/toggle-brain) ; evil style

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(map! :map org-mode-map
      :localleader
      :desc "View exported file"
      "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any),
found, using `org-view-output-file-extensions'."
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Find output files with these extensions, in order, viewing the first match.")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(map! :map org-mode-map
      :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))

(after! org
  (use-package! org
    :init
    (defconst noncog/gtd-file "~/Documents/org/gtd/gtd.org")
    (defun noncog/toggle-gtd ()
      "A function for toggling the view of the your chosen file in a side window."
      (interactive)
      (if (get-file-buffer noncog/gtd-file)
          (progn(kill-buffer (get-file-buffer noncog/gtd-file))(message "Killed GTD buffer."))
        (progn(display-buffer (find-file-noselect noncog/gtd-file))(message "Opened GTD buffer."))))
    ;;(set-popup-rule! "^gtd.org" :side 'right :vslot -1 :width 60 :modeline t :select t :quit nil)
    (map! :leader :desc "Open gtd.org" "o g" #'noncog/toggle-gtd) ; evil style
    :config
    (setq org-agenda-files (list
                            "gtd/inbox.org"
                            "gtd/gtd.org"
                            "gtd/calendar.org"
                            "personal/habits.org"))
    (add-to-list 'org-modules 'org-habit t)             ; Enables org-habit in modules.
    (setq org-habit-show-habits-only-for-today t        ; Ensure habits only shown in one section.
          org-habit-show-all-today t)                   ; Keep habits visible even if done today.
    (setq org-log-done 'time                            ; Add completion time to DONE items.
          org-log-into-drawer t)                        ; Puts log times into a drawer to hide them.
    (setq org-refile-targets
          '(("gtd.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
            ("calendar.org" :maxlevel . 3)
            ("someday.org" :maxlevel . 2)))
    ;; consider adding org-roam directory as the need arises.
    (setq org-todo-keywords
          '((sequence      ; For headings.
             "TODO(t!)"    ; A task that needs doing & is ready to do.
             "NEXT(n!)"    ; A task that needs doing & is ready to do.
             "STRT(s!)"    ; A task that is in progress.
             "WAIT(w@/!)"  ; Something external is holding up this task.
             "HOLD(h@/!)"  ; This task is paused/on hold because of me.
             "PROJ(p!)"    ; A project, which usually contains other tasks.
             "IDEA(i!)"    ; An unconfirmed and unapproved task or notion.
             "NOTE(N!)"    ; A fleeting note, in person, idea, link, anything.
             "APPT(a!)"    ; An appointment.
             "MEET(m!)"    ; A meeting.
             "READ(r!)"    ; Something to read.
             "LINK(l!)"    ; A link I want to note.
             "|"           ; Marker signifying remaining are a "completed" states.
             "DONE(d!)"    ; Task successfully completed.
             "KILL(k@/!)") ; Task was cancelled, aborted or is no longer applicable.
            (sequence      ; For nesting headings like lists.
             "[ ](T)"      ; A task that needs doing.
             "[-](S!)"     ; Task is in progress.
             "[?](W@/!)"   ; Task is being held up or paused.
             "|"
             "[X](D)"))    ; Task was completed.
          org-todo-keyword-faces
          '(("[-]"  . +org-todo-active)
            ("STRT" . +org-todo-active)
            ("NOTE" . +org-todo-active)
            ("[?]"  . +org-todo-onhold)
            ("WAIT" . +org-todo-onhold)
            ("HOLD" . +org-todo-onhold)
            ("PROJ" . +org-todo-project)
            ("KILL" . +org-todo-cancel)))
    (setq org-capture-templates
          `(("i" "Inbox" entry
             (file+headline "inbox.org" "Inbox")
             "* %?\n%i\n" :prepend t)
            ))
    (map! :leader :desc "Org capture" "x" #'org-capture)
    (setq org-return-follows-link t                     ; Pressing enter opens links.
          org-capture-bookmark nil                      ; Prevent org capture from adding to bookmarks.
          org-log-states-order-reversed nil             ; Log state changes in drawer chronologically.
          org-list-allow-alphabetical t                 ; Enable use of alphabet as list bullets.
          org-insert-heading-respect-content nil        ; Insert the heading at cursor, not at end of list.
          org-imenu-depth 10                            ; Allow imenu to search deeply in org docs.
          org-use-property-inheritance t                ; Sub-headings inherit parent properties. Useful.
          org-use-fast-todo-selection 'auto             ; Method used for org-todo. ('auto, 'exprt, or 'nil)
          org-fold-core-style 'overlays                 ; Used to hide IDs in Org Roam backlinks buffer.
          ;org-extend-today-until 25
          evil-collection-calendar-want-org-bindings t) ; Add evil bindings to org's calendar. Much needed.
    (setq org-highlight-latex-and-related '(native script entities))
    (setq org-ellipsis " ▾ "                            ; Set custom ellipsis for folded headings.
          org-startup-with-inline-images t              ; Show images at startup.
          org-hide-leading-stars t                      ; Remove excess heading stars.
          org-hide-emphasis-markers t                   ; Hide formatting for emphasis markup. See org-appear.
          org-pretty-entities t                         ; Show entities like sub/superscript as UTF8.
          org-hidden-keywords nil                       ; Considering using to hide certain keywords with org-appear and org-modern.
          org-src-preserve-indentation t)               ; Prevents changes to whitespace in source blocks.
    (defadvice! my/+org--restart-mode-h-careful-restart (fn &rest args)
      :around #'+org--restart-mode-h
      (let ((old-org-capture-current-plist (and (bound-and-true-p org-capture-mode)
                                                (bound-and-true-p org-capture-current-plist))))
        (apply fn args)
        (when old-org-capture-current-plist
          (setq-local org-capture-current-plist old-org-capture-current-plist)
          (org-capture-mode +1))))
    (add-hook! 'org-capture-after-finalize-hook (org-element-cache-reset t))
    ))

(after! org-agenda
  (use-package! org-agenda
    :init
    (defun noncog/my-agenda ()
      "My custom agenda launcher."
      (interactive)
      (org-agenda nil "o"))
    (map! :leader :desc "My agenda" "o a o" #'noncog/my-agenda)
    :config
    (setq org-agenda-start-with-log-mode t)             ; Show 'completed' items in agenda.
    ;(setq org-agenda-compact-blocks t)
    (setq noncog/agenda-width 70)
    (set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width noncog/agenda-width :modeline nil :select t :quit t)
    (custom-set-faces!
      '(org-agenda-structure
        :height 1.3 :weight bold))
    (defun noncog/agenda-remove-empty ()
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
           (cond
            ;; delete region if previously found two blank lines
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
      (setq buffer-read-only t)
    )
    (add-hook! 'org-agenda-finalize-hook #'noncog/agenda-remove-empty)
    (setq org-agenda-tags-column (+ 10 (* -1 noncog/agenda-width)))
    ;(setq org-overriding-columns-format "%10TODO %7EFFORT %PRIORITY     %100ITEM 100%TAGS")
    ;(setq org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")
    ;(setq org-agenda-tags-column 'auto)
    (defun noncog/skip-tag (tag)
      "Skip trees with this tag."
      (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (current-headline (or (and (org-at-heading-p) (point))
                                   (save-excursion (org-back-to-heading)))))
        (if (member tag (org-get-tags current-headline))
            next-headline nil)))
    (defun noncog/skip-all-but-this-tag (tag)
      "Skip trees that are not this tag."
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (if (re-search-forward (concat ":" tag ":") subtree-end t)
            nil          ; tag found, do not skip
          subtree-end))) ; tag not found, continue after end of subtree
    (setq org-agenda-custom-commands
          '(
            ("o" "My Agenda" (
             (agenda
              ""
              ( ;; Today
               ;(org-agenda-overriding-header "Today\n")
               (org-agenda-overriding-header " Agenda\n")
               (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
               (org-agenda-block-separator nil)
               (org-agenda-format-date " %a, %b %-e %y")  ; american date format
               (org-agenda-start-on-weekday nil)          ; start today
               (org-agenda-start-day nil)                 ; don't show previous days
               (org-agenda-span 1)                        ; only show today
               (org-scheduled-past-days 0)                ; don't show overdue
               (org-deadline-warning-days 0)              ; don't show deadlines for the future
               (org-agenda-time-leading-zero t)           ; unify times formatting
               (org-agenda-remove-tags t)
               (org-agenda-time-grid '((today remove-match) (800 1000 1200 1400 1600 1800 2000 2200) "" ""))
               ;(org-agenda-todo-keyword-format "%-4s")
               (org-agenda-prefix-format '((agenda . " %8:c %-5t ")))
               (org-agenda-dim-blocked-tasks nil)
               (org-agenda-skip-function '(noncog/skip-tag "inbox"))
               (org-agenda-entry-types '(:timestamp :deadline :scheduled))
               ))
             (agenda
              ""
              ( ;; Next Three Days
               ;(org-agenda-overriding-header "\nNext Three Days\n")
               (org-agenda-overriding-header "")
               (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
               (org-agenda-block-separator nil)
               (org-agenda-format-date " %a %b %-e")
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+1d")
               (org-agenda-span 3)
               (org-scheduled-past-days 0)
               (org-deadline-warning-days 0)
               (org-agenda-time-leading-zero t)
               (org-agenda-skip-function '(or (noncog/skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
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
               (org-agenda-format-date " %a %b %-e")
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+4d")
               (org-agenda-span 14)
               (org-scheduled-past-days 0)
               (org-deadline-warning-days 0)
               (org-agenda-time-leading-zero t)
               (org-agenda-time-grid nil)
               (org-agenda-prefix-format '((agenda . "  %?-5t %?-9:c")))
               ;(org-agenda-todo-keyword-format "%-4s")
               (org-agenda-skip-function '(or (noncog/skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
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
               (org-agenda-format-date "%a %b %-e")
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "-60d")
               (org-agenda-span 60)
               (org-scheduled-past-days 60)
               (org-deadline-past-days 60)
               (org-deadline-warning-days 0)
               (org-agenda-time-leading-zero t)
               (org-agenda-time-grid nil)
               (org-agenda-prefix-format '((agenda . "  %?-9:c%t ")))
               ;(org-agenda-todo-keyword-format "%-4s")
               (org-agenda-skip-function '(or (noncog/skip-tag "inbox") (org-agenda-skip-entry-if 'todo '("DONE" "KILL"))))
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
               (org-agenda-prefix-format '((todo . "  %?:c ")))
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
               (org-agenda-prefix-format '((todo . "  %?:c ")))
               ;(org-agenda-todo-keyword-format "%-4s")
               (org-agenda-dim-blocked-tasks nil)
               ))
             (tags-todo
              "inbox"
              ( ;; Inbox
               (org-agenda-overriding-header (propertize "\n Inbox\n" 'help-echo "Effort: 'c e' Refile: 'SPC m r'")) ; Ads mouse hover tooltip.
               ;(org-agenda-remove-tags t)
               (org-agenda-block-separator nil)
               (org-agenda-prefix-format "  %?-4e ")
               ;(org-agenda-todo-keyword-format "%-4s")
               ))
             ))))
    ))

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(after! ox
  (use-package! ox
    :config
    (setq org-export-headline-levels 5)
    (setq org-export-with-creator t
          org-export-creator-string (format "Doom Emacs %s (Org mode %s)" emacs-version org-version))
    ))

(use-package! ox-latex
  :init
  (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-src-block-backend 'engraved)
  :config
  (add-to-list 'org-latex-classes
               '("notes"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! engrave-faces-html
  :after ox-html)

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars nil                     ; Let Org handle hiding the stars. Caused issue.
        org-modern-table-vertical 1                   ; Use thinner lines than default.
        org-modern-table-horizontal 0.2               ; Unify line thickness with vertical line.
        org-modern-progress nil                       ; Use default progress face.
        org-modern-priority nil                       ; Use default priority face.
        org-modern-todo-faces                         ; Inherit Doom's Org faces.
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-horizontal-rule (make-string 80 ?─)
        )
  )

(use-package! org-modern-indent
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 1))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t                   ; Show emphasis markup.
        org-appear-autosubmarkers t                 ; Show sub/superscript
        org-appear-autoentities t                   ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil                    ; Shows Org links.
        org-appear-autokeywords nil                 ; Shows hidden Org keywords.
        org-appear-inside-latex nil)                ; Show LaTeX code. Use Fragtog instead.
  )

(after! org-roam
  (use-package! org-roam
    :init
    (setq org-roam-directory (file-truename "~/Projects/exocortex"))
    (setq org-roam-db-location (file-truename "~/Projects/exocortex/exocortex.db"))
    (setq org-attach-id-dir (file-truename "~/Projects/exocortex/attachments"))
    (setq org-roam-file-exclude-regexp (rx (or "data/" "inbox.org")) )
    (setq org-roam-graph-exclude-matcher '("inbox.org"))
    :config
    (org-roam-db-autosync-mode)
    (setq org-roam-capture-templates
          '(("d" "default" plain
             "%?"
             :target
             (file+head "nodes/${slug}.org"
                        "#+title: ${title}\n#+filetags: :draft:\n")
             :immediate-finish t
             :unnarrowed t)
            ))
    ;; TODO: set org-roam-dailies-capture-templates
    (defun noncog/org-roam-is-draft-p (node)
      "Is this org-roam node a draft?"
      (member "draft" (org-roam-node-tags node))
      )
    (defun noncog/org-roam-random-draft ()
      "Get a random node with the draft tag. "
      (interactive)
      (org-roam-node-random nil #'noncog/org-roam-is-draft-p))
    (map! :leader :desc "Random draft node" "n r u" #'noncog/org-roam-random-draft)
    ))

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)  :config
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(map! :leader :desc "Roam-UI graph" "n r g" #'org-roam-ui-open)

(map! :leader :desc "Roam-UI graph here" "n r G" #'org-roam-ui-open)

(after! org-noter
  (use-package! org-noter
    :config
    (setq org-noter-separate-notes-from-heading t)      ; Adds a blank line between headings and notes.
    (setq org-noter-always-create-frame nil)            ; Don't create a new frame for the session.
    (setq org-noter-kill-frame-at-session-end nil)            ; Don't kill any frames since we don't create any.
    ))

(after! projectile
  (use-package! projectile
    :config
    (setq projectile-project-search-path '("~/Projects"))
    (map! :map project-prefix-map
          :leader
          :desc "List dirty projects"
          "p l" #'projectile-browse-dirty-projects)
    ))

(after! plantuml-mode
  (setq plantuml-default-exec-mode 'jar)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar))
  )

(pdf-tools-install)

(after! pdf-tools
  (use-package! pdf-tools
    :config
    (when IS-MAC (add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode))
    ))

(after! yasnippet
  (use-package! yasnippet
    :config
    (setq yas-triggers-in-field t)
    ))

(after! magit
  (defcustom my-git-commit-style-convention-checks '(summary-has-type
                                                     summary-type-lowercase
                                                     summary-has-separator
                                                     summary-scope-lowercase
                                                     summary-title-starts-with-lowercase
                                                     summary-title-uses-imperative-verb
                                                     summary-title-not-end-in-punctuation)
    "List of checks performed by `my-git-commit-check-style-conventions'.
  Valid members are `summary-has-type',  `summary-type-lowercase',
  `summary-has-separator', `summary-scope-lowercase',
  `summary-title-starts-with-lowercase', `summary-title-uses-imperative-verb', and
  `summary-title-not-end-in-punctuation'.
  That function is a member of `git-commit-finish-query-functions'."
    :options '(summary-has-type
               summarty-type-lowercase
               summary-has-separator
               summary-scope-lowercase
               summary-title-starts-with-lowercase
               summary-title-uses-imperative-verb
               summary-title-not-end-in-punctuation
               )
    :type '(list :convert-widget custom-hook-convert-widget)
    :group 'git-commit)
  (defun my-git-commit-check-style-conventions (&optional force) ; TODO check using force
    "Check for violations of certain basic style conventions.
  
  For each violation ask the user if she wants to proceed anyway.
  Option `my-git-commit-check-style-conventions' controls which
  conventions are checked."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (git-commit-summary-regexp) nil t)
      (let* ((summary (match-string 1))
             (commit-type (substring summary 0 (string-match-p "[()!:]" summary)))
             (commit-separator-check (string-match-p "^.*[^[:blank:]]:" summary))
             (commit-separator-location (string-match-p ":" summary))
             (commit-scope (if (string-match-p "(.*)[!:]" summary) (substring summary (1+ (string-match-p "(" summary)) (string-match-p ")!?:?" summary)) nil))
             (commit-title (if commit-separator-check (substring summary (+ commit-separator-location 2)) "undetectable-title"))
             (lowercase-title-first-word (downcase (substring commit-title 0 (string-match-p "[[:blank:]]" commit-title)))))
        (and (or (not (memq 'summary-type-lowercase my-git-commit-style-convention-checks))
                 (let ((case-fold-search nil)) (string-match-p "^[[:lower:]]*$" commit-type))
                 (y-or-n-p "Commit type is not lowercase. Commit anyway?"))
             (or (not (memq 'summary-has-type my-git-commit-style-convention-checks))
                 (car (member commit-type (get-commit-types)))
                 (when (y-or-n-p "Commit type is invalid. Commit anyway?")
                   (when (y-or-n-p (format "Add `%s' to list of commit types?" commit-type))
                     (with-temp-buffer
                       (insert commit-type)
                       (insert "\n")
                       (write-region (point-min) (point-max) commit-types-file t))) t))
             (or (not (memq 'summary-has-separator my-git-commit-style-convention-checks))
                 (not (null commit-separator-check))
                 (y-or-n-p "Commit title has invalid separator. Commit anyway?"))
             (or (not (memq 'summary-scope-lowercase my-git-commit-style-convention-checks))
                 (when (or (not commit-scope) (string-empty-p commit-scope)) (y-or-n-p "Commit scope is empty. Commit anyway?"))
                 (when commit-scope (if (let ((case-fold-search nil)) (string-match-p "^[[:lower:]]*$" commit-scope)) t
                                      (y-or-n-p "Commit scope invalid. Only lowercase letters allowed. Commit anyway?"))))
             (or (not (memq 'summary-title-starts-with-lowercase my-git-commit-style-convention-checks))
                 (if (not commit-separator-check) (y-or-n-p "Title undetectable. Commit anyway?") (let ((case-fold-search nil))
                   (string-match-p "^[[:lower:]]" commit-title)))
                 (y-or-n-p "Commit title does not start with lowercase letter. Commit anyway?"))
             (or (not (memq 'summary-title-uses-imperative-verb my-git-commit-style-convention-checks))
                 (if (not commit-separator-check) (y-or-n-p "Imperative verb undetectable. Commit anyway?")
                   (car (member lowercase-title-first-word (get-imperative-verbs))))
                 (when (y-or-n-p "Commit title should use imperative verb. Does it?")
                   (when (y-or-n-p (format "Add `%s' to list of commit types?" lowercase-title-first-word))
                     (with-temp-buffer
                       (insert lowercase-title-first-word)
                       (insert "\n")
                       (write-region (point-min) (point-max) imperative-verbs-file t))) t))
             (or (not (memq 'summary-title-not-end-in-punctuation my-git-commit-style-convention-checks))
                (not (string-match-p "[\\.!\\?;,:]$" commit-title))
                (y-or-n-p "Commit title ends with punctuation. Commit anyway?"))))))
  (setq commit-types-file "~/.config/git/commit/commit-types")
  (defun get-commit-types ()
    "Return a list of commit types."
    (let ((file-path commit-types-file))
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t))))
  (setq imperative-verbs-file "~/.config/git/commit/imperative-verbs")
  (defun get-imperative-verbs ()
    "Return a list of imperative verbs."
    (let ((file-path imperative-verbs-file))
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t))))
  (use-package! magit
    :custom
    (git-commit-summary-max-length 50)                  ; Maximum title (summary) length.
    (git-commit-fill-column 72)                         ; Description column limit.
    :config
    (defun my/magit-process-environment (env)
      "Detect and set git -bare repo env vars when in tracked dotfile directories."
      (let* ((default (file-name-as-directory (expand-file-name default-directory)))
             (git-dir (expand-file-name "~/.dotfiles/"))
             (work-tree (expand-file-name "~/"))
             (dotfile-dirs
              (-map (apply-partially 'concat work-tree)
                    (-uniq (-keep #'file-name-directory (split-string (shell-command-to-string
                    (format "/usr/bin/git --git-dir=%s --work-tree=%s ls-tree --full-tree --name-only -r HEAD"
                            git-dir work-tree))))))))
        (push work-tree dotfile-dirs)
        (when (member default dotfile-dirs)
          (push (format "GIT_WORK_TREE=%s" work-tree) env)
          (push (format "GIT_DIR=%s" git-dir) env)))
      env)
    (advice-add 'magit-process-environment
                :filter-return #'my/magit-process-environment)
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
    (add-to-list 'git-commit-finish-query-functions
                 #'my-git-commit-check-style-conventions)
    ))

(after! company
  (use-package! company
    :config
    (setq company-idle-delay 0.3
          company-tooltip-limit 10
          company-minimum-prefix-length 1)
    ))

(after! vterm
  (use-package! vterm
    :config
    (define-key vterm-mode-map (kbd "<C-backspace>") (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
    ))

;; Add an extra line to work around bug in which-key imprecise
(after! which-key
  (defun add-which-key-line (f &rest r) (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))))
  (advice-add 'which-key--show-popup :around #'add-which-key-line)
  )

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override nil))

(defun noncog/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . noncog/org-mode-visual-fill))
