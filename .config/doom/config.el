(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(setq doom-theme 'doom-dracula
      doom-dracula-brighter-modeline t
      doom-dracula-colorful-headers t)

(when IS-MAC
  (setq doom-font (font-spec :family "Jetbrains Mono" :size 12)
        doom-big-font (font-spec :family "Jetbrains Mono" :size 16)
        doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
        doom-unicode-font (font-spec :family "JuliaMono")
        doom-serif-font (font-spec :family "IBM Plex Mono" :size 10 :weight 'light)))

(when IS-LINUX
  (setq doom-font (font-spec :family "Jetbrains Mono" :size 12)
        doom-big-font (font-spec :family "Jetbrains Mono" :size 16)
        ;;doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
        ;;doom-unicode-font (font-spec :family "JuliaMono")
        ;;doom-serif-font (font-spec :family "IBM Plex Mono" :size 10 :weight 'light)
        ))

(setq-default x-stretch-cursor t)

(add-to-list 'default-frame-alist '(alpha . 93)) ; [0-100]

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type 'visual)

(setq-default evil-scroll-count 5)

(global-auto-revert-mode 1)

(global-subword-mode 1)

(setq evil-want-fine-undo t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(defun wm-focus-on-error (direction move-fn)
    (when IS-LINUX
     (condition-case nil (funcall move-fn)
       (user-error (start-process "wm" nil "i3-msg" "focus" direction))))
     (when IS-MAC
      (condition-case nil (funcall move-fn)
        (user-error (start-process "wm" nil "yabai" "-m" "window" "--focus" direction)))))

(defun wm-window-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west"))))
    (wm-focus-on-error direction #'windmove-left)))

(defun wm-window-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east"))))
    (wm-focus-on-error direction #'windmove-right)))

(defun wm-window-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north"))))
    (wm-focus-on-error direction #'windmove-up)))

(defun wm-window-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south"))))
    (wm-focus-on-error direction #'windmove-down)))

(when IS-MAC
  (map! "s-h" #'wm-window-left)
  (map! "s-j" #'wm-window-down)
  (map! "s-k" #'wm-window-up)
  (map! "s-l" #'wm-window-right)
  (map! "s-=" #'balance-windows)
  (map! "s-Q" #'evil-quit))

(when IS-LINUX
  (map! "s-h" #'wm-window-left)
  (map! "s-j" #'wm-window-down)
  (map! "s-k" #'wm-window-up)
  (map! "s-l" #'wm-window-right)
  (map! "s-Q" #'evil-quit))

(map! :leader "w h" #'wm-window-left)
(map! :leader "w j" #'wm-window-down)
(map! :leader "w k" #'wm-window-up)
(map! :leader "w l" #'wm-window-right)

(when IS-MAC (setq mac-command-modifier 'control ; Maps Command -> Control
                    mac-control-modifier 'meta   ; Maps Control -> Alt (Meta)
                    mac-option-modifier 'super)) ; Maps Option -> Super

(setq org-directory "~/Projects/exocortex/")
(setq org-refile-targets
      '(("~/Projects/exocortex/nodes/calendar.org" :maxlevel . 3)))
(after! org
  (setq org-todo-keywords
        '((sequence
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
  (setq org-hide-emphasis-markers t     ; Hide syntax for emphasis markup. See org-appear.
        org-pretty-entities nil         ; Show entities like sub/superscript as UTF8.
        org-src-preserve-indentation t) ; Prevents changes to whitespace in source blocks.
  (setq org-ellipsis " ▾ "              ; Use a custom ellipsis for folded headings.
        org-hide-leading-stars t        ; Remove excess heading stars.
        org-hidden-keywords nil)        ; Can use to hide certain keywords.
  (setq org-startup-with-latex-preview nil)
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-image-actual-width '(0.9)           ; Use an in-buffer width closer to export's
        org-startup-with-inline-images t)       ; Show images at startup.
  (setq org-imenu-depth 10                     ; Allow imenu to search deeply in org docs.
        org-fold-core-style 'overlays          ; Bugfix: Hide IDs in org-roam backlinks.
        org-use-property-inheritance t         ; Sub-headings inherit parent properties.
        org-insert-heading-respect-content nil ; Insert heading here, not at end of list.
        org-use-fast-todo-selection 'auto)     ; Method to select TODO heading keywords.
  (setq org-log-done 'time                     ; Add completion time to DONE items.
        org-log-into-drawer t                  ; Log times into a drawer to hide them.
        org-log-states-order-reversed nil)     ; Log state changes chronologically.
  (setq org-list-allow-alphabetical t)         ; Enable use of alphabet as list bullets.
  (setq org-return-follows-link t)             ; Pressing enter opens links.
  (setq evil-collection-calendar-want-org-bindings t)
  (add-to-list 'org-modules 'org-habit t)
  (defun noncog/my-agenda ()
    "My custom agenda launcher."
    (interactive)
    (org-agenda nil "o"))
  (map! :leader :desc "My agenda" "o a o" #'noncog/my-agenda)
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
  )

(setq org-agenda-files (list
                        "~/Projects/exocortex/nodes/calendar.org"))
(setq noncog/agenda-width 70)
(set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width noncog/agenda-width :modeline nil :select t :quit t)
(after! org-agenda
  (setq org-habit-show-habits-only-for-today t ; Only show habits in one section.
        org-habit-show-all-today t)            ; Keep habits visible even if done.
  ;(setq +org-habit-min-width)
  ;(setq +org-habit-graph-padding)
  ;(setq +org-habit-graph-window-ratio)
  ;(setq org-habit-graph-column)
  ;(setq org-habit-today-glyph)
  ;(setq org-habit-completed-glyph)
  ;(setq org-habit-show-done-always-green)
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
  (setq org-agenda-start-with-log-mode t)      ; Show 'completed' items in agenda.
  )

(after! org-capture
  (setq org-capture-templates
        `(("i" "Inbox" entry
           (file+headline "inbox.org" "Inbox")
           "* %?\n%i\n" :prepend t)))
  (setq org-capture-bookmark nil)
  (map! :leader :desc "Org capture" "x" #'org-capture
        :leader :desc "Pop up scratch buffer" "X" #'doom/open-scratch-buffer)
  (defadvice! my/+org--restart-mode-h-careful-restart (fn &rest args)
    :around #'+org--restart-mode-h
    (let ((old-org-capture-current-plist
           (and (bound-and-true-p org-capture-mode)
                (bound-and-true-p org-capture-current-plist))))
      (apply fn args)
      (when old-org-capture-current-plist
        (setq-local org-capture-current-plist old-org-capture-current-plist)
        (org-capture-mode +1))))
  (add-hook! 'org-capture-after-finalize-hook (org-element-cache-reset t))
  )

(setq org-roam-directory (file-truename "~/Projects/exocortex"))
(setq org-roam-db-location (file-truename "~/Projects/exocortex/exocortex.db"))
(setq org-attach-id-dir (file-truename "~/Projects/exocortex/attachments"))
;(setq org-roam-file-exclude-regexp (rx (or "data/" "inbox.org")) )
(use-package! org-roam
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target
           (file+head "nodes/${slug}.org"
                      "#+title: ${title}\n#+filetags: :draft:\n")
           :immediate-finish t
           :unnarrowed t)))
  (org-roam-db-autosync-mode)
  (defun noncog/org-roam-is-draft-p (node)
    "Is this org-roam node a draft?"
    (member "draft" (org-roam-node-tags node)))
  (defun noncog/org-roam-random-draft ()
    "Get a random node with the draft tag. "
    (interactive)
    (org-roam-node-random nil #'noncog/org-roam-is-draft-p))
  (map! :leader :desc "Random draft node" "n r u" #'noncog/org-roam-random-draft)
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start nil)
  (map! :leader :desc "Roam-UI graph" "n r g" #'org-roam-ui-open-in-browser)
  )

(set-popup-rule! "^*xwidget" :ignore t :side 'left :width 120 :vslot 1 :quit t :select t :modeline nil)

(defcustom org-roam-ui-use-webkit t
  "Use embedded webkit to preview.
This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets`
option."
  :type 'boolean
  :group 'roam)

(defun org-roam-ui-browser (url)
  "Use browser specified by user to load URL.
Use default browser if nil."
  (if org-roam-ui-url-browser
      (let ((browse-url-generic-program org-roam-ui-url-browser)
            (browse-url-generic-args roam-url-args))
        (ignore browse-url-generic-program)
        (ignore browse-url-generic-args)
        (browse-url-generic url))
    (browse-url url)))

(defun org-roam-ui-open-url (url)
  "Ask the browser to load URL.
Use default browser unless `xwidget' is available."
  (if (and org-roam-ui-use-webkit
           (featurep 'xwidget-internal))
      (progn
        (save-window-excursion (xwidget-webkit-browse-url url))
        (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
          (when (buffer-live-p buf) (and (eq buf (current-buffer)) (quit-window)))
          (display-buffer buf)))
            (let (display-buffer-alist) (org-roam-ui-browser url))))

;;;###autoload
(define-minor-mode org-roam-ui-open-in-browser
  "open org-roam-ui in the browser"
 :lighter "roam"
 (org-roam-ui-open-url "http://127.0.0.1:35901"))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars nil              ; Let Org handle hiding the stars.
        org-modern-table-vertical 1            ; Use thinner lines than default.
        org-modern-table-horizontal 0.2        ; Unify line thickness with vertical line.
        org-modern-progress nil                ; Use default progress face.
        org-modern-priority nil                ; Use default priority face.
        org-modern-todo-faces                  ; Inherit Doom's Org faces.
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
  (setq org-appear-autoemphasis t              ; Show emphasis markup.
        org-appear-autosubmarkers t            ; Show sub/superscript
        org-appear-autoentities nil            ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil               ; Shows Org links.
        org-appear-autokeywords nil            ; Shows hidden Org keywords.
        org-appear-inside-latex nil)           ; Show LaTeX code. Use Fragtog instead.
  )

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! ox
  (setq org-export-with-creator t
        org-export-creator-string (format "Doom Emacs %s (Org mode %s)" emacs-version org-version))
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-headline-levels 5))

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

(after! ox-latex
  
  (setq org-latex-src-block-backend 'engraved)
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
  (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  )

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! engrave-faces-html
  :after ox-html)

(after! org-noter
  (setq org-noter-always-create-frame nil        ; Don't create a new frame for the session.
        org-noter-kill-frame-at-session-end nil) ; Don't kill any frames since none created.
  (setq org-noter-separate-notes-from-heading t) ; Adds line between headings and notes.
  )

(after! projectile
  (setq projectile-project-search-path '("~/Projects"))
  (defun +my/compile-in-vterm ()
    "Run `compile-command' in vterm in current project's root directory"
    (interactive)
    (projectile-run-vterm)
    (vterm-send-string "cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug && cmake --build build --config Release && ./build/minimax")
    (vterm-send-return))
  (defun +mypopup-kill (some-popup-window)
    (progn (+popup--kill-buffer (window-buffer some-popup-window) 0.1) ) t)
  (defun +mypopup-resize (some-popup-window)
    (fit-window-to-buffer some-popup-window nil 16 nil nil t))
  (map! :map project-prefix-map
        :leader
        :desc "Run project in vterm"
        "p v" #'+my/compile-in-vterm)
  (set-popup-rule! "^*vterm minimax*" :size #'+mypopup-resize :quit #'+mypopup-kill :autosave 'ignore)
  (map! :map project-prefix-map
        :leader
        :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects)
  )

(after! plantuml-mode
  (setq plantuml-default-exec-mode 'jar)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar))
  )

(after! pdf-tools
  (when IS-MAC (add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode))
  (pdf-tools-install)
  )

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
  (setq git-commit-summary-max-length 50)                  ; Maximum title (summary) length.
  (setq git-commit-fill-column 72)                         ; Description column limit.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'git-commit-finish-query-functions
               #'my-git-commit-check-style-conventions)
  )

(after! company
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 1)
  (set-company-backend! 'org-mode
    '(company-capf company-files :with company-yasnippet))
  (set-company-backend! 'sh-mode
    '(company-shell company-files :with company-yasnippet))
  )

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

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(after! persp-mode
  
  )

(defun noncog/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . noncog/org-mode-visual-fill))

(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  )

(after! vertico
  (setq vertico-sort-function #'vertico-sort-alpha)
  )

(map! :after vertico
      :map vertico-map
      "C-d" #'scroll-up-command)

(map! :after vertico
      :map vertico-map
      "C-u" #'scroll-down-command)

(after! doom-modeline
  
  )

(setq doom-modeline-height 35)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name t)
(setq doom-modeline-persp-icon t)

(after! nav-flash
  (setq nav-flash-delay 0.75)
  )

(after! doom-dashboard
  (map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
  )

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2)
  )
