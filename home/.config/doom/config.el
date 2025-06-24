;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defconst user-home-directory (getenv "HOME")
  "Filepath of the user's home directory.")

(defconst user-system-name (string-remove-suffix ".local" (system-name))
    "Name of the user's system.")

(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

(use-package! system-settings
  :config
  (setq system-settings-list
      (append
       ;; Put system-specific settings at the front so they're found first.
       (when (string= user-system-name "Ganymede")
         '((wm-command . "yabai -m window --")
           (wm-focus-command . "focus")
           (wm-move-command . "swap")
           (wm-direction-left . "west")
           (wm-direction-right . "east")
           (wm-direction-up . "north")
           (wm-direction-down . "south")))
       ;; Put default settings after system-specific settings.
       '((wm-command . "i3-msg ")
         (wm-focus-command . "focus")
         (wm-move-command . "move")
         (wm-direction-left . "left")
         (wm-direction-right . "right")
         (wm-direction-up . "up")
         (wm-direction-down . "down")))))

(when (string= user-login-name "noncog")
  (setq user-full-name "noncog"
        user-mail-address "noncog@github.com"))

(global-auto-revert-mode 1)

(setq evil-want-fine-undo t)

(global-subword-mode 1)

(setq evil-kill-on-visual-paste nil)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! wm
  :config
  (setq wm-command (system-settings-get 'wm-command)
        wm-focus-command (system-settings-get 'wm-focus-command)
        wm-move-command (system-settings-get 'wm-move-command)
        wm-direction-left (system-settings-get 'wm-direction-left)
        wm-direction-right (system-settings-get 'wm-direction-right)
        wm-direction-up (system-settings-get 'wm-direction-up)
        wm-direction-down (system-settings-get 'wm-direction-down))
  ;; I use super as my window manager modifier key.
  ;; These binds allow the window manager windowing keybinds to work within Emacs.
  (map! (:map global-map
              "s-h" #'wm-focus-win-left
              "s-j" #'wm-focus-win-down
              "s-k" #'wm-focus-win-up
              "s-l" #'wm-focus-win-right
              "s-H" #'wm-move-win-left
              "s-J" #'wm-move-win-down
              "s-K" #'wm-move-win-up
              "s-L" #'wm-move-win-right
              "s-=" #'balance-windows
              "s-v" #'evil-window-vsplit
              "s-s" #'evil-window-split
              "s-Q" #'evil-quit))
  ;; These binds allow you to use Doom's window management keybinds from Emacs to windows outside of it.
  (map! :leader "w h" #'wm-focus-win-left
        "w j" #'wm-focus-win-down
        "w k" #'wm-focus-win-up
        "w l" #'wm-focus-win-right
        "w H" #'wm-move-win-left
        "w J" #'wm-move-win-down
        "w K" #'wm-move-win-up
        "w L" #'wm-move-win-right)
  ;; These binds allow you to resize windows directionally.
  ;; These echo the window manager functionality but are not fully integrated yet.
  (map! "C-s-h" #'wm-resize-win-left
        "C-s-j" #'wm-resize-win-down
        "C-s-k" #'wm-resize-win-up
        "C-s-l" #'wm-resize-win-right))

(setq-default evil-scroll-count 10)

(when (featurep :system 'macos)
  (setq mac-command-modifier 'control ; Maps Command -> Control
        mac-control-modifier 'meta    ; Maps Control -> Alt (Meta)
        mac-option-modifier 'super))  ; Maps Option -> Super

(map! :leader "h T" nil
      :leader "t T" #'doom/toggle-profiler)

(map! :leader "t o" #'doom/set-frame-opacity)

(when (and (doom-font-exists-p "JetBrains Mono")
           (featurep :system 'linux))
           (setq doom-font (font-spec :family "JetBrains Mono" :size 14)
                 doom-big-font (font-spec :family "JetBrains Mono" :size 16)))

(setq doom-theme 'doom-one)

(map! :leader "h t" nil
      :leader "t t" #'load-theme)

(setq-default x-stretch-cursor t)

(when (and (featurep :system 'macos)
           (>= emacs-major-version 29))
           (add-to-list 'default-frame-alist '(undecorated-round . t)))

(when (and (featurep :system 'macos)
           (>= emacs-major-version 29))
      (setq frame-inhibit-implied-resize '(font font-backend tab-bar-lines)))

(map! "C-s-f" nil
      :leader "t F" nil)

(use-package! doom-modeline
  :defer t
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon t
        doom-modeline-buffer-file-true-name t
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon nil
        doom-modeline-vcs-max-length 60
        auto-revert-check-vc-info t))

(setq display-line-numbers-type 'visual
      display-line-numbers-grow-only t)
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)

(setq +doom-dashboard-pwd-policy user-home-directory) ;; NOTE: uses personal constant.

(use-package! helpful
  :defer t
  :config
  (setq helpful-max-buffers 10)
  (advice-remove 'helpful--navigate #'+popup--helpful-open-in-origin-window-a)
  (defadvice! my/+popup--helpful-open-in-origin-window-a (button)
    "Open links in non-popup, originating window rather than helpful's window."
    :override #'helpful--navigate
    (let ((path (substring-no-properties (button-get button 'path)))
          enable-local-variables
          origin)
      (save-popups!
       (find-file path)
       (when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
         (goto-char pos))
       (setq origin (selected-window))
       (recenter 0)) ; Added argument 0 to cause recenter to top of screen.
      (select-window origin))))

(use-package! vertico
  :defer t
  :config
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

(use-package! sinister)

(use-package! mini-ontop)

(use-package! stillness-mode)

(use-package! avy
  :defer t
  :config
  ;; Behavior
  (setq avy-all-windows t))

(setq +lookup-provider-url-alist
      '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("Debian Package" "https://packages.debian.org/search?keywords=%s")
        ("Github" "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("StackOverflow" "https://stackoverflow.com/search?q=%s")
        ("Doom Issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
        ("Internet archive" "https://web.archive.org/web/*/%s")
        ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
        ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
        ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s&title=Special%3ASearch&wprov=acrw1")
        ("AUR" "https://aur.archlinux.org/packages?O=0&K=%s")))

(use-package! projectile
  :defer t
  :init
  (setq projectile-project-search-path
        '(("~/.dotfiles" . 0)
          ("~/dev/projects" . 1)   ; My projects.
          ("~/dev/source" . 1)))   ; Other's code.
  :config
  ;; Behavior
  ;; - Disable automatic project discovery.
  (setq projectile-auto-discover nil
        projectile-track-known-projects-automatically nil)
  ;; Keybinds
  (map! :map project-prefix-map
        :leader
        :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects))

(use-package! magit
  :defer t
  :config
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/dev/projects" . 1)   ; My projects.
          ("~/dev/source" . 1)))   ; Other's code.
  (require 'magit-lint)) ;; Load my custom commit linter.

(use-package! vterm
  :defer t
  :init
  ;; Update Bash version on macOS if available.
  (when (and (featurep :system 'macos)
             (file-exists-p "/opt/homebrew/bin/bash"))
    (setq vterm-shell "/opt/homebrew/bin/bash"))
  :config
  ;; Keybinds
  ;; - Fix M-backspace keybind on macOS.
  (evil-define-key* 'insert vterm-mode-map (kbd "<M-backspace>") #'vterm-send-meta-backspace))

(use-package! dired
  :defer t)

(use-package! dirvish
  :defer t
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/.dotfiles/" "Dotfiles")
     ("o" "~/documents/org/" "Org")))
  :config
  (set-popup-rule! "^ ?\\*\\(?:[Dd]irvish\\|SIDE :: \\).*" :width 0.15 :ignore t :quit 'current))

(use-package! org
  :defer t
  :init
  ;; Functions
  (defun org-file (path) ;; Inspired by jwiegley.
    "Return PATH expanded relative to org-directory.

- Intended for use with file names.
- PATH must not begin with a slash."
    (expand-file-name path org-directory))
  (defun org-subdirectory (subdir)
    "Return SUBDIR expanded as directory name relative to org-directory."
    (file-name-as-directory (org-file subdir)))
  ;; Variables
  (setq org-directory (file-truename "~/documents/org/") ;; NOTE: trailing slash denotes a directory.
        org-id-locations-file (expand-file-name "data/org-ids" org-directory)
        org-inbox-directory (org-subdirectory "inbox")
        org-inbox-file (expand-file-name (concat user-system-name ".org") org-inbox-directory))
  :config
  ;; Appearance
  (setq org-default-notes-file org-inbox-file   ; Set default notes file to inbox file.
        org-hide-leading-stars t                ; Hide leading stars.
        org-ellipsis " ▾ "                      ; Use UTF-8 to indicate folded heading.
        org-hidden-keywords nil                 ; Don't hide any TODO keywords.
        org-image-actual-width '(0.9)           ; Use an in-buffer image width closer to export's
        org-startup-with-inline-images t        ; Show images at startup.
        org-startup-with-latex-preview nil      ; Don't show LaTeX on startup.
        org-hide-emphasis-markers t             ; Hide syntax for emphasis. (Use org-appear)
        org-src-preserve-indentation t          ; Keep language specific indenting in source blocks.
        org-pretty-entities t)                  ; Show sub/superscript as UTF8.
  (setq org-property-format "%-10s %s")
  ;; Behavior
  ;; - General
  (setq org-list-allow-alphabetical t           ; Use alphabet as lists.
        org-use-property-inheritance t          ; Sub-headings inherit parent properties.
        org-imenu-depth 5                       ; Allow imenu to search deeply in org docs.
        org-return-follows-link t               ; Allow return to open links.
        org-insert-heading-respect-content nil  ; Insert heading here, not at end of list.
        org-use-fast-todo-selection 'auto)      ; Method to select TODO heading keywords.
  ;; - Logging
  (setq org-log-done 'time                      ; Add completion time to DONE items.
        org-log-into-drawer t                   ; Log times into a drawer to hide them.
        org-log-reschedule t                    ; Log rescheduling of scheduled items.
        org-log-redeadline t                    ; Log rescheduling of deadline items.
        org-treat-insert-todo-heading-as-state-change t
        org-log-states-order-reversed nil)      ; Log times reverse chronologically.
  ;; - Habits
  (add-to-list 'org-modules 'org-habit t)       ; Enable org-habit for tracking repeated actions.
  ;; - Calendar
  (setq evil-collection-calendar-want-org-bindings t) ; Use Evil keybinds to move in calendar.
  ;; - Tags
  ;; - Todos
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"      ; Task that needs doing & is ready to do.
           "NEXT(N!)"      ; Task that needs doing & is ready to do.
           "|"             ; Required to get org-roam to ignore the following todo items.
           "DONE(d!)"      ; Task successfully completed.
           "KILL(k@/!)")   ; Task cancelled or not applicable.
          (type
           "WAIT(w@/!)"    ; Task on hold by somthing.
           "SOMEDAY(s!)"   ; Task that could be done someday.
           "MAYBE(m!)"     ; Task that I might do someday.
           "BOOKMARK(b!)"  ; A link to be bookmarked.
           "ISSUE(I!)"     ; An issue.
           "IDEA(i!)"      ; An idea.
           "NOTE(n!)"))    ; A fleeting note, in person, idea, or link.
        org-todo-keyword-faces
        '(("TODO"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("SOMEDAY" . +org-todo-onhold)
          ("MAYBE" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("DONE" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

(use-package! denote
  :after org
  :config
  (setq denote-directory org-directory    ;; Use org-directory with denote.
        org-id-ts-format denote-id-format ;; Use denote-id-format for org-id.
        org-id-method 'ts                 ;; Use org-roam compatible front-matter.
        ;; Use Denote ID as date to avoid the org timestamp which is missing seconds.
        denote-date-format denote-id-format
        denote-org-front-matter ":PROPERTIES:\n:ID: %4$s\n:END:\n#+title: %1$s\n#+filetags: %3$s\n"))

(use-package! org-roam
  :defer t
  :init
  (setq org-roam-directory org-directory
        org-roam-db-location (expand-file-name "data/org-roam.db" org-roam-directory))
  :config
  (require 'org-roam-file)       ;; Integrates org-roam with org-agenda and denote.
  (require 'org-roam-include)    ;; Prevents certain files with org headings with org-id from being included in database.
  (setq org-roam-db-node-include-function #'org-roam-include-p
        org-roam-include-exclude-directories (list org-inbox-directory)
        org-roam-include-exclude-files (list "agenda.org" "bookmarks.org"))
  (setq org-roam-file-rename-exclude (append
                                      '("~/Documents/org/resource/bookmarks.org"
                                        "~/Documents/org/agenda.org"
                                        "~/Documents/org/system-overview.org")
                                      (directory-files org-inbox-directory 'full ".org$") nil))
  (require 'org-roam-tags)       ;; Automatic tag insertion after node insertion.
  (when (modulep! :ui modeline)
    (require 'org-roam-modeline) ;; Integrate doom-modeline with the denote file naming scheme under org-roam.
    (setq doom-modeline-buffer-file-name-function #'org-roam-modeline-process-buffer-file-name
          doom-modeline-buffer-file-truename-function #'org-roam-modeline-process-buffer-file-name)))

(use-package! org-capture
  :defer t
  :config
  (defun org-capture-id-get-created ()
    "Create an ID and CREATED property for the current entry.

Intended for use with `:before-finalize` keyword in `org-capture-templates`."
    (org-entry-put (point) "CREATED" (format-time-string "[%F %a %R]" (date-to-time (org-id-get-create)))))

  ;; NOTE: Does not support errors. Implicitly sets the id does not give option to pass in ID.
  ;; TODO: Rewrite.
  ;; TODO: Extend org-bookmark to use link capturing abilities.
  ;; (setq +org-capture-fn #'org-roam-capture)
  ;; (set-popup-rule! "^*Capture*$" :side 'bottom :height 1 :select nil :autosave 'ignore)
  ;; (set-popup-rule! "^CAPTURE-.*$" :side 'bottom :height 0.3 :vslot -1 :quit nil :select t :autosave 'ignore)
  (setq org-capture-bookmark nil
        org-capture-templates
        '(("t" "Task" entry
           (file org-inbox-file)
           "* TODO %?"
           :prepend t
           :before-finalize (org-capture-id-get-created)
           :empty-lines-before 1)
          ("n" "Note" entry
           (file org-inbox-file)
           "* NOTE %?"
           :prepend t
           :before-finalize (org-capture-id-get-created)
           :empty-lines-before 1)
          ("b" "Bookmark" entry
           (file org-inbox-file)
           "* BOOKMARK %?"
           :prepend t
           :before-finalize (org-capture-id-get-created)
           :empty-lines-before 1))))
  ;; Hidden templates used in certain context.
  ;; ("T" "Hidden Templated tasks.")
  ;; ("Ta" "Hidden Agenda tasks.")
  ;; ("Tah" "Task in Agenda Heading" entry
  ;;  (file org-inbox-file)
  ;;  "* TODO %?"
  ;;  :prepend t
  ;;  :before-finalize (org-id-get-create)
  ;;  :empty-lines-before 1)))
  ;; ))

  ;; (defun my/org-capture-context () (if (string= (buffer-file-name) (expand-file-name "~/Documents/org/agenda.org")) t nil))
  ;; (setq org-capture-templates-contexts
  ;;       '(("t" ((not-in-file .  "~/Documents/org/agenda.org")))
  ;;         ("n" ((not-in-file .  "~/Documents/org/agenda.org")))
  ;;         ("v" (my/org-capture-context))
  ;;         ("t" "v" (my/org-capture-context (in-file .  "~/Documents/org/agenda.org")))
  ;;         ))
  ;; ("t" "v" ((in-file . "~/Documents/org/agenda.org"))))
  ;; ("t" "n" (my/org-capture-context (in-file .  "~/Documents/org/agenda.org")))
  ;; Known to work:
  ;; ("v" (my/org-capture-context (in-file .  "~/Documents/org/agenda.org")))

  ;; )
;; ("t" "v" ((in-file . "~/Documents/org/agenda.org")
;; )

(use-package! org-roam-capture
  :defer t
  :config
  (setq org-roam-capture-templates
        '(("n" "node" plain "%?"
           :target (file+head "node/${id}--${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :target (file+head "resource/person/${id}--${slug}.org"
                              "#+title: ${title}\n#+filetags: :person:\n")))))

(use-package! org-roam-protocol
  :defer t
  :config
  (setq org-roam-protocol-store-links t)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "*%?"
           :target (file+head "resource/reference/${id}--${slug}.org" "#+title: ${title}\n\n${body}")
           :unnarrowed t)
          ("b" "Bookmark" plain "%?"
           :target (file+olp "resource/bookmarks.org" ("*Testing"))
           :unnarrowed t
           :empty-lines-before 1))))

(use-package! org-roam-dailies
  :defer t
  :config
  ;; (set-popup-rule! "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)" :side 'right :vslot 1 :width 60 :modeline nil :select t :quit nil)
  (setq org-roam-dailies-directory "log/journal/"
        ;; TODO: Investigate if this format will show up in Calendar. Due to org-time-string.org mention in help string.
        org-roam-dailies-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${id}--log.org" "#+title: Log\n#+date: %u\n#+filetags:\n\n* Log")))))

(use-package! org-agenda
  :defer t
  :init
  (add-hook! 'org-agenda-finalize-hook #'noncog/agenda-remove-empty)
  ;; Keybinds
  (defun noncog/my-agenda ()
    "My custom agenda launcher."
    (interactive)
    (org-agenda nil "o"))
  (map! :leader :desc "My agenda" "o a o" #'noncog/my-agenda)
  ;; Helpers
  (defun org-agenda-primary-file ()
    "Return primary agenda file."
    (org-file "agenda.org"))
  :config
  ;; Appearance
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
    (setq buffer-read-only t))
  (setq noncog/agenda-width 70)
  (setq org-agenda-tags-column (+ 10 (* -1 noncog/agenda-width)))
  (setq org-agenda-custom-commands
        '(
          ("o" "My Agenda" (
           (agenda
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
             (org-agenda-prefix-format '((agenda . " %8:(org-roam-agenda-category) %-5t ")))
             (org-agenda-dim-blocked-tasks nil)
             ;; TODO: Fix inbox not-skipping... Since I no longer have that tag.
             (org-agenda-skip-function '(noncog/skip-tag "inbox"))
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
             (org-agenda-format-date " %a, %b %-e")
             (org-agenda-start-on-weekday nil)
             (org-agenda-start-day "+4d")
             (org-agenda-span 28)
             (org-scheduled-past-days 0)
             (org-deadline-warning-days 0)
             (org-agenda-time-leading-zero t)
             (org-agenda-time-grid nil)
             ;(org-agenda-prefix-format '((agenda . "  %?-5t %?-9:c")))
             (org-agenda-prefix-format '((agenda . " %8:(org-roam-agenda-category) %-5t ")))
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
             (org-agenda-format-date " %a, %b %-e")
             (org-agenda-start-on-weekday nil)
             (org-agenda-start-day "-60d")
             (org-agenda-span 60)
             (org-scheduled-past-days 60)
             (org-deadline-past-days 60)
             (org-deadline-warning-days 0)
             (org-agenda-time-leading-zero t)
             (org-agenda-time-grid nil)
             (org-agenda-prefix-format '((agenda . "  %?-9:(org-roam-agenda-category)%t ")))
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
             (org-agenda-prefix-format '((todo . "  %?:(org-roam-agenda-category) ")))
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
             (org-agenda-prefix-format '((todo . "  %?:(org-roam-agenda-category) ")))
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
  ;;(set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline nil :select t :quit t)
  ;; TODO: Possibly extend this for named agendas to appear in side window.
  (set-popup-rule! "^\\*Org Agenda\\*" :side 'right :vslot 1 :width 60 :modeline nil :select t :quit nil)
  ;; Behavior
  (setq org-agenda-start-with-log-mode t)      ; Show 'completed' items in agenda.
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
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12(org-roam-agenda-category)%?-12t% s")
          (todo . " %i %-12(org-roam-agenda-category) ")
          (tags . " %i %-12(org-roam-agenda-category) ")
          (search . " %i %-12(org-roam-agenda-category) "))))

(use-package! org-refile
  :config
  (setq org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file
        org-log-refile t                       ; Log when a heading is refiled.
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((nil :maxlevel . 3)
                            (org-agenda-primary-file :maxlevel . 5)
                            (org-agenda-files :maxlevel . 3))))

(use-package! org-modern
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

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autokeywords nil         ; Don't show hidden todo-keywords.
        org-appear-autolinks nil            ; Don't expand link markup.
        org-appear-autoemphasis t           ; Show emphasis markup.
        org-appear-autosubmarkers t         ; Show sub/superscript
        org-appear-autoentities t           ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil            ; Shows Org links.
        org-appear-inside-latex nil))       ; Don't show inside latex.

(use-package! toc-org
  :defer t
  :config
  (setq org-toc-default-depth 3)
  ;; Extend org-toc to add folding to table of contents using HTML.
  (defconst toc-org-fold-tag-regexp ":fold:\\(\\(\s+-->\\)?$\\|[^ ]*?:\\(\s+-->\\)?$\\)"
    "Regexp to find the heading with the :fold: tag")
  (defun toc-org-insert-toc (&optional dry-run)
    "Update table of contents in heading tagged :TOC:.

When DRY-RUN is non-nil, the buffer is not modified, only the
internal hash-table is updated to enable `org-open-at-point' for
TOC links.

The table of contents heading may also be set with these tags:

- :TOC_#: Sets the maximum depth of the headlines in the table of
          contents to the number given, e.g. :TOC_3: for
          3 (default for plain :TOC: tag is 2).

- :TOC_#_gh: Sets the maximum depth as above and also uses
             GitHub-style anchors in the table of contents (the
             default).  The other supported style is :TOC_#_org:,
             which is the default org style.

Headings may be excluded from the TOC with these tags:

- :noexport: Exclude this heading.

- :noexport_#: Exclude this heading's children with relative
               level greater than number given (e.g. :noexport_1:
               causes all child headings to be excluded).

Note that :noexport: is also used by Org-mode's exporter, but
not :noexport_#:."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search t)
             (markdown-syntax-p (derived-mode-p 'markdown-mode))
             (heading-symbol-regexp (if markdown-syntax-p "^#" "^\\*")))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward (concat heading-symbol-regexp toc-org-toc-org-regexp) (point-max) t)
          (let* ((tag (match-string 2))
                 ;; is there a better way to convert char to number?
                 (depth (if tag (- (aref tag 1) ?0) toc-org-max-depth))
                 (hrefify-tag (if (and tag (>= (length tag) 4))
                                  (downcase (substring tag 3)) toc-org-hrefify-default))
                 (hrefify-string (concat "toc-org-hrefify-" hrefify-tag))
                 (hrefify (intern-soft hrefify-string))
                 (put-quote (save-match-data (string-match toc-org-quote-tag-regexp (match-string 0))))
                 (put-fold (save-match-data (string-match toc-org-fold-tag-regexp (match-string 0))))
                 (toc-prefix (if put-quote (if markdown-syntax-p "```\n" "#+BEGIN_QUOTE\n") ""))
                 (toc-suffix (if put-quote (if markdown-syntax-p "```\n" "#+END_QUOTE\n") "")))
            (if hrefify
                (let ((new-toc
                       (concat (if put-fold "#+html:<details><summary>Table of Contents</summary>\n" "")
                               toc-prefix
                               (toc-org-hrefify-toc
                                (toc-org-flush-subheadings (toc-org-raw-toc markdown-syntax-p) depth)
                                hrefify
                                markdown-syntax-p
                                (when toc-org-hrefify-hash
                                  (clrhash toc-org-hrefify-hash)))
                               toc-suffix
                               (if put-fold "#+html:</details>\n" ""))))
                  (unless dry-run
                    (newline (forward-line 1))
                    ;; skip drawers
                    (let ((end (save-excursion ;; limit to next heading
                                 (search-forward-regexp heading-symbol-regexp (point-max) 'skip))))
                      (while (re-search-forward toc-org-drawer-regexp end t)
                        (skip-chars-forward "[:space:]")))
                    (beginning-of-line)
                    ;; insert newline if TOC is currently empty
                    (when (looking-at heading-symbol-regexp)
                      (open-line 1))
                    ;; find TOC boundaries
                    (let ((beg (point))
                          (end
                           (save-excursion
                             (when (search-forward-regexp heading-symbol-regexp (point-max) 'skip)
                               (forward-line -1))
                             (end-of-line)
                             (point))))
                      ;; update the TOC, but only if it's actually different
                      ;; from the current one
                      (unless (equal (buffer-substring-no-properties beg end) new-toc)
                        (delete-region beg end)
                        (insert new-toc)))))
              (message (concat "Hrefify function " hrefify-string " is not found")))))))))

(use-package! org-contacts
  :config
  (setq org-contacts-directory (org-subdirectory "person")))

(use-package! org-ql
  :defer t
  :after denote)

(use-package! org-ql-search
  :after org-roam
  :autoload org-dblock-write:org-ql)

(use-package! org-sidebar
  :after org)

(use-package! sh-script
  :defer t
  :init
  (set-file-template! "\\.sh" :trigger "__sh" :mode 'sh-mode)
  :config
  (set-formatter! 'shfmt
    '("shfmt" "-filename" filepath "-ci" "-bn" "-sr" "-ln"
      (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))
      (when apheleia-formatters-respect-indent-level
        (list "-i"
              (number-to-string
               (cond
                (indent-tabs-mode 0)
                ((boundp 'sh-basic-offset)
                 sh-basic-offset)
                (t 4))))))
    :modes '(sh-mode))
  (defun my/bash-info-page ()
    "Go to the Bash info page."
    (interactive)
    (info "Bash")))
