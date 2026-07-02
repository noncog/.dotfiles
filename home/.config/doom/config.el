;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Initial Setup

;; Set user variables.
(when (string= user-login-name "noncog")
  (setq user-full-name "noncog"
        user-mail-address "noncog@github.com"))

;; Extend user variables.
(defconst user-home-directory (getenv "HOME")
  "Filepath of the user's home directory.")

(defconst user-system-name (string-remove-suffix ".local" (system-name))
  "Name of the user's system.")

;; Setup per-system settings using a basic wrapper around alist.
(defvar system-settings-list nil
  "An associative list holding per-system settings for variables.")

(defun system-settings-get (setting)
  "Return the first value found for `SETTING' in `system-settings-list'."
  (alist-get setting system-settings-list))

;; Update load-path to include subdirectories of the Doom config directory.
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Initialize macOS setup.
(when (featurep :system 'macos)
  ;; Unify macOS keybinds (based on current OS keybind settings) with
  ;; standard Linux layout. Keep your Linux muscle memory on a Mac.
  (setq mac-command-modifier 'control ; Maps Command -> Control
        ns-command-modifier 'control
        mac-control-modifier 'meta    ; Maps Control -> Alt (Meta)
        ns-control-modifier 'meta
        mac-option-modifier 'super   ; Maps Option -> Super
        ns-option-modifier 'super)
  ;; Update the default Bash shell to a newer one from Homebrew.
  ;; NOTE: This value is currently hard-coded. Instead, could search for
  ;;       HOMEBREW_PREFIX variable and use the expanded path if it exists.
  (when (file-exists-p "/opt/homebrew/bin/bash")
    (setq shell-file-name "/opt/homebrew/bin/bash"))
  ;; Setup the frame for rounded corners with no title, if available.
  (when (>= emacs-major-version 29)
    (add-to-list 'default-frame-alist '(undecorated-round . t))))

;;; Interface

(use-package doom-ui
  :defer t
  :init
  (setq doom-theme 'modus-vivendi-deuteranopia)
  (setq doom-font (font-spec :family "JetBrains Mono" :size 11.0)
        doom-big-font (font-spec :family "JetBrains Mono" :size 14.0)))

(use-package display-line-numbers
  :defer t
  :config
  (setq display-line-numbers-type 'visual
        display-line-numbers-grow-only t)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h))

(global-auto-revert-mode 1)       ; Revert buffer to show file changes on disk.
(global-subword-mode 1)           ; Enable iterating through camelcase words.
(setq-default x-stretch-cursor t) ; Show cursor (point) as wide as glyph under it.

;; Remap default global keybinds.

;; Move 'toggle' binds from 'help' to 'toggle' menu.
;; Disable frame fullscreen keybinds. Breaks on different window managers.
(map! "C-s-f" nil
      :leader
      "h T" nil
      "t T" #'doom/toggle-profiler
      "h t" nil
      "t t" #'load-theme
      "t F" nil
      "t o" #'doom/set-frame-opacity)

;; Configure Evil

(use-package evil-vars
  :defer t
  :config
  (setq evil-want-fine-undo t          ; Enable granular undo for evil operations.
        evil-split-window-below t      ; Emulate i3 horizontal window splits.
        evil-vsplit-window-right t     ; Emulate i3 vertical window splits.
        evil-kill-on-visual-paste nil) ; Prevent paste from adding to kill ring. Allows multiple pastes.
  (setq-default evil-scroll-count 10)) ; Reduce scroll count instead of scroll by pages.

(use-package evil-collection
  :defer t
  :config
  (setq evil-collection-calendar-want-org-bindings t))

;; Unify internal/external window management.
;; Allows use of same keybinds across window managers and Emacs windows.

(use-package windman
  :config
  (setq windman-command "i3-msg "
        windman-focus-command "focus"
        windman-move-command "move"
        windman-direction-left "left"
        windman-direction-right "right"
        windman-direction-up "up"
        windman-direction-down "down")
  (map!
   "C-s-h" #'windman-resize-win-left
   "C-s-j" #'windman-resize-win-down
   "C-s-k" #'windman-resize-win-up
   "C-s-l" #'windman-resize-win-right
   :map global-map
         "s-h" #'windman-focus-win-left
         "s-j" #'windman-focus-win-down
         "s-k" #'windman-focus-win-up
         "s-l" #'windman-focus-win-right
         "s-H" #'windman-move-win-left
         "s-J" #'windman-move-win-down
         "s-K" #'windman-move-win-up
         "s-L" #'windman-move-win-right
         "s-=" #'balance-windows
         "s-v" #'evil-window-vsplit
         "s-s" #'evil-window-split
         "s-Q" #'evil-quit
   :leader "w h" #'windman-focus-win-left
            "w j" #'windman-focus-win-down
            "w k" #'windman-focus-win-up
            "w l" #'windman-focus-win-right
            "w H" #'windman-move-win-left
            "w J" #'windman-move-win-down
            "w K" #'windman-move-win-up
            "w L" #'windman-move-win-right))

(use-package vertico
  :defer t
  :config
  ;; Add Vim scroll binds to minibuffer.
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

;;; Lookup

;; Add or remove lookup providers I don't agree with or won't use.
;; NOTE: Overwrites any conditional providers added from modules.
(setq +lookup-provider-url-alist
      '(("Doom issues"       "https://github.com/orgs/doomemacs/projects/2/views/30?filterQuery=%s")
        ;;("Doom discourse"    "https://discourse.doomemacs.org/search?q=%s")
        ;;("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
        ;;("Google images"     "https://www.google.com/images?q=%s")
        ;;("Google maps"       "https://maps.google.com/maps?q=%s")
        ("Kagi"              "https://kagi.com/search?q=%s")
        ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
        ("Debian Package" "https://packages.debian.org/search?keywords=%s")
        ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("DevDocs.io"        "https://devdocs.io/#q=%s")
        ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
        ("StackExchange"     "https://stackexchange.com/search?q=%s")
        ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
        ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
        ("Internet archive"  "https://web.archive.org/web/*/%s")
        ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
        ;; FIXME: %3 breaks string expansion for this url/provider.
        ;; ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s&title=Special%3ASearch&wprov=acrw1")
        ("AUR" "https://aur.archlinux.org/packages?O=0&K=%s")))

;;; Project Management

(use-package magit
  :defer t
  :config
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/development/projects" . 1) ; My projects.
          ("~/development/source" . 1))) ; Other's code.
  ;; Load my custom commit linter.
  (require 'magit-lint))

(use-package projectile
  :defer t
  :config
  (setq projectile-auto-discover nil
        projectile-track-known-projects-automatically nil
        projectile-project-search-path
        '(("~/.dotfiles" . 0)
          ("~/development/projects" . 1) ; My projects.
          ("~/development/source" . 1))) ; Other's code.
  (map! :map project-prefix-map
        :leader :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects))

;;; Org

(use-package org
  :defer t
  :init
  (setq org-directory "~/documents/org/")
  (defvar org-data-directory (expand-file-name "data/" org-directory)
    "A directory used to hold data files related to org.")
  (defvar org-inbox-directory (expand-file-name "inbox/" org-directory)
    "A directory used to hold `org-capture' items.")
  (defvar org-inbox-file (expand-file-name (concat user-system-name ".org") org-inbox-directory)
    "Inbox file to use with `org-capture'. Uses system name to avoid sync conflicts.")
  :config
  ;; Appearance
  (setq org-hide-leading-stars t                        ; Hide leading heading stars.
        org-ellipsis " ▾ ")                             ; Use UTF-8 to indicate a folded heading.
  ;; Modules
  (add-to-list 'org-modules 'org-habit t)               ; Enable org-habit for tracking repeated actions.
  (add-to-list 'org-modules 'ol-man t)                  ; Enable links to man pages.
  (add-to-list 'org-modules 'ol-info t)                 ; Enable links to info pages.
  ;; Task Management
  (setq org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)")))
  ;; Logging
  (setq org-log-into-drawer t                           ; Log times into a drawer to hide them.
        org-log-reschedule t                            ; Log rescheduling of scheduled items.
        org-log-redeadline t                            ; Log rescheduling of deadline items.
        org-log-states-order-reversed nil               ; Log times reverse chronologically.
        org-treat-insert-todo-heading-as-state-change t ; Enable logging on `org-insert-todo-heading'.
        org-log-done 'time                              ; Add completion time to DONE items.
        ))

(use-package org-id
  :defer t
  :config
  (setq org-id-locations-file (expand-file-name "org.id" org-data-directory)
        org-id-locations-file-relative t                ; Use relative references for cross-platform compatibility.
        org-id-track-globally t                         ; Track identifiers in all org files so id links always work.
        org-id-method 'ts                               ; Use timestamps for unique identifiers.
        org-id-ts-format "%Y%m%dT%H%M%S"))              ; ISO-8601 timestamp format for identifiers.

(use-package vulpea
  :defer t
  :hook ((after-init . vulpea-db-autosync-mode))
  :init
  ;; TODO: Investigate if setq-default should be used.
  (setq vulpea-db-sync-directories (list org-directory)
        vulpea-db-location (expand-file-name "vulpea.db" org-data-directory)
        ;; FIXME: If directory does not exist, it won't create the file.
        vulpea-default-notes-directory (expand-file-name "notes/" org-directory)
        vulpea-db-index-heading-level t                 ; Index heading level notes.
        vulpea-db-sync-scan-on-enable 'async)           ; Automatically scan on enable.
  :config
  (setq vulpea-buffer-alias-property "ALIASES"))

;; Setup "notes" keybinds.

;; From: [[file:~/.config/emacs/modules/config/default/+evil-bindings.el:::prefix-map ("n" . "notes"]]
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

(use-package agent-shell
  ;; investigate if after evil needs to happen.
  :defer t
  :config
  (setq agent-shell-preferred-agent-config (agent-shell-opencode-make-agent-config)
        agent-shell-opencode-default-model-id "llama.cpp/unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_M"
        agent-shell-opencode-default-session-mode-id "plan")
  ;; Fix (Re-bind) broken default binds on Doom.
  (map! :map agent-shell-mode-map
        :m [tab] #'agent-shell-next-item
        :n [return] #'agent-shell-ui-toggle-fragment
        :localleader :desc "Cycle session mode"
        "m"  #'agent-shell-cycle-session-mode))
