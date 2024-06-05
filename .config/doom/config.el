;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Variables

(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(setq +lookup-provider-url-alist
      '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
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

;; Appearance

(setq doom-theme 'doom-vibrant)

(setq doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-big-font (font-spec :family "JetBrains Mono" :size 14))

(setq-default x-stretch-cursor t)

;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'default-frame-alist '(undecorated-round . t))

(when (and IS-MAC (>= emacs-major-version 29)) (setq frame-inhibit-implied-resize '(font font-backend tab-bar-lines)))

(setq display-line-numbers-type 'visual)

(dolist (mode '(org-mode-hook))
      (add-hook mode #'doom-disable-line-numbers-h))

(setq display-line-numbers-grow-only t)

;; Behavior

(setq evil-want-fine-undo t)

(global-subword-mode 1)

(setq evil-kill-on-visual-paste nil)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(global-auto-revert-mode 1)

(setq-default evil-scroll-count 10)

(defun wm-win-cmd-in-direction (wm-cmd direction emacs-fn)
  (let ((tell-wm (concat (cond (IS-LINUX "i3-msg ") (IS-MAC "yabai -m window --")) wm-cmd " " direction)))
    (condition-case nil (funcall emacs-fn)
      (user-error (apply #'start-process "wm" nil (split-string tell-wm))))))

(defun wm-focus-win-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-left)))

(defun wm-focus-win-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-right)))

(defun wm-focus-win-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-up)))

(defun wm-focus-win-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-down)))

(defun wm-move-win-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-left)))

(defun wm-move-win-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-right)))

(defun wm-move-win-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-up)))

(defun wm-move-win-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-down)))

(defun wm-resize-win-left ()
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally 10) (enlarge-window-horizontally -10)))

(defun wm-resize-win-right ()
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally -10) (enlarge-window-horizontally 10)))

(defun wm-resize-win-up ()
  (interactive)
  (if (window-in-direction 'above nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(defun wm-resize-win-down ()
  (interactive)
  (if (window-in-direction 'below nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(map! "s-h" #'wm-focus-win-left
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
      "s-Q" #'evil-quit)

(map! :leader "w h" #'wm-focus-win-left
              "w j" #'wm-focus-win-down
              "w k" #'wm-focus-win-up
              "w l" #'wm-focus-win-right
              "w H" #'wm-move-win-left
              "w J" #'wm-move-win-down
              "w K" #'wm-move-win-up
              "w L" #'wm-move-win-right)

(map! "C-s-h" #'wm-resize-win-left
      "C-s-j" #'wm-resize-win-down
      "C-s-k" #'wm-resize-win-up
      "C-s-l" #'wm-resize-win-right)

;; Keybinds

(when IS-MAC (setq mac-command-modifier 'control ; Maps Command -> Control
                    mac-control-modifier 'meta   ; Maps Control -> Alt (Meta)
                    mac-option-modifier 'super)) ; Maps Option -> Super

(map! :leader "h t" nil
      :leader "t t" #'load-theme
      :leader "h T" nil
      :leader "t T" #'doom/toggle-profiler
      :leader :desc "Menu-bar mode" "t m" #'toggle-menu-bar-mode-from-frame)

(when IS-MAC
  (map! "C-s-f" nil ; Frame fullscreen toggle is broken on macOS.
   :leader "t F" nil))

(use-package! avy
  :defer t
  :config
  ;; Behavior
  (setq avy-all-windows t))

(use-package! beacon
  :config
  ;; Appearance
  (setq beacon-color "#61bfff")
  (setq beacon-size 40
        beacon-blink-duration 0.3
        beacon-blink-delay 0.5)
  ;; Behavior
  (setq beacon-blink-when-buffer-changes nil
        beacon-blink-when-window-scrolls nil
        beacon-blink-when-point-moves-vertically 10
        beacon-blink-when-focused nil) ; Does not work with Doom/Yabai.
  (beacon-mode 1))

(use-package! helpful
  :defer t
  :config
  ;; Behavior
  (setq helpful-max-buffers nil)
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
      (select-window origin)))
  ;; Keybinds
  (map! :map helpful-mode-map "C-h" #'winner-undo
        :map helpful-mode-map "C-l" #'winner-redo))

(use-package! which-key
  :defer t
  :config
  ;; Behavior
  (setq which-key-idle-delay 0.5)
  ;; Keybinds
  (setq which-key-use-C-h-commands t
        prefix-help-command #'which-key-C-h-dispatch)
  
  (defadvice! fix-which-key-dispatcher-a (fn &rest args)
    :around #'which-key-C-h-dispatch
    (let ((keys (this-command-keys-vector)))
      (if (equal (elt keys (1- (length keys))) ?\?)
          (let ((keys (which-key--this-command-keys)))
            (embark-bindings (seq-take keys (1- (length keys)))))
        (apply fn args))))
  ;; Fixes
  (setq which-key-allow-imprecise-window-fit nil)
  ;; (defun add-which-key-line (f &rest r) (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))))
  ;; (advice-add 'which-key--show-popup :around #'add-which-key-line)
  (setq which-key-allow-multiple-replacements t)
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1")))
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`org-agenda-\\(.*\\)") . (nil . " \\1")))
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`treemacs-\\(.*\\)") . (nil . " \\1"))))

(use-package! doom-modeline
  :defer t
  :config
  ;; Appearance
  (setq doom-modeline-height 35)
  (setq doom-modeline-buffer-file-true-name t
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p))
  (setq doom-modeline-vcs-max-length 60
        auto-revert-check-vc-info t)
  (setq doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t
        doom-modeline-persp-icon t))

(use-package! persp-mode
  :defer t
  :config
  ;; Fixes
  (setq persp-emacsclient-init-frame-behaviour-override nil))

(use-package! vertico
  :defer t
  :config
  ;; Behavior
  (setq vertico-sort-function #'vertico-sort-history-alpha)
  ;; Keybinds
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command))

(use-package! marginalia
  :defer t
  :config
  ;; Appearance
  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))
  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))
  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package! company
  :defer t
  :config
  ;; Behavior
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 1)
  (set-company-backend! 'org-mode
    '(company-capf company-files :with company-yasnippet))
  (set-company-backend! 'sh-mode
    '(company-shell company-files :with company-yasnippet)))

(use-package! yasnippet
  :defer t
  :config
  ;; Behavior
  (setq yas-triggers-in-field t))

(use-package! projectile
  :defer t
  :init
  ;; Keybinds
  (map! :map project-prefix-map
        :leader
        :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects)
  :config
  ;; Variables
  (setq projectile-project-search-path '("~/Projects"))
  ;; Behavior
  (setq projectile-auto-discover nil
        projectile-track-known-projects-automatically nil))

(use-package! magit
  :defer t
  :config
  ;; Behavior
  (setq magit-repository-directories
        '(("~/Projects" . 1)))
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
               summary-title-not-end-in-punctuation)
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
  (defvar commit-types-file "~/.config/git/commit/commit-types"
    "A list of commit types to be checked by: TODO: link functions.")
  (defun get-commit-types ()
    "Return a list of commit types."
    (let ((file-path commit-types-file))
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t))))
  (defvar imperative-verbs-file "~/.config/git/commit/imperative-verbs"
    "A list of imperative verbs used in git commits to be checked by: TODO: link functions.")
  (defun get-imperative-verbs ()
    "Return a list of imperative verbs."
    (let ((file-path imperative-verbs-file))
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t))))
  (setq git-commit-summary-max-length 50) ; Maximum title (summary) length.
  (setq git-commit-fill-column 72)        ; Description column limit.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'git-commit-finish-query-functions
               #'my-git-commit-check-style-conventions)
  (defun my/magit-edit-bare-dotfile ()
    "Select a dotfile to edit."
    (interactive)
    (let ((default-directory (expand-file-name "~/")))
      (find-file (concat default-directory "/" (magit-read-file-from-rev "HEAD" "Find .dotfile")))))
  
  (map! :leader :desc "Edit .dotfiles" "g d" #'my/magit-edit-bare-dotfile))

(use-package! vterm
  :defer t
  :init
  ;; Variables
  (when (and IS-MAC (file-exists-p "/opt/homebrew/bin/bash")) (setq vterm-shell "/opt/homebrew/bin/bash"))
  :config
  ;; Keybinds
  (evil-define-key* 'insert vterm-mode-map (kbd "<M-backspace>") #'vterm-send-meta-backspace)
  )

(use-package! visual-fill-column
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  ;:hook (org-mode . visual-fill-column-mode)
  :config
  ;; Fixes
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package! org
  :defer t
  :init
  ;; Variables
  (setq org-directory (file-truename "~/Documents/notes"))
  (remove-hook 'org-mode-hook #'+org-make-last-point-visible-h)
  :config
  (setq org-todo-keywords
        '((sequence
           "EVENT(e!)"    ; An event.
           "|"
           "DONE(d!)"     ; Successfully completed.
           "KILL(k@/!)")  ; Cancelled or not applicable.
          (sequence
           "MEET(m!)"     ; A meeting.
           "|"
           "DONE(d!)"     ; Successfully completed.
           "KILL(k@/!)")  ; Cancelled or not applicable.
          (sequence
           "APPT(a!)"     ; An appointment.
           "|"
           "DONE(d!)"     ; Successfully completed.
           "KILL(k@/!)")  ; Cancelled or not applicable.
          (sequence
           "TODO(t!)"     ; Task that needs doing & is ready to do.
           "NEXT(N!)"     ; Task that needs doing & is ready to do.
           "WAIT(w@/!)"   ; Something is holding this up.
           "|"            ; Required to get org-roam to ignore the following todo items.
           "DONE(d!)"     ; Task successfully completed.
           "KILL(k@/!)")  ; Task cancelled or not applicable.
          (type
           "|"            ; Required to get org-roam to ignore the following todo items.
           "ISSUE(I!)"    ; An issue.
           "IDEA(i!)"     ; An idea.
           "NOTE(n!)")    ; A fleeting note, in person, idea, or link.
          (type
           "QUESTION(q!)" ; A question that I want to be able to hold information about.
           "|"
           "ANSWER(q!)"   ; A question that I want to be able to hold information about.
           ))
        org-todo-keyword-faces
        '(("ISSUE" . +org-todo-project)
          ("NEXT" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("KILL" . +org-todo-cancel)
          ("NOTE" . +org-todo-active)
          ("LINK" . +org-todo-active)
          ("QUESTION" . +org-todo-active)))
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-tag-alist
        '(("agenda")
          ("person")
          ("project")))
  ;; Appearance
  (setq org-hide-emphasis-markers t            ; Hide syntax for emphasis. (Use org-appear)
        org-pretty-entities t                  ; Show sub/superscript as UTF8.
        org-src-preserve-indentation t)        ; Don't change whitespace in source blocks.
  (setq org-ellipsis " ▾ "                     ; Use a custom ellipsis for folded headings.
        org-hide-leading-stars t               ; Remove excess heading stars.
        org-hidden-keywords nil)               ; Can use to hide certain keywords.
  (setq org-image-actual-width '(0.9)          ; Use an in-buffer width closer to export's
        org-startup-with-inline-images t)      ; Show images at startup.
  (setq org-startup-with-latex-preview nil)    ; Show rendered LaTeX in the buffers.
  (setq org-highlight-latex-and-related '(native script entities))
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  ;; Behavior
  (setq org-imenu-depth 10                     ; Allow imenu to search deeply in org docs.
        org-use-property-inheritance t         ; Sub-headings inherit parent properties.
        org-insert-heading-respect-content nil ; Insert heading here, not at end of list.
        org-use-fast-todo-selection 'auto)     ; Method to select TODO heading keywords.
  (setq org-log-done 'time                     ; Add completion time to DONE items.
        org-log-into-drawer t                  ; Log times into a drawer to hide them.
        org-log-reschedule t                   ; Log rescheduling of scheduled items.
        org-log-redeadline t                   ; Log rescheduling of deadline items.
        org-log-refile t                       ; Log when a heading is refiled.
        ;org-log-repeat t
        org-log-states-order-reversed nil)     ; Log state changes chronologically.
  (setq org-list-allow-alphabetical t)         ; Enable use of alphabet as list bullets.
  (setq org-return-follows-link t)             ; Pressing enter opens links.
  (add-to-list 'org-modules 'org-habit t)      ; Enable repeated task tracking/graphing!
  (setq evil-collection-calendar-want-org-bindings t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (defun my/org-remove-all-tags ()
    "Remove all tags for the current Org heading."
    (interactive)
    (org-set-tags nil))
  (defun my/org-capture-print-help-link ()
    "Prints a link as a result for use in an org-capture result function."
    (interactive)
    (let* ((link (pop org-stored-links))
          (left (car link))
          (right (cadr link)))
    (format "[[%s][%s]]" left right)))
  
  (defun my/org-store-help-link (symbol)
    "Select symbol from prompt or point and insert link to help page for it."
      (push (list (format "help:%s" symbol) symbol) org-stored-links))
  
  (defun my/org-insert-last-link (symbol)
    "Select symbol from prompt or point and insert link to help page for it."
      (my/org-store-help-link symbol)
      (org-insert-all-links 1 "" ""))
  
  (defun my/org-insert-symbol-link ()
    "Select symbol from prompt or point and insert link to help page for it."
    (interactive)
    (let ((symbol (symbol-name (helpful--read-symbol
                                "Symbol: " (helpful--symbol-at-point)
                                #'helpful--bound-p))))
      (my/org-insert-last-link symbol)))
  (map! :map (embark-symbol-map
              embark-variable-map
              embark-function-map
              embark-command-map)
        "I" #'my/org-insert-help-link
        "l" #'my/org-store-help-link)
  (setq org-use-tag-inheritance t)
  (add-to-list 'org-tags-exclude-from-inheritance "agenda")
  (add-to-list 'org-tags-exclude-from-inheritance "project"))

(use-package! denote
  :after org
  :init
  ;; Variables
  (setq denote-directory org-directory)
  :config
  ;; Behavior
  (setq org-id-ts-format denote-id-format
        org-id-method 'ts)
  (setq denote-org-front-matter ":PROPERTIES:\n:ID: %4$s\n:DATE: %2$s\n:END:\n#+title: %1$s\n#+filetags: %3$s\n")
  ;;(setq denote-dired-directories-include-subdirectories t)
  )

(use-package! org-roam
  :defer t
  :after denote ;; Wait until Denote loads to integrate Org Roam with it.
  :init
  ;; Variables
  (setq org-roam-directory org-directory)
  (setq org-roam-db-location (expand-file-name ".org.db" org-roam-directory))
  :config
  ;; Appearance
  ;(setq org-roam-node-display-template)
  (defun my/doom-modeline-process-buffer-file-name (file-name)
    "Return buffer-file-name filtered if necessary to display in Doom Modeline."
      (when (s-contains-p org-directory file-name)
               (replace-regexp-in-string
                (concat denote-id-regexp "--") ""
                (replace-regexp-in-string denote-keywords-regexp ".org" file-name))))
  
  (setq doom-modeline-buffer-file-name-function #'my/doom-modeline-process-buffer-file-name)
  (setq doom-modeline-buffer-file-truename-function #'my/doom-modeline-process-buffer-file-name)
  ;; Behavior
  ;; TODO: Check what each slug function does and minimize it. Currently doing excess character removing/checks in both functions.
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                        ("__*" . "_")                   ;; remove sequential underscores
                        ("^_" . "")                     ;; remove starting underscore
                        ("_$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (denote-sluggify 'title slug)))))
  ;; TODO: Somehow ensure that Denote's keyword (tag) front-matter is being added too!
  (setq org-roam-extract-new-file-path (concat denote-id-format "--${slug}.org"))
  ;; NOTE: This doesn't need to be named org-roam-
  (defun my/org-roam-agenda-file-p ()
    "Return non-nil if current buffer has todo keywords or scheduled items.
  TODO entries marked as done are ignored, meaning the this
  function returns nil if current buffer contains only completed
  tasks. The only exception is headings tagged as REFILE."
  ;; TODO: Add ignoring of certain directories/files.
    (interactive)
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (let ((todo-type (org-element-property :todo-type h)))
          (or
           ;; any headline with some todo keyword
           (eq 'todo todo-type)
           ;; any headline with REFILE tag (no inheritance)
           (seq-contains-p (org-element-property :tags h) "REFILE")
           ;; any non-todo headline with an active timestamp
           (and
            (not (eq 'done todo-type))
            (org-element-property :contents-begin h)
            (save-excursion
              (goto-char (org-element-property :contents-begin h))
              (let ((end (save-excursion
                           ;; we must look for active timestamps only
                           ;; before then next heading, even if it's
                           ;; child, but org-element-property
                           ;; :contents-end includes all children
                           (or
                            (re-search-forward org-element-headline-re
                                               (org-element-property :contents-end h)
                                               ':noerror)
                            (org-element-property :contents-end h)))))
                (re-search-forward org-ts-regexp end 'noerror)))))))
      nil 'first-match))
  (defvar my/org-roam-sort-filetags t
    "Boolean to decide if org roam's filetags should be sorted.")
  
  (defun my/org-roam-update-filetags ()
    "Add or remove FILETAGS to the current node based on conditions."
    ;; TODO: Keep org tag alist up-to-date with org-roam tags.
    ;; TODO: Force length check for one or more tags using seq-length.
    (let* ((title (org-roam--get-keyword "title"))
           (title-as-tag (concat "@" (s-replace " " "" title)))
           (get-filetags (org-roam--get-keyword "filetags"))
           (filetags (if get-filetags (split-string get-filetags ":" 'omit-nulls) nil))
           (new-tags filetags)
           (add-tags (list))
           (remove-tags (list)))
      ;; Ensure the agenda tag is sest when appropriate. See my/org-roam-agenda-file-p
      (if (my/org-roam-agenda-file-p)
          (add-to-list 'add-tags "agenda" )
        (add-to-list 'remove-tags "agenda"))
      ;; Ensure that @PersonName tag is set when node is tagged as a person.
      (if (seq-contains-p filetags "person")
          (add-to-list 'add-tags title-as-tag)
        (add-to-list 'remove-tags title-as-tag))
      ;; Update the list of filetags.
      (setq new-tags (seq-union new-tags add-tags))
      (setq new-tags (seq-difference new-tags remove-tags))
      (when (not (seq-set-equal-p filetags new-tags))
        (setq new-tags (seq-uniq new-tags)))
      ;; Apply latest filetags to node.
      (org-roam-set-keyword
       "filetags" (org-make-tag-string
                   (if my/org-roam-sort-filetags
                       (sort new-tags #'string<)
                       new-tags)))
      (denote-rename-file-using-front-matter (buffer-file-name))))
  (defun my/org-roam-get-agenda-files ()
    "Return a list of node files containing the 'agenda' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"agenda\"%"))]))))
  (defun my/org-roam-update-agenda-files (&rest _)
    "A hook function to update the value of `org-agenda-files'."
    (setq org-agenda-files (my/org-roam-get-agenda-files)))
  (defun my/org-roam-pre-save-hook ()
    "Update 'org-agenda-files' when org-roam file is saved."
    (when (and (not (active-minibuffer-window))
               (org-roam-buffer-p))
      (progn (my/org-roam-update-filetags)
             (my/org-roam-update-agenda-files))))
  (defun my/org-roam-node-insert-hook (id link-description)
    "Update @PersonName tag when linking to their node in a task."
    (let* ((node (org-roam-node-from-id id))
           (filetags (org-roam-node-tags node))
           (title (concat "@" (s-replace " " "" (org-roam-node-title node)))))
      (when (seq-contains-p filetags "person")
          (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (when (eq 'todo (org-element-property
                             :todo-type
                             (org-element-at-point)))
              (org-set-tags (seq-uniq (cons title (org-get-tags nil t))))))))))
  (add-hook 'org-roam-post-node-insert-hook #'my/org-roam-node-insert-hook)
  (add-hook 'before-save-hook #'my/org-roam-pre-save-hook)
  ;; TODO: See if it's necessary to run this hook in both before-save-hook and in org's agenda functions.
  (advice-add 'org-agenda :before #'my/org-roam-update-agenda-files)
  (advice-add 'org-todo-list :before #'my/org-roam-update-agenda-files)
  ;; TODO: Look into other functions to hook.
  (defun my/org-roam-agenda-files ()
    "Select from and visit an Org Roam node tagged with 'agenda'"
    (interactive)
    (org-roam-node-find nil "agenda"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (org-roam-list-files :maxlevel . 5)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12(my/org-roam-agenda-category)%?-12t% s")
          (todo . " %i %-12(my/org-roam-agenda-category) ")
          (tags . " %i %-12(my/org-roam-agenda-category) ")
          (search . " %i %-12(my/org-roam-agenda-category) ")))
  
  (defun my/org-roam-agenda-category ()
    "Get category of item at point for agenda.
  
  Category is defined by one of the following items:
  
  - CATEGORY property
  - TITLE keyword
  - TITLE property
  - filename without directory and extension
  
  Usage example:
  
    (setq org-agenda-prefix-format
          '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))
  
  Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (org-roam--get-keyword "title"))
           (category (org-get-category)))
      (or (if (and title (string-equal category file-name)) title category) "")))
  ;; TODO: Consider shifting off args like bash and returning the rest.
  ;; TODO: Consider adding a toggle to enable/disable.
  (defun my/org-roam-node-find-advice (&optional OTHER-WINDOW INITIAL-INPUT FILTER-FN PRED &key TEMPLATES)
      "Returns a list of formatted args for org-roam-node-find to use as defaults."
      '(nil "!private "))
  (advice-add 'org-roam-node-find :filter-args #'my/org-roam-node-find-advice)
  ;; Keybinds
  (defun noncog/org-roam-is-draft-p (node)
    "Is this org-roam node a draft?"
    (member "draft" (org-roam-node-tags node)))
  (defun noncog/org-roam-random-draft ()
    "Get a random node with the draft tag. "
    (interactive)
    (org-roam-node-random nil #'noncog/org-roam-is-draft-p))
  (map! :leader :desc "Random draft node" "n r u" #'noncog/org-roam-random-draft)
  (makunbound 'org-roam-add-property))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :init
  (defvar my/+lookup--xwidget-webkit-last-session-buffer nil)
  (defun my/+lookup-xwidget-webkit-open-url-fn (url &optional new-session)
    (if (not (display-graphic-p))
        (browse-url url)
      (unless (featurep 'xwidget-internal)
        (user-error "Your build of Emacs lacks Xwidgets support and cannot open Xwidget WebKit browser"))
      (let ((orig-last-session-buffer (if (boundp 'xwidget-webkit-last-session-buffer)
                                          xwidget-webkit-last-session-buffer
                                        nil)))
        (setq xwidget-webkit-last-session-buffer my/+lookup--xwidget-webkit-last-session-buffer)
        (save-window-excursion
          (xwidget-webkit-browse-url url new-session))
        (display-buffer xwidget-webkit-last-session-buffer)
        (setq my/+lookup--xwidget-webkit-last-session-buffer xwidget-webkit-last-session-buffer
              xwidget-webkit-last-session-buffer orig-last-session-buffer))))
  ;; Keybinds
  (map! :leader "n r g" #'org-roam-ui-open)
  :config
  ;; Appearance
  (setq org-roam-ui-sync-theme t)
  ;; Behavior
  (setq org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start nil)
        ;org-roam-ui-browser-function #'my/+lookup-xwidget-webkit-open-url-fn)
  (defun my/+org-roam-ui-popup-kill (roam-ui-popup-buffer)
    (progn (+popup--kill-buffer roam-ui-popup-buffer 0.1) (org-roam-ui-mode -1)))
  ;(setq display-buffer-mark-dedicated t)
  ;; (set-popup-rule! "\\*xwidget"
  ;;   :side 'right :width 0.33 :height 0.5 :ttl #'my/+org-roam-ui-popup-kill :slot -1 :quit nil :modeline nil)
  ;; (defadvice! my/+org-roam-ui--on-msg-open-node (data)
  ;;   "Open a node CAREFULLY when receiving DATA from the websocket."
  ;;   :override #'org-roam-ui--on-msg-open-node
  ;;   (let* ((id (alist-get 'id data))
  ;;          (node (org-roam-node-from-id id))
  ;;          (pos (org-roam-node-point node))
  ;;          (buf (find-file-noselect (org-roam-node-file node))))
  ;;     (run-hook-with-args 'org-roam-ui-before-open-node-functions id)
  ;;     (unless (window-live-p org-roam-ui--window)
  ;;       (if-let ((windows (cl-set-difference (doom-visible-windows) (list (minibuffer-window))))
  ;;                (newest-window (car (seq-sort-by #'window-use-time #'> windows))))
  ;;           (setq org-roam-ui--window newest-window)
  ;;         (setq org-roam-ui--window newest-window)))
  ;;     (set-window-buffer org-roam-ui--window buf)
  ;;     (select-window org-roam-ui--window)
  ;;     (goto-char pos)
  ;;     (run-hook-with-args 'org-roam-ui-after-open-node-functions id)))
  ;; (defadvice! +org-roam-ui-always-sync-theme ()
  ;;   "Always sync da theme..."
  ;;   :after #'org-roam-ui-open
  ;;   (while (when (and org-roam-ui-mode (ignore-errors (get-buffer-window-list "*xwidget webkit: ORUI *"))) (websocket-send-text org-roam-ui-ws-socket (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))))
  )

(use-package! org-sidebar
  :after org)

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
             ;(org-agenda-overriding-header "Today\n")
             (org-agenda-overriding-header " Agenda\n")
             (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
             (org-agenda-block-separator nil)
             (org-agenda-format-date " %a, %b %-e")  ; american date format
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
             ;; TODO: Fix inbox not-skipping... Since I no longer have that tag.
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
             (org-agenda-prefix-format '((agenda . " %8:c %-5t ")))
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
  ;;(set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline nil :select t :quit t)
  (set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline nil :select t :quit 'current)
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
  )

(use-package! org-super-agenda
  :after org-agenda)

(use-package! org-ql
  :defer t
  :after denote)

(use-package! org-ql-search
  :after org-roam
  :autoload org-dblock-write:org-ql)

(use-package! org-capture
  :defer t
  ;;:init
  ;;
  :config
  ;; Variables
  ;(setq +org-capture-fn #'org-roam-capture)
  (setq org-capture-templates
        `(("t" "Task" entry
           (file+headline "inbox.org" "Tasks")
           "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n%i" :prepend t)
          ("i" "Issue" entry
           (file+headline "inbox.org" "Issues")
           "* ISSUE %?\n:LOGBOOK:\n- State \"ISSUE\"       from              %U\n:END:\n%i" :prepend t)
          ("n" "Note" entry
           (file+headline "inbox.org" "Notes")
           "* NOTE %?\n:LOGBOOK:\n- State \"NOTE\"       from              %U\n:END:\n%i" :prepend t)
          ("I" "IDEA" entry
           (file+headline "inbox.org" "Ideas")
           "* IDEA %?\n:LOGBOOK:\n- State \"IDEA\"       from              %U\n:END:\n%i" :prepend t)
          ("?" "Question" entry
           (file+headline "inbox.org" "Questions")
           "* QUESTION %?\n:LOGBOOK:\n- State \"QUESTION\"       from              %U\n:END:\n%i" :prepend t)
          ("b" "Bookmark" entry
           (function org-bookmark-location)
           "* %(org-bookmark-format-link)\n:PROPERTIES:\n:DATE: %U\n:END:\n%i%?" :prepend t :immediate-finish t)
          ("a" "Appointment" entry
           (file+headline "inbox.org" "Appointments")
           "* APPT %?\n:LOGBOOK:\n- State \"APPT\"       from              %U\n:END:\n%i" :prepend t)
          ("m" "Meeting" entry
           (file+headline "inbox.org" "Meetings")
           "* MEET %?\n:LOGBOOK:\n- State \"MEET\"       from              %U\n:END:\n%i" :prepend t)
          ("e" "Event" entry
           (file+headline "inbox.org" "Events")
           "* EVENT %?\n:\n:LOGBOOK:\n- State \"EVENT\"       from              %U\n:END:\n%i" :prepend t)
          ("s" "Symbol" table-line
             (function org-bookmark-symbol)
             "|%(my/org-capture-print-help-link)|%?||" :table-line-pos "III-1" :immediate-finish t)))
  (set-popup-rule! "^*Capture*$" :side 'bottom :height 1 :select nil :autosave 'ignore)
  
  (set-popup-rule! "^CAPTURE-.*$" :side 'bottom :height 0.3 :vslot -1 :quit nil :select t :autosave 'ignore)
  ;; Behavior
  (setq org-capture-bookmark nil)
  ;; Fixes
  ;; (defadvice! my/+org--restart-mode-h-careful-restart (fn &rest args)
  ;;   :around #'+org--restart-mode-h
  ;;   (let ((old-org-capture-current-plist
  ;;          (and (bound-and-true-p org-capture-mode)
  ;;               (bound-and-true-p org-capture-current-plist))))
  ;;     (apply fn args)
  ;;     (when old-org-capture-current-plist
  ;;       (setq-local org-capture-current-plist old-org-capture-current-plist)
  ;;       (org-capture-mode +1))))
  ;; (add-hook! 'org-capture-after-finalize-hook (org-element-cache-reset t))
  )

(use-package! org-roam-capture
  :defer t
  ;; :if (file-exists-p org-directory) ; Only load if the directory exists.
  :config
  (setq org-roam-capture-templates
        '(("n" "node" plain "%?"
           :target (file+head "node/%<%Y%m%dT%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (defun my/org-roam-file-property-p (property)
    (let ((has-prop (org-find-property property)))
      (when has-prop (org-with-point-at has-prop (org-before-first-heading-p)))))
  
  (defun my/org-roam-add-new-property-file-date ()
    "Set the 'DATE' file property using the 'buffer-file-name'."
    (interactive)
    (save-excursion
      (unless (my/org-roam-file-property-p "DATE")
        (org-set-property "DATE" (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time (denote-retrieve-filename-identifier (buffer-file-name))))))))
  
  ;; TODO: Add ability to modify category based on other contexts such as log files.
  ;; - This should ultimately work based off of a parent directory name list. Or use manual correction...
  
  (defun my/org-roam-add-new-property-category ()
    "Set the 'CATEGORY' file property using the 'buffer-file-name'."
    (interactive)
    (save-excursion
      (unless (my/org-roam-file-property-p "CATEGORY")
        (org-set-property "CATEGORY" (org-get-title)))))
  
  (add-hook 'org-roam-capture-new-node-hook #'my/org-roam-add-new-property-file-date 100)
  (add-hook 'org-roam-capture-new-node-hook #'my/org-roam-add-new-property-category 100)
  )

(use-package! org-roam-protocol
  :defer t
  :config
  ;; Behavior
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :target (file+head "resource/%<%Y%m%dT%H%M%S>--${slug}.org" "#+title: ${title}\n\n${body}")
          :unnarrowed t)))
  (setq org-roam-protocol-store-links t))

(defun my/org-protocol-make-app-handler ()
  "Generate the Applescript application 'OrgProtocol.app' using Applescript and elisp."
  ;; TODO: Create options to change where the application gets created.
  (interactive)
  ;; TODO: Consider prompting are you sure?
  (let* ((script (expand-file-name "OrgProtocolClient.applescript" doom-user-dir))
         (app "/Applications/OrgProtocolClient.app")
         ;; NOTE: Once you've made the link succesffuly you should be able to update the app without a problem, otherwise, recursively run 'lsregister' until there's no app for that command. Could automate that eventually.
         (plist (expand-file-name "Contents/Info.plist" app))
         (emacsclient "/opt/homebrew/bin/emacsclient -n "))
    (with-temp-file script
      (insert "on open location this_URL\n")
      (insert (format "\tset EC to \"%s\"\n" emacsclient))
      (insert "\tset filePath to quoted form of this_URL\n")
      (insert "\tdo shell script EC & filePath\n")
      (insert "\ttell application \"Emacs\" to activate\n")
      (insert "end open location\n"))
    ;; Must be done for osacompile to recognize application extension.
    (when (file-exists-p app) (delete-directory app t))
    (let ((status (call-process-shell-command (format "osacompile -o %s %s" app script))))
      (if (eq status 0)
          (with-temp-file plist
            (when (file-exists-p script) (delete-file script t))
            (when-let  ((plist-buffer (get-file-buffer plist)))
              (kill-buffer plist-buffer))
            (insert-file-contents plist)
            (goto-char (point-max))
            (goto-char (re-search-backward "</dict>"))
            (previous-line) (end-of-line) (newline)
            (insert "\t<key>CFBundleURLTypes</key>\n")
            (insert "\t<array>\n")
            (insert "\t\t<dict>\n")
            (insert "\t\t\t<key>CFBundleURLName</key>\n")
            (insert "\t\t\t<string>org-protocol handler</string>\n")
            (insert "\t\t\t<key>CFBundleURLSchemes</key>\n")
            (insert "\t\t\t<array>\n")
            (insert "\t\t\t\t<string>org-protocol</string>\n")
            (insert "\t\t\t</array>\n")
            (insert "\t\t</dict>\n")
            (insert "\t</array>")
            (write-region nil nil plist))
        (error "Something wen't wrong with the compilation of the script!")))))

(use-package! org-roam-dailies
  :after org-roam
  :init
  ;; Variables
  (setq org-roam-dailies-directory "log/")
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y%m%dT%H%M%S>--log.org" "#+title: Log\n"))))
  )

(defvar org-bookmark--format-hash (make-hash-table :test #'equal)
  "A hash of (DOMAIN TITLE-FORMATTER) to be applied to link titles.")

(defun org-bookmark-add-link-formatter (domain title-transformer)
  "Adds a DOMAIN and associated TITLE-TRANSFORMER to the org-bookmark--format-hash"
  (puthash domain title-transformer org-bookmark--format-hash))

(defun org-bookmark-replace-in-link-title (regex replacement)
  "Replace REGEX of a link's title with REPLACEMENT."
  (lambda (title)
    (replace-regexp-in-string regex replacement title)))

(defun org-bookmark-format-stored-link ()
  "Return a pretty-printed top of `org-stored-links'."
  (let* ((link (caar org-stored-links))
         (title (cl-cadar org-stored-links))
         (parsed-link (url-generic-parse-url link))
         (domain (concat (url-type parsed-link) "://" (url-host parsed-link)))
         (formatter (gethash domain org-bookmark--format-hash)))
    (progn (unless org-link-keep-stored-after-insertion (pop org-stored-links))
    (org-link-make-string link (if formatter (funcall formatter title) title)))))

(defun org-bookmark-format-link ()
  (if org-stored-links
      (org-bookmark-format-stored-link)
      (when (fboundp 'org-cliplink-capture) (org-cliplink-capture))))

(defvar org-bookmark-location-handlers nil
  "List of bookmark location handlers by priority.

Each item is a function of zero arguments that opens an
appropiriate file/line and returns non-nil on match.")

(defun org-bookmark-handler-file-heading (file heading)
  "Open or create the FILE and HEADING to insert a bookmark at."
  (set-buffer (org-capture-target-buffer file))
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode"
             (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote heading)) nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " heading "\n")
    (beginning-of-line 0))
  t)

(defun org-bookmark-handler-match-url (regex file heading)
  "For link matching REGEX select FILE at HEADING."
  (if
      (caar org-stored-links)
      (when (string-match regex (caar org-stored-links))
        (org-bookmark-handler-file-heading file heading))
    (when (string-match regex (string-trim (substring-no-properties
                                                (current-kill 0))))
      (org-bookmark-handler-file-heading file heading))))

(defvar org-bookmark-append-style 'beginning)

(defun org-bookmark-append-style-valid-p ()
  (if (and (symbolp org-bookmark-append-style)
           (member org-bookmark-append-style (list 'nil 'beginning 'end)))
           t nil))

;; TODO: These need some work, steal from org capture insertion commands.
(defun org-bookmark-append-content (initial-content)
  (unless (org-bookmark-append-style-valid-p)
    (error "Invalid org-bookmark appending style '%s'" org-bookmark-append-style))
  (when (eq org-bookmark-append-style 'beginning)
    (progn
      (org-end-of-meta-data t)
      (insert initial-content "\n")))
  (when (eq org-bookmark-append-style 'end)
    (progn
      (outline-next-heading)
      (beginning-of-line 0)
      (insert initial-content "\n"))))

(defun org-bookmark-find-rg ()
  (unless (executable-find "rg")
    (error "Org-bookmark could not find rg in your path!")))

(defun org-bookmark-find-link ()
  (let* ((link (if (caar org-stored-links)
                   (caar org-stored-links)
                 (string-trim (substring-no-properties (current-kill 0)))))
         (old-links
          (split-string (shell-command-to-string
                         (format "rg -Fn '[%s]' %s" link org-directory))
                        "\n" t)))
    (if old-links
        (let ((old-link (car old-links)))
          (if (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" old-link)
              (let ((file (match-string 1 old-link))
                    (line (string-to-number (match-string 2 old-link))))
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line))
                (if (string-match-p "https://www.youtube.com" link)
                    (not (y-or-n-p "old link: redo?"))
                  (message "%d old link(s)" (length old-links)) t))
            (error "Could not match %s" old-link)))
      nil)))

(defun org-bookmark-capture-kill ()
  (let ((initial-content (plist-get org-store-link-plist :initial)))
    (when (and org-bookmark-append-style initial-content (not (string-empty-p initial-content)))
      (org-bookmark-append-content initial-content))
    (unless org-link-keep-stored-after-insertion (pop org-stored-links))
    (org-capture-kill)))

(defun org-bookmark-handle-location ()
  (let ((hands org-bookmark-location-handlers)
        hand)
    (while (and (setq hand (pop hands))
                (null
                 (apply (car hand) (cdr hand)))))))

(defun org-bookmark-location ()
  "Selects a location to store the current bookmark link."
  (if (org-bookmark-find-link)
      (org-bookmark-capture-kill)
    (org-bookmark-handle-location)))

(org-bookmark-add-link-formatter "https://github.com" (org-bookmark-replace-in-link-title ":[ ].*$?" ""))

(setq org-bookmark-location-handlers '((org-bookmark-handler-file-heading "inbox.org" "Bookmarks")))

(defun org-bookmark-symbol ()
  (let ((symbol (symbol-name (helpful--read-symbol
                              "Symbol: " (helpful--symbol-at-point)
                              #'helpful--bound-p))))
    (my/org-store-help-link symbol)
    (apply #'org-bookmark-handler-file-heading '("~/Documents/notes/area/20231006T042354--emacs.org" "Useful Symbols and Keybinds"))))

;(use-package! orca
;  :after org-protocol
  ;;; Variables
  ;;; (setq orca-handler-list
  ;;;       '((orca-handler-match-url
  ;;;          "https://www.reddit.com"         "/home/noncog/Documents/notes/wiki/emacs.org"
  ;;;          "\\ * Reddit")))
  ;;; (setq orca-handler-list
  ;;;       '((orca-handler-match-url
  ;;;          "https://www.reddit.com/emacs/"
  ;;;          "~/Documents/notes/wiki/emacs.org"
  ;;;          "Reddit")
  ;;;         (orca-handler-match-url
  ;;;          "https://emacs.stackexchange.com/"
  ;;;          "~/Documents/notes/wiki/emacs.org"
  ;;;          "\\* Questions")
  ;;;         (orca-handler-current-buffer
  ;;;          "\\* Tasks")))
;  )

(use-package! org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  ;; Appearance
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
        org-modern-star nil                    ; Disabled due to font instability.
        org-modern-horizontal-rule (make-string 80 ?─)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  ;; Appearance
  (setq org-appear-autoemphasis t              ; Show emphasis markup.
        org-appear-autosubmarkers t            ; Show sub/superscript
        org-appear-autoentities t              ; Show LaTeX like Org pretty entities.
        org-appear-autolinks nil               ; Shows Org links.
        org-appear-autokeywords nil            ; Shows hidden Org keywords.
        org-appear-inside-latex nil)           ; Show LaTeX code. Use Fragtog instead.
  )

(use-package! orglink
  :config
  (setq orglink-activate-in-modes '(sh-mode emacs-lisp-mode))
  (global-orglink-mode))

(use-package! toc-org
  :defer t
  :config
  ;; Behavior
  (setq org-toc-default-depth 3)
  (defadvice! my/+toc-org-insert-toc-folded (&optional dry-run)
    "An advised function to extend 'toc-org-insert-toc to add html details tags."
    :override #'toc-org-insert-toc
    (interactive)
    (let ((dry-run nil))
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search t)
             (markdown-syntax-p (derived-mode-p 'markdown-mode))
             (heading-symbol-regexp (if markdown-syntax-p "^#" "^\\*")))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward (concat heading-symbol-regexp toc-org-toc-org-regexp) (point-max) t)
          (let* ((tag (match-string 2))
                 (depth (if tag
                            (- (aref tag 1) ?0) ;; is there a better way to convert char to number?
                          toc-org-max-depth))
                 (hrefify-tag (if (and tag (>= (length tag) 4))
                                  (downcase (substring tag 3))
                                toc-org-hrefify-default))
                 (hrefify-string (concat "toc-org-hrefify-" hrefify-tag))
                 (hrefify (intern-soft hrefify-string))
                 (put-quote (save-match-data (string-match toc-org-quote-tag-regexp (match-string 0))))
                 (toc-prefix (if put-quote (if markdown-syntax-p "```\n" "#+html:<details><summary>Table of Contents</summary>\n#+BEGIN_QUOTE\n")  ""))
                 (toc-suffix (if put-quote (if markdown-syntax-p "```\n" "#+END_QUOTE\n#+html:</details>\n") "")))
            (if hrefify
                (let ((new-toc
                       (concat toc-prefix
                               (toc-org-hrefify-toc
                                (toc-org-flush-subheadings (toc-org-raw-toc markdown-syntax-p) depth)
                                hrefify
                                markdown-syntax-p
                                (when toc-org-hrefify-hash
                                  (clrhash toc-org-hrefify-hash)))
                               toc-suffix)))
                  (unless dry-run
                    (newline (forward-line 1))
  
                    ;; skip drawers
                    (let ((end
                           (save-excursion ;; limit to next heading
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
                      (unless (equal
                               (buffer-substring-no-properties beg end)
                               new-toc)
                        (delete-region beg end)
                        (insert new-toc)))))
              (message (concat "Hrefify function " hrefify-string " is not found")))))))))
  )

(use-package! ox
  :defer t
  :init
  ;; Keybinds
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
        :localleader
        :desc "View exported file"
        "v" #'org-view-output-file)
  :config
  ;; Appearance
  (setq org-export-with-creator t
        org-export-creator-string (format "Doom Emacs %s (Org mode %s)" emacs-version org-version))
  ;; Behavior
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-headline-levels 5))

(use-package! org-noter
  :defer t
  :init
  ;; Variables
  (setq org-noter-notes-search-path (file-truename "~/Documents/org/references"))
  :config
  ;; Behavior
  (setq org-noter-always-create-frame nil        ; Don't create a new frame for the session.
        org-noter-kill-frame-at-session-end nil) ; Don't kill any frames since none created.
  ;; Appearance
  (setq org-noter-separate-notes-from-heading t) ; Adds line between headings and notes.
  (setq org-noter-disable-narrowing t) ; Let me see everything.
  (map! :leader :desc "Kill org noter session" "n k" #'org-noter-kill-session)
  )

(use-package! pdf-tools
  :defer t
  :config
  ;; Behavior
  (pdf-tools-install t)
  ;; Fixes
  (when IS-MAC (add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode)))

(use-package! sh-script
  :defer t
  :init
  (set-file-template! "\\.sh" :trigger "__sh" :mode 'sh-mode)
  (defun my/bash-info-page ()
    "Go to the Bash info page."
    (interactive)
    (info "Bash"))
  :config
  (set-formatter! 'shfmt
    '("shfmt" "-ci" "-bn" "-sr" "-kp"
      ("-i" "%d" (unless indent-tabs-mode tab-width))
      ("-ln" "%s" (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))))))

(defun sp-wrap-quote ()
  "Wrap following sexp in quotes."
  (interactive "*")
  (sp-wrap-with-pair "\""))

(defun org-execute-named-src-block-in-lit-config ()
  (interactive)
  (org-babel-with-temp-filebuffer +literate-config-file (org-babel-goto-named-src-block "gen-org-protocol-script") (org-babel-execute-src-block)))
