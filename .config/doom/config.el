;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(setq doom-theme 'doom-vibrant)
;(setq doom-theme 'doom-Iosvkem)
;(setq doom-theme 'doom-dracula
;      doom-dracula-brighter-modeline t
;      doom-dracula-colorful-headers t)
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
;(add-to-list 'default-frame-alist '(alpha . 93)) ; [0-100]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-inhibit-implied-resize '(font font-backend tab-bar-lines))
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-grow-only t)
(setq evil-want-fine-undo t)
(global-subword-mode 1)
(defun noncog/repeat-function-count (fn &optional count)
  "Repeat given function COUNT times."
  (let ((total (or count 1))
        (counter 0))
    (while (< counter total)
      (funcall fn)
      (setq counter (+ counter 1)))))
(advice-add 'evil-previous-visual-line :around #'noncog/repeat-function-count)
(advice-add 'evil-next-visual-line :around #'noncog/repeat-function-count)
(setq evil-kill-on-visual-paste nil)
(setq evil-split-window-below t
      evil-vsplit-window-right t)
(global-auto-revert-mode 1)
;(setq-default evil-scroll-count 5)
(define-minor-mode prot/scroll-center-cursor-mode
  "Toggle centred cursor scrolling behavior"
  :init-value nil
  :lighter " S="
  :global nil
  (if prot/scroll-center-cursor-mode
      (setq-local scroll-margin (* (frame-height) 2)
                  scroll-conservatively 0
                  maximum-scroll-margin 0.5)
    (dolist (local '(scroll-preserve-screen-position
                     scroll-conservatively
                     maximum-scroll-margin
                     scroll-margin))
      (kill-local-variable `,local))))
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
  (map! "s-h" #'wm-window-left
        "s-j" #'wm-window-down
        "s-k" #'wm-window-up
        "s-l" #'wm-window-right
        "s-=" #'balance-windows
        "s-v" #'evil-window-vsplit
        "s-s" #'evil-window-split
        "s-Q" #'evil-quit))
(when IS-LINUX
  (map! "s-h" #'wm-window-left
        "s-j" #'wm-window-down
        "s-k" #'wm-window-up
        "s-l" #'wm-window-right
        ;; add workspace balancing python script.
        ;; add window splitting.
        "s-Q" #'evil-quit))
(map! :leader "w h" #'wm-window-left)
(map! :leader "w j" #'wm-window-down)
(map! :leader "w k" #'wm-window-up)
(map! :leader "w l" #'wm-window-right)
(when IS-MAC (setq mac-command-modifier 'control ; Maps Command -> Control
                    mac-control-modifier 'meta   ; Maps Control -> Alt (Meta)
                    mac-option-modifier 'super)) ; Maps Option -> Super
(use-package! beacon
  :init
  ;; Appearance
  (setq beacon-color "#61bfff")
  (setq beacon-size 40
        beacon-blink-duration 0.3
        beacon-blink-delay 0.5)
  ;; Behavior
  (setq beacon-blink-when-buffer-changes t
        beacon-blink-when-window-scrolls nil
        beacon-blink-when-focused nil)
  )
(beacon-mode 1)
(use-package! which-key
  :defer t
  :config
  ;; Behavior
  (setq which-key-idle-delay 0.5)
  ;(setq which-key-use-C-h-commands t)
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
   '(("" . "\\`treemacs-\\(.*\\)") . (nil . " \\1")))
  )
(use-package! helpful
  :defer t
  :config
  ;; Behavior
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
      (select-window origin)))
  ;; Keybinds
  (map! :map helpful-mode-map "C-h" #'winner-undo
        :map helpful-mode-map "C-l" #'winner-redo)
  )

(use-package! doom-modeline
  :defer t
  :config
  ;; Appearance
  (setq doom-modeline-height 35)
  (setq doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p))
  (setq doom-modeline-vcs-max-length 60
        ;doom-modeline-github t
        ;doom-modeline-github-interval 60
        auto-revert-check-vc-info t)
  (setq doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t
        doom-modeline-persp-icon t)
  )
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(setq +doom-dashboard-pwd-policy "~")
(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(use-package! doom-dashboard
  :defer t
  :config
  ;; Appearance
  ;; Behavior
  ;; Keybinds
  )
(use-package! persp-mode
  :defer t
  :config
  ;; Fixes
  (setq persp-emacsclient-init-frame-behaviour-override nil)
  )
(use-package! vertico
  :defer t
  :config
  ;; Behavior
  (setq vertico-sort-function #'vertico-sort-history-alpha)
  ;; Keybinds
  (map! :map vertico-map "C-u" #'scroll-down-command
        :map vertico-map "C-d" #'scroll-up-command)
  )
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
      (propertize (file-size-human-readable size) 'face (list :foreground color))))
  )
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
    '(company-shell company-files :with company-yasnippet))
  )
(use-package! yasnippet
  :defer t
  :config
  ;; Behavior
  (setq yas-triggers-in-field t)
  )
(setq projectile-project-search-path '("~/Projects"))
(setq doom-projectile-cache-blacklist '("~" "/tmp" "/" "~/.config/emacs" "/opt/homebrew"))
(setq projectile-auto-discover nil)

(use-package! projectile
  :defer t
  :config
  ;; Variables
  ;; Behavior
  ;; Keybinds
  )

(map! :map project-prefix-map
      :leader
      :desc "List dirty projects"
      "p l" #'projectile-browse-dirty-projects)


(use-package! treemacs
  :defer t
  :config
  ;; Appearance
  (setq doom-themes-treemacs-theme "doom-colors")
  ;; Behavior
  (treemacs-follow-mode 1)
  (setq treemacs-project-follow-cleanup t)
  )
;(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
(defadvice! my/magit-display-buffer-fullframe-status-topleft-v1 (buffer)
  "Override Doom's default +magit-display-buffer-fn to use built-in behavior."
  :override #'+magit-display-buffer-fn
  ;; The following is a direct copy of magit-display-buffer-fullframe-status-topleft-v1
  (display-buffer
   buffer
   (cond ((eq (with-current-buffer buffer major-mode)
              'magit-status-mode)
          '(magit--display-buffer-fullframe))
         ((with-current-buffer buffer
            (derived-mode-p 'magit-diff-mode 'magit-process-mode))
          '(magit--display-buffer-topleft))
         (t
          '(display-buffer-same-window)))))

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
  (setq git-commit-summary-max-length 50)                  ; Maximum title (summary) length.
  (setq git-commit-fill-column 72)                         ; Description column limit.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'git-commit-finish-query-functions
               #'my-git-commit-check-style-conventions)
  )
(use-package! vterm
  :defer t
  :config
  ;; Keybinds
  (define-key vterm-mode-map (kbd "<C-backspace>") (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
  )
(use-package! visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))
(setq org-directory "~/Projects/brain/")

(use-package! org
  :defer t
  ;:hook
  :config
  ;; Variables
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"     ; Task that needs doing & is ready to do.
           "NEXT(N!)"     ; Task that needs doing & is ready to do.
           "WAIT(w@/!)"   ; Something is holding this up.
           "NOTE(n!)"     ; A fleeting note, in person, idea, or link.
           "LINK(l!)"     ; A link I want to note.
           "APPT(a!)"     ; An appointment.
           "|"
           "DONE(d!)"     ; Task successfully completed.
           "KILL(k@/!)")) ; Task cancelled or not applicable.
        org-todo-keyword-faces
        '(("PROJ" . +org-todo-project)
          ("NEXT" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("NOTE" . +org-todo-active)
          ("KILL" . +org-todo-cancel)))
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
  ;; Behavior
  (setq org-imenu-depth 10                     ; Allow imenu to search deeply in org docs.
        org-use-property-inheritance t         ; Sub-headings inherit parent properties.
        org-insert-heading-respect-content nil ; Insert heading here, not at end of list.
        org-use-fast-todo-selection 'auto)     ; Method to select TODO heading keywords.
  (setq org-log-done 'time                     ; Add completion time to DONE items.
        org-log-into-drawer t                  ; Log times into a drawer to hide them.
        org-log-states-order-reversed nil)     ; Log state changes chronologically.
  (setq org-list-allow-alphabetical t)         ; Enable use of alphabet as list bullets.
  (setq org-return-follows-link t)             ; Pressing enter opens links.
  (add-to-list 'org-modules 'org-habit t)      ; Enable repeated task tracking/graphing!
  (setq evil-collection-calendar-want-org-bindings t)
  ;; Fixes
  (setq org-fold-core-style 'overlays)         ; Bugfix: Hide IDs in org-roam backlinks.
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
  )

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
(map! :leader :desc "My agenda" "o a o" #'noncog/my-agenda)

(use-package! org-agenda
  :defer t
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
  ;;(set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline nil :select t :quit t)
  (set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline nil :select t :quit 'current)
  ;; Behavior
  (setq org-agenda-start-with-log-mode t)      ; Show 'completed' items in agenda.
  )

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
(add-hook! 'org-agenda-finalize-hook #'noncog/agenda-remove-empty)
(after! (org-agenda org-roam)
  (defun vulpea-task-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-task-update-tag ()
    "Update task tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-task-p)
              (setq tags (cons "task" tags))
            (setq tags (remove "task" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-task-files ()
    "Return a list of note files containing 'task' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (or (like tag (quote "%\"task\"%"))
                   (like tag (quote "%\"schedule\"%")))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-task-files)))

  (add-hook 'find-file-hook #'vulpea-task-update-tag)
  (add-hook 'before-save-hook #'vulpea-task-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.

If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
      (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                               (point-max) t)
        (replace-match ""))))
  )
;; Keybinds
(defun noncog/my-agenda ()
  "My custom agenda launcher."
  (interactive)
  (org-agenda nil "o"))
;; Helpers
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
(use-package! org-capture
  :defer t
  :config
  ;; Variables
  (setq org-capture-templates
        `(("i" "Inbox" entry
           (file+headline "inbox.org" "Inbox")
           "* %?\n%i\n" :prepend t)
          ("l" "Link" entry
           (file+headline "inbox.org" "Inbox")
           "* LINK %?\n%i\n" :prepend t)))
  ;; Behavior
  (setq org-capture-bookmark nil)
  ;; Keybinds
  (map! :leader :desc "Org capture" "x" #'org-capture
        :leader :desc "Pop up scratch buffer" "X" #'doom/open-scratch-buffer)
  ;; Fixes
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
(setq org-roam-directory (file-truename "~/Projects/brain"))
(setq org-roam-db-location (file-truename "~/Projects/brain/brain.db"))
(setq org-attach-id-dir (file-truename "~/Projects/brain/.attachments"))
;(setq org-roam-file-exclude-regexp (rx (or "data/" "inbox.org")) )

(use-package! org-roam
  :defer t
  :config
  ;; Variables
  ;; Behavior
  ;(setq +org-roam-auto-backlinks-buffer t)
  (setq org-roam-capture-templates
        '(("n" "node" plain
           "%?"
           :target
           (file+head "nodes/${slug}.org" "#+title: ${title}\n#+filetags: :draft:\n")
           :immediate-finish t
           :unnarrowed t)))
  (org-roam-db-autosync-mode)
  ;; Keybinds
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
(map! :leader "n r g" #'org-roam-ui-open)

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :init
  
  :config
  ;; Appearance
  (setq org-roam-ui-sync-theme t)
  ;; Behavior
  (setq org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start nil
        org-roam-ui-browser-function #'+lookup-xwidget-webkit-open-url-fn)
  (defun +org-roam-ui-popup-kill (roam-ui-popup-buffer)
    (progn (+popup--kill-buffer roam-ui-popup-buffer 0.1) (org-roam-ui-mode -1)))
  (setq display-buffer-mark-dedicated t)
  (set-popup-rule! "^\\*xwidget" :side 'right :ttl #'+org-roam-ui-popup-kill :slot -1 :width 0.33 :height 0.5 :quit 'current :select nil :modeline nil)
  ;; (set-popup-rule! (regexp-quote org-roam-buffer) ; persistent org-roam buffer
  ;;      :side 'right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 1)
  ;; (set-popup-rule! "^\\*org-roam: " ; node dedicated org-roam buffer
  ;;      :side 'right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 2)
  (defadvice! my/+org-roam-ui--on-msg-open-node (data)
    "Open a node CAREFULLY when receiving DATA from the websocket."
    :override #'org-roam-ui--on-msg-open-node
    (let* ((id (alist-get 'id data))
           (node (org-roam-node-from-id id))
           (pos (org-roam-node-point node))
           (buf (find-file-noselect (org-roam-node-file node))))
      (run-hook-with-args 'org-roam-ui-before-open-node-functions id)
      (unless (window-live-p org-roam-ui--window)
        (if-let ((windows (cl-set-difference (doom-visible-windows) (list (minibuffer-window))))
                 (newest-window (car (seq-sort-by #'window-use-time #'> windows))))
            (setq org-roam-ui--window newest-window)
          (setq org-roam-ui--window newest-window)))
      (set-window-buffer org-roam-ui--window buf)
      (select-window org-roam-ui--window)
      (goto-char pos)
      (run-hook-with-args 'org-roam-ui-after-open-node-functions id)))
  (defadvice! +org-roam-ui-always-sync-theme ()
    "Always sync da theme..."
    :after #'org-roam-ui-open
    (while (when (and org-roam-ui-mode (ignore-errors (get-buffer-window-list "*xwidget webkit: ORUI *"))) (websocket-send-text org-roam-ui-ws-socket (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme)))))))
    )
  ;; Keybinds
  )
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
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))
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
        org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-horizontal-rule (make-string 80 ?─))
  )
(use-package! org-modern-indent
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 1))
(use-package! toc-org
  :defer t
  :config
  ;; Behavior
  (setq org-toc-default-depth 2)
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
(use-package! org-noter
  :defer t
  :config
  ;; Behavior
  (setq org-noter-always-create-frame nil        ; Don't create a new frame for the session.
        org-noter-kill-frame-at-session-end nil) ; Don't kill any frames since none created.
  (setq org-noter-separate-notes-from-heading t) ; Adds line between headings and notes.
  )
(use-package! pdf-tools
  :defer t
  :config
  ;; Fixes
  (when IS-MAC (add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode))
  )
  (pdf-tools-install t)
(use-package! plantuml-mode
  :defer t
  :config
  ;; Behavior
  (setq plantuml-default-exec-mode 'jar)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar))
  )
(use-package! ox
  :defer t
  :config
  ;; Appearance
  (setq org-export-with-creator t
        org-export-creator-string (format "Doom Emacs %s (Org mode %s)" emacs-version org-version))
  ;; Behavior
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-headline-levels 5)
  ;; Keybinds
  )
(use-package! ox-latex
  :defer t
  :config
  ;; Appearance
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
  ;; Behavior
  (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  )
(use-package! engrave-faces-latex
  :after ox-latex)
(use-package! engrave-faces-html
  :after ox-html)
(use-package! lsp-clangd
  :defer t
  :config
  ;; Behavior
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2)
  )
