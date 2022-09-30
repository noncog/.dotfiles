;;; $doomdir/config.el -*- lexical-binding: t; -*-

;; some functions use this to identify you, e.g. gpg configuration, email
;; clients, file templates and snippets. it is optional.
(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(global-auto-revert-mode 1)

(setq scroll-margin 5)

(setq doom-theme 'doom-dracula)
(setq doom-dracula-brighter-modeline t)
(setq doom-dracula-colorful-headers t)

(custom-set-faces!
 `(cursor :background ,(doom-color 'magenta)))

(setq doom-font (font-spec :family "fira code" :size 14))
(setq doom-unicode-font (font-spec :family "fira code" :size 16))

(add-to-list 'default-frame-alist '(alpha . 96)) ; [0-100]

(after! display-line-numbers
  (setq display-line-numbers-type nil))

(display-time-mode 1)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
;; scroll buffer by a line
(global-set-key (kbd "M-p") #'scroll-down-line)
(global-set-key (kbd "M-n") #'scroll-up-line)

(eval-when-compile (require 'rx))

(defun noncog/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (noncog/emacs-i3-window-focus
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
     (noncog/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (noncog/emacs-i3-resize-window
       (intern (elt (split-string command) 2))
       (intern (elt (split-string command) 1))
       (string-to-number (elt (split-string command) 3))))
    ;("layout toggle split" (transpose-frame))
    ("split v" (+evil/window-split-and-follow))
    ("split h" (+evil/window-vsplit-and-follow))
    ("kill" (evil-quit))
    (_ (shell-command (concat "i3-msg " (pp-to-string command))))))

(defun noncog/emacs-i3-direction-exists-p (dir)
  (some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun noncog/emacs-i3-window-focus (dir)
  (let ((other-window (windmove-find-other-window dir)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (shell-command (concat "i3-msg -s $(i3 --get-socket) focus " (symbol-name dir)))
      (windmove-do-window-select dir))))

(defun noncog/emacs-i3-move-window (dir)
  ;(message (concat "Move window " (pp-to-string dir) "."))
  (funcall (intern (concat "+evil/window-move-" (pp-to-string dir)))))

(defun noncog/emacs-i3-resize-window (dir kind value)
  (pcase kind
    ('shrink
     (pcase dir
       ('width
        (evil-window-decrease-width value))
       ('height
        (evil-window-decrease-height value))))
    ('grow
     (pcase dir
       ('width
        (evil-window-increase-width value))
       ('height
        (evil-window-increase-height value))))))

(setq org-directory "~/documents/org/")

(defconst noncog/brain-file "/home/jake/documents/org/brain.org")

(defun noncog/toggle-brain ()
  "A function for toggling the view of the your chosen file in a side window."
  (interactive)
  (if (get-file-buffer noncog/brain-file)
      (progn(kill-buffer (get-file-buffer noncog/brain-file))(message "Killed Brain buffer."))
    (progn(display-buffer (find-file-noselect noncog/brain-file))(message "Opened Brain buffer."))))

;; set it's window behavior
(set-popup-rule! "^brain.org" :side 'right :vslot -1 :width 70 :modeline t :select t :quit nil)

;; evil style
(map! :leader :desc "Brain.org" "b t" #'noncog/toggle-brain)
;; emacs style
(global-set-key (kbd "C-c b") #'noncog/toggle-brain)

(after! consult
  (defadvice! org-show-entry-consult-a (fn &rest args)
    :around #'consult-line
    :around #'consult-org-heading
    :around #'consult--grep
    (when-let ((pos (apply fn args)))
      (and (derived-mode-p 'org-mode) (org-fold-reveal '(4))))))

(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
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
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(after! org
  (use-package! org
    :config
    (add-to-list 'org-modules 'org-habit t)             ; enable org-habit
    (setq org-agenda-files (quote ("~/documents/org/brain.org"
                                   "~/documents/org/university/linear-algebra-3720"
                                   "~/documents/org/university/data-structures-and-algorithms-5870"
                                   "~/documents/org/university/network-concepts-and-administration-3723"
                                   "~/documents/org/university/software-engineering-5801"
                                   "~/documents/org/university/information-assurance-3755"
                                   "~/projects/form-visualizer"
                                   "~/projects/immediate-sdk")))
    (setq org-habit-show-habits-only-for-today t)       ; ensure habits only shown in one section
    (setq org-habit-show-all-today t)                   ; keep habits visible even if done today
    (setq org-log-done 'time)                           ; add completion time to DONE items.
    (setq org-log-into-drawer t)                        ; puts log times into a drawer to hide them
    ;; (setq org-todo-keywords
    ;;     '((sequence "TODO(t)" "IDEA(i)" "PROJ(p)" "STRT(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")
    ;;       (sequence "[ ](T)" "[-](S)" "[?](W)" "[X](D)")
    ;;       (sequence "OKAY(o)" "YES(y)" "NO(n)")))
    (setq org-return-follows-link t)                    ; enter opens links in org
    (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks
    (setq org-insert-heading-respect-content nil)       ; insert the heading at cursor, not at end
    (setq org-imenu-depth 10)                           ; allow imenu to search deeply in org docs
    (setq org-use-property-inheritance t)               ; sub-headings inherit parent properties
    (setq org-use-fast-todo-selection 'auto)            ; method used for org-todo
    ;(setq org-use-fast-todo-selection 'expert)
    ;(setq org-use-fast-todo-selection nil)
    (setq evil-collection-calendar-want-org-bindings t) ; add evil bindings to org's calendar
    ;; these should be moved to ox-latex
    (setq org-latex-src-block-backend 'engraved)        ; add syntax highlighting to org latex exports
    ;(setq org-babel-results-keyword "OUTPUT")           ; a possible workaround, can change this instead of results block exporting, allow to not use :exports both and instead use the result from within the code document, possibly reducing export time. Need to understand more. org babel result shows many variables to play with.
    (setq org-highlight-latex-and-related '(native script entities)) ; supposedly teco had other issues with this but I haven't found them...
    (setq org-ellipsis " ▾ ")                           ; set custom ellipsis
    (setq org-src-preserve-indentation t)               ; prevent adding spaces/indents
    (setq org-hide-emphasis-markers t)                  ; hide formatting for markup
    (setq org-hide-leading-stars t)                     ; remove excess heading stars
    (setq org-fontify-quote-and-verse-blocks nil)
    (setq org-fontify-whole-heading-line nil)
    )
  )

(defun noncog/agenda-remove-empty ()
  "A simple function to remove empty agenda sections. Scans for blank lines. Blank sections defined by having two consecutive blank lines. Not compatible with the block separator."
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

(defun noncog/my-agenda ()
  "My custom agenda launcher."
  (interactive)
  (org-agenda nil "o"))

(map! :leader :desc "My agenda" "o a o" #'noncog/my-agenda)

(after! org-agenda
  (use-package! org-agenda
    :config
    (setq org-agenda-start-with-log-mode t)             ; show 'completed' done items in agenda
    (set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 67 :modeline nil :select t :quit t)
    (custom-set-faces!
      '(org-agenda-structure
        :height 1.3 :weight bold))
    ;(set-face-attribute 'org-agenda-structure nil :height 120 :weight 'bold)
    (setq org-agenda-custom-commands
          '(
            ("o" "My Agenda" (
             (agenda "" ( ;; Today
                         (org-agenda-overriding-header "Today\n")
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-block-separator nil)
                         (org-agenda-format-date "%a, %b %-e %y")   ; american date format
                         (org-agenda-start-on-weekday nil)          ; start today
                         (org-agenda-start-day nil)                 ; don't show previous days
                         (org-agenda-span 1)                        ; only show today
                         (org-scheduled-past-days 0)                ; don't show overdue
                         (org-deadline-warning-days 0)              ; don't show deadlines for the future
                         (org-agenda-time-leading-zero t)           ; unify times formatting
                         (org-agenda-time-grid '((daily today remove-match) (800 1000 1200 1400 1600 1800 2000) "" ""))
                         (org-agenda-prefix-format '((agenda . "  %?-5t %?-9:c")))
                         (org-agenda-dim-blocked-tasks nil)
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))
                         ))
              (agenda ""( ;; Next Three Days
                         (org-agenda-overriding-header "\nNext Three Days\n")
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-block-separator nil)
                         (org-agenda-format-date "%a %b %-e")
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-start-day "+1d")
                         (org-agenda-span 3)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-time-leading-zero t)
                         (org-agenda-time-grid '((daily weekly) () "" ""))
                         (org-agenda-prefix-format '((agenda . "  %?-9:c%t ")))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-dim-blocked-tasks nil)
                         ))
              (agenda ""( ;; Upcoming Deadlines
                         (org-agenda-overriding-header "\nUpcoming Deadlines\n")
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-block-separator nil)
                         (org-agenda-format-date "%a %b %-e")
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-start-day "+4d")
                         (org-agenda-span 14)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-time-leading-zero t)
                         (org-agenda-time-grid nil)
                         (org-agenda-prefix-format '((agenda . "  %?-5t %?-9:c")))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-entry-types '(:deadline))
                         (org-agenda-show-all-dates nil)
                         (org-agenda-dim-blocked-tasks nil)
                         ))
              (agenda ""( ;; Past Due
                         (org-agenda-overriding-header "\nPast Due\n")
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
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-entry-types '(:deadline :scheduled))
                         (org-agenda-show-all-dates nil)
                         (org-agenda-dim-blocked-tasks nil)
                         ))
              (agenda ""( ;; Habits
                         (org-agenda-overriding-header "\nHabits")
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-block-separator nil)
                         (org-agenda-format-date "")
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-start-day nil)
                         (org-agenda-span 1)
                         (org-agenda-time-grid nil)
                         (org-agenda-prefix-format '((agenda . "  %?-5t %?-7:c")))
                         (org-agenda-dim-blocked-tasks nil)
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))
                         ))
              (todo "" ( ;; Important Tasks No Date
                        (org-agenda-overriding-header "\nImportant Tasks - No Date\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'notregexp "\\[\\#A\\]"))
                        (org-agenda-block-separator nil)
                        (org-agenda-time-grid nil)
                        (org-agenda-prefix-format '((todo . "  %?:c ")))
                        (org-agenda-dim-blocked-tasks nil)
                        ))
             ))))
    
    (add-hook! 'org-agenda-finalize-hook #'noncog/agenda-remove-empty)
    )
  )

(defun noncog/kill-org-noter-session ()
  "Fully exits the noter-session and the pdf buffers it used, leaving the org file."
  (interactive)
  (let ((pdf-fname (buffer-file-name (org-noter--session-doc-buffer org-noter--session)))
        (ses-id (org-noter--session-id org-noter--session))) (set-window-dedicated-p (get-buffer-window (get-file-buffer pdf-fname)) nil)
        (call-interactively #'org-noter-kill-session ses-id) (doom/kill-this-buffer-in-all-windows (get-file-buffer pdf-fname))))

(map! :leader :desc "Kill org noter session" "n k" #'noncog/kill-org-noter-session)

(after! org-noter
  (use-package! org-noter
    :config
    (setq org-noter-notes-search-path '("~/documents/org/noter"))
    (setq org-noter-separate-notes-from-heading t)      ; Adds a blank line between headings and notes
    (setq org-noter-auto-save-last-location t)          ; remembers where I left off in pdfs and notes
    ;; don't touch my frames...
    (setq org-noter-always-create-frame nil)
    (setq org-noter-kill-frame-at-session-end nil)
    )
  )

(add-to-list '+evil-collection-disabled-list 'org-present)

(defun noncog/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children)
  )

(add-hook 'org-present-after-navigate-functions 'noncog/org-present-prepare-slide)

(setq recenter-positions '(top bottom))

(defun noncog/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-fold-show-entry) (outline-show-children))
    (unless (org-before-first-heading-p) (outline-next-heading))
    (unless (and (bolp) (org-at-heading-p))
      ;(org-up-heading-safe)
      ;(hide-subtree)
      ;(error "Boundary reached")
      ;(if (eobp) (error "end of slide") (org-present-next))
      (org-present-next)
      )
    (org-overview)
    (org-reveal t)
    (org-fold-show-entry)
    (recenter-top-bottom)
    (outline-show-children)
    (recenter-top-bottom)))

(defun noncog/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (org-present-prev)
      )
    (org-overview)
    (org-reveal t)
    (org-fold-show-entry)
    (recenter-top-bottom)
    (outline-show-children)
    (recenter-top-bottom)))

(defun noncog/org-present-start ()
  (setq-local display-line-numbers nil)
  (setq-local header-line-format " ")
  (org-display-inline-images)
  ;; add space to the top of the screen
  ;; default inherits from the modeline, which for me, is colored
  (custom-set-faces!
    '(header-line :inherit nil :height 2.0))
  (doom-big-font-mode 1)
  ;; (setq visual-fill-column-width 110)               ; sets the text width of the centered buffer
  ;; (setq visual-fill-column-center-text t)           ; centers the buffer
  ;; (visual-fill-column-mode 1)
  ;; (visual-line-mode 1)
  )
(add-hook! 'org-present-mode-hook #'noncog/org-present-start)

(defun noncog/org-present-end ()
  (setq-local display-line-numbers 'visual)
  (setq-local header-line-format nil)
  (org-remove-inline-images)
  (custom-set-faces!
    '(header-line :height 1.0))
  (+org/show-next-fold-level)
  (doom-big-font-mode 0)
  ;; (setq visual-fill-column-width 0)               ; sets the text width of the centered buffer
  ;; (setq visual-fill-column-center-text nil)           ; centers the buffer
  ;; (visual-fill-column-mode 0)
  ;; (visual-line-mode 0)
  )
(add-hook! 'org-present-mode-quit-hook #'noncog/org-present-end)

(use-package! org-present
  :config
  (map! :map org-mode-map :localleader "m" #'org-present)
  (map! :map org-present-mode-keymap "C-q" #'org-present-quit)
  (map! :map org-present-mode-keymap "C-g" #'org-present-beginning)
  (map! :map org-present-mode-keymap "C-j" #'noncog/org-show-next-heading-tidily)
  (map! :map org-present-mode-keymap "C-k" #'noncog/org-show-previous-heading-tidily)
  )

(defvar +org-plot-term-size '(1050 . 650)
  "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")

(after! org-plot
  (defun +org-plot-generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)))

  (defun +org-plot-gnuplot-term-properties (_type)
    (format "background rgb '%s' size %s,%s"
            (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))

  (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
  (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))

(after! ox
  (setq org-export-headline-levels 7)
  (setq org-export-with-creator t)
  (setq org-export-creator-string (concat "Doom Emacs " emacs-version " (Org mode " org-version ")" ))
  )

(use-package! ox-latex
  :init
  (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  )

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-page))

(use-package! engrave-faces-latex
  :after ox-latex
)

(defun noncog/pulsar-scroll-recenter-middle (&rest _args)
  (pulsar-recenter-middle))

(pulsar-global-mode 1)

(use-package! pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.095)
  (setq pulsar-iterations 12)
  (setq pulsar-face 'pulsar-magenta)                  ; used by pulsar-pulse-line
  (setq pulsar-highlight-face 'pulsar-yellow)         ; used by pulsar-highlight-*
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  
  (add-to-list 'pulsar-pulse-functions 'evil-window-left)
  (add-to-list 'pulsar-pulse-functions 'evil-window-down)
  (add-to-list 'pulsar-pulse-functions 'evil-window-up)
  (add-to-list 'pulsar-pulse-functions 'evil-window-right)
  
  (add-to-list 'pulsar-pulse-functions 'evil-window-next)
  (add-to-list 'pulsar-pulse-functions 'evil-window-prev)
  
  ;(add-to-list 'pulsar-pulse-functions 'select-window)
  ;(add-to-list 'pulsar-pulse-functions 'windmove-do-window-select)
  ;(add-to-list 'pulsar-pulse-functions 'windmove-find-other-window)
  (advice-add 'evil-scroll-up :after #'noncog/pulsar-scroll-recenter-middle)
  (advice-add 'evil-scroll-down :after #'noncog/pulsar-scroll-recenter-middle)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)
  )

(after! company
  (use-package! company
    :config
    (setq company-idle-delay 0.1)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 1)
    (setq company-global-modes '(not text-mode erc-mode circe-mode message-mode help-mode gud-mode vterm-mode))
    (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
    (set-company-backend! 'org-mode
      '(company-capf company-files :with company-yasnippet))
    (set-company-backend! 'sh-mode
      '(company-shell company-files :with company-yasnippet))
    )
  )

(after! ace-window
  ;; customize face used for indicators
  (custom-set-faces!
    '(aw-leading-char-face :foreground "magenta" :background "bg" :weight bold :height 180 :box (:line-width 1 :color "magenta")))
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f))
  (setq aw-mnibuffer-flag t)
  (setq aw-scope 'global)
  )

(after! projectile
  (setq projectile-ignored-projects '("/opt/doom-emacs/"))
  (map! :leader :desc "List dirty projects" "p l" #'projectile-browse-dirty-projects)
  )

(after! yasnippet
  (use-package! yasnippet
    :config
    (setq yas-snippet-dirs '("~/.config/doom/snippets/"))
    (setq yas-indent-line 'fixed)                       ; prevent yasnippet from changing my indents
    )
  )

(use-package! python
  :init
  (setq python-shell-interpreter "python3")
  )

(defun noncog/remove-electric-indent-mode ()
  (electric-indent-local-mode -1))

(add-hook 'sh-mode-hook 'noncog/remove-electric-indent-mode)
