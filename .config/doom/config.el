;;; $doomdir/config.el -*- lexical-binding: t; -*-

;; some functionality uses this to identify you, e.g. gpg configuration, email
;; clients, file templates and snippets. it is optional.
(setq user-full-name "jake turner"
      user-mail-address "john@doe.com")

(global-auto-revert-mode 1)

(setq doom-theme 'doom-dracula)
(setq doom-dracula-brighter-modeline t)
(setq doom-dracula-colorful-headers t)

(setq doom-font (font-spec :family "fira code" :size 14))
(setq doom-unicode-font (font-spec :family "fira code" :size 16))

(custom-set-faces!
 `(cursor :background ,(doom-color 'magenta)))

(add-to-list 'default-frame-alist '(alpha . 96)) ; [0-100]

(after! time
  (display-time-mode 1))

(map! :leader :desc "dashboard" "d" #'+doom-dashboard/open)
(map! :leader :desc "brain.org" "b t" #'noncog/toggle-brain)
(map! :leader :desc "kill org noter session" "n k" #'noncog/kill-org-noter-session)
(map! :leader :desc "my agenda" "o a o" #'noncog/my-agenda)

(after! emacs
  (use-package! emacs
    :defer
    :config
    (setq scroll-margin 5)
    ;; scroll buffer around point
    (global-set-key (kbd "m-p") #'scroll-down-line)
    (global-set-key (kbd "m-n") #'scroll-up-line)
    (global-set-key (kbd "c-c b") #'noncog/toggle-brain)
    ;(global-set-key (kbd "f5") #'which-key-show-previous-page-cycle)
    ;(global-set-key (kbd "f5") #'which-key-show-next-page-cycle)
    )
  )

(setq org-directory "~/documents/org/")

(defconst noncog/brain-file "/home/jake/documents/org/brain.org")
(defvar noncog/brain-visible nil)

(defun noncog/toggle-brain ()
  "A function for toggling the view of the your chosen file in a side window."
  (interactive)
  (if (and noncog/brain-visible (get-buffer-window (get-file-buffer noncog/brain-file)) t)
      ;; buffer is visible
      (let ((buffer (get-file-buffer noncog/brain-file)))
        (delete-window (get-buffer-window buffer))
        (kill-buffer buffer)
        (setq noncog/brain-visible nil))
    ;; buffer not visible
    (progn
      (display-buffer (find-file-noselect noncog/brain-file))
      (setq noncog/brain-visible t))))

;; set it's window behavior
(set-popup-rule! "^brain.org" :side 'right :vslot -1 :width 70 :modeline t :select t :quit nil)

(after! consult
  (defadvice! org-show-entry-consult-a (fn &rest args)
    :around #'consult-line
    :around #'consult-org-heading
    :around #'consult--grep
    (when-let ((pos (apply fn args)))
      (and (derived-mode-p 'org-mode) (org-fold-reveal '(4))))))

(after! org
  (use-package! org
    :config
    (add-to-list 'org-modules 'org-habit t)             ; enable org-habit
    (setq org-agenda-files (quote ("~/documents/org/brain.org"
                                   "~/documents/org/university/linear-algebra-3720"
                                   "~/documents/org/university/data-structures-and-algorithms-5870"
                                   "~/documents/org/university/network-concepts-and-administration-3723"
                                   "~/documents/org/university/software-engineering-5801"
                                   "~/documents/org/university/information-assurance-3755")))
    ;(setq org-habit-show-habits-only-for-today nil)
    (setq org-habit-show-all-today t)
    ;(setq org-agenda-repeating-timestamp-show-all nil)
    (setq org-log-done 'time)                           ; add completion time to DONE items.
    (setq org-log-into-drawer t)                        ; puts log times into a drawer to hide them
    (setq org-return-follows-link t)                    ; enter opens links in org
    (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks list
    (setq org-insert-heading-respect-content nil)       ; insert the heading at cursor, not at end
    (setq org-imenu-depth 10)
    (setq org-use-property-inheritance t)
    (setq org-ellipsis " ▾")                            ; set custom ellipsis
    ;(setq org-edit-src-content-indentation 0)           ; prevent adding spaces/indents to
    (setq org-hide-emphasis-markers t)                  ; hide formatting for markup
    (setq org-structure-template-alist '(("d" . "SRC emacs-lisp :tangle no :noweb-ref")
                                         ("e" . "EXAMPLE")
                                         ("h" . "HTML")
                                         ("q" . "QUOTE")
                                         ("s" . "SRC")))
    )
  )

(defun noncog/my-agenda ()
  "my custom agenda launcher."
  (interactive)
  (org-agenda nil "o"))

(defun noncog/agenda-remove-empty ()
  "A simple function to remove empty agenda sections."
  (interactive)
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (let* ((agenda-blank-line "[[:blank:]]*$")
         (content-line-count (if (looking-at-p agenda-blank-line) 0 1))
         (content-blank-line-count (if (looking-at-p agenda-blank-line) 1 0))
         (start-pos (point))) ;; initializes variables and scans first line.
    (while (not (eobp))
      (forward-line 1)
       (cond
        ((when (> content-blank-line-count 1)
          (delete-region start-pos (point))
          (setq content-blank-line-count 0)
          (setq start-pos (point))
          )
         )
        ((not (looking-at-p agenda-blank-line))
         (setq content-line-count (1+ content-line-count))
         (setq start-pos (point))
         (setq content-blank-line-count 0)
         )
        ((looking-at-p agenda-blank-line)
         (setq content-blank-line-count (1+ content-blank-line-count))
         )
       )
       )
    (when (> content-blank-line-count 1)
      (delete-region start-pos (point))
      (setq content-blank-line-count 0)
      )
    )
  (goto-char (point-min))
  (setq buffer-read-only t)
)

(after! org-agenda
  (use-package! org-agenda
    :init
    (set-popup-rule! "^*org agenda*" :side 'right :vslot 1 :width 67 :modeline nil :select t :quit t)
    :config
    (setq org-agenda-start-with-log-mode t)             ; show 'completed' done items in agenda
    (set-face-attribute 'org-agenda-structure nil :height 120 :weight 'bold)
    ;; todo consider adding this numeric week number
    (setq org-agenda-custom-commands
          '(
            ("o" "my agenda" (
              (todo "" ( ;; important tasks no date
                        (org-agenda-overriding-header "important tasks - no date\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'notregexp "\\[\\#a\\]"))
                        (org-agenda-block-separator nil)
                        (org-agenda-time-grid nil)
                        (org-agenda-prefix-format '((todo . "  %?:c ")))
                        (org-agenda-dim-blocked-tasks nil)
                        ))
             (agenda "" ( ;; today
                         (org-agenda-overriding-header "\ntoday\n")
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
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":style:.*habit"))
                         ))
              (agenda ""( ;; next three days
                         (org-agenda-overriding-header "\nnext three days\n")
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
              (agenda ""( ;; upcoming deadlines
                         (org-agenda-overriding-header "\nupcoming deadlines\n")
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
              (agenda ""( ;; past due
                         (org-agenda-overriding-header "\npast due\n")
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
              (agenda ""( ;; habits
                         (org-agenda-overriding-header "\nhabits")
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-block-separator nil)
                         (org-agenda-format-date "")
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-start-day nil)
                         (org-agenda-span 1)
              ;;            (org-scheduled-past-days 0)
              ;;            (org-deadline-warning-days 0)
                         (org-agenda-time-grid nil)
                         (org-agenda-prefix-format '((agenda . "  %?-5t %?-7:c")))
              ;;            (org-agenda-show-all-dates nil)
                         (org-agenda-dim-blocked-tasks nil)
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":style:.*habit"))
                         ))
             ))))
    
    (add-hook! 'org-agenda-finalize-hook #'noncog/agenda-remove-empty)
    )
  )

(defun noncog/kill-org-noter-session ()
  "Automatically closes pdfs when done noting them."
  (interactive)
  (let ((pdf-fname (buffer-file-name (org-noter--session-doc-buffer org-noter--session))) (ses-id (org-noter--session-id org-noter--session))) (set-window-dedicated-p (get-buffer-window (get-file-buffer pdf-fname)) nil) (call-interactively #'org-noter-kill-session ses-id) (doom/kill-this-buffer-in-all-windows (get-file-buffer pdf-fname))))

(after! org-noter
  (use-package! org-noter
    :config
    (setq org-noter-notes-search-path '("~/documents/org/noter"))
    (setq org-noter-always-create-frame nil)
    (setq org-noter-kill-frame-at-session-end nil)
    (setq org-noter-separate-notes-from-heading t)
    (setq org-noter-auto-save-last-location t)
    )
  )

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-page))

(after! display-line-numbers
  (setq display-line-numbers-type 'visual))

(defun noncog/pulsar-scroll-recenter-middle (&rest _args)
  (pulsar-recenter-middle))

(use-package! pulsar
  :config
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.095)
  (setq pulsar-iterations 12)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  
  (add-to-list 'pulsar-pulse-functions 'evil-window-left)
  (add-to-list 'pulsar-pulse-functions 'evil-window-down)
  (add-to-list 'pulsar-pulse-functions 'evil-window-up)
  (add-to-list 'pulsar-pulse-functions 'evil-window-right)
  
  (add-to-list 'pulsar-pulse-functions 'evil-window-next)
  (add-to-list 'pulsar-pulse-functions 'evil-window-prev)
  (advice-add 'evil-scroll-up :after #'noncog/pulsar-scroll-recenter-middle)
  (advice-add 'evil-scroll-down :after #'noncog/pulsar-scroll-recenter-middle)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)
  )

(pulsar-global-mode 1)

(after! company
  (use-package! company
    :config
    (setq company-global-modes '(not text-mode erc-mode circe-mode message-mode help-mode gud-mode vterm-mode))
    (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
    (set-company-backend! 'org-mode
      '(company-capf company-files :with company-yasnippet))
    (set-company-backend! 'sh-mode
      '(company-shell company-files :with company-yasnippet))
    (setq company-idle-delay 0.1)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 1)
    )
  )

(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "white" :background "red"
      :weight bold :height 140 :box (:width 1 :color "red")))
  )
