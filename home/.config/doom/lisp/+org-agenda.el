;;; +org-agenda.el --- My org agenda configurations -*- lexical-binding: t; -*-

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
                            (org-agenda-prefix-format '((agenda . "  %?-5t %?:c ")))
                                        ;(org-agenda-prefix-format '((agenda . "   %?-5t %?:(org-roam-agenda-category) ")))
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
                            (org-agenda-prefix-format '((agenda . " %(org-roam-agenda-category) %-5t ")))
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
