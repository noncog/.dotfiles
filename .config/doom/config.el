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

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(global-auto-revert-mode 1)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(map! :leader :desc "Brain.org" "b t" #'noncog/toggle-brain)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-dracula)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'normal'))

(setq doom-font (font-spec :family "Fira Code" :size 14))
(setq doom-unicode-font (font-spec :family "Fira Code" :size 16))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;(set-cursor-color "#ff79c6")
;(evil-set-cursor-color "#ff79c6")
;(+evil-update-cursor-color-h)

(setq display-line-numbers-type 'visual)

(set-popup-rule! "^brain.org" :side 'right :width 70 :select t :quit nil)

;; file and a sentinel variables
(defconst noncog/brain-file "/home/jake/documents/org/brain.org")
(defvar noncog/brain-visible nil)

(defun noncog/toggle-brain ()
  "A function for toggling the view of the your chosen file in a side window."
  (interactive)
  (if (and noncog/brain-visible (get-buffer-window (get-file-buffer noncog/brain-file)) t)
      (let ((buffer (get-file-buffer noncog/brain-file))) ;; visible
        (delete-window (get-buffer-window buffer))
        (kill-buffer buffer)
        (setq noncog/brain-visible nil))
    (progn ;; not visible
      (display-buffer (find-file-noselect noncog/brain-file))
      (setq noncog/brain-visible t))))

(after! emacs
  (use-package! emacs
    :defer
    :config
    ;; scroll buffer around point
    (global-set-key (kbd "M-p") #'scroll-down-line)
    (global-set-key (kbd "M-n") #'scroll-up-line)
    (global-set-key (kbd "C-c b") #'noncog/toggle-brain)
    )
  )

(setq org-directory "~/documents/org/")
(setq org-noter-notes-search-path '("~/documents/org/noter"))

(after! org
  
  (use-package! org
    :config
    (setq org-modules (quote (org-habit)))
    
    ;(setq org-habit-show-habits-only-for-today nil)
    ;(setq org-habit-show-all-today t)
    ;(setq org-agenda-repeating-timestamp-show-all nil)
    (setq org-agenda-start-with-log-mode t)             ; show 'completed' done items in agenda
    (setq org-log-done 'time)                           ; add completion time to DONE items.
    (setq org-log-into-drawer t)                        ; puts log times into a drawer to hide them
    (setq org-return-follows-link t)                    ; enter opens links in org
    (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks list
    (setq org-insert-heading-respect-content nil)       ; insert the heading at cursor, not at end
    (setq org-ellipsis " ▾")                            ; set custom ellipsis
    ;(setq org-edit-src-content-indentation 0)           ; prevent adding spaces/indents to
    (setq org-hide-emphasis-markers t)                  ; hide formatting for markup
    )
  )

(after! org-noter
  (use-package! org-noter
    :config
    (setq org-noter-always-create-frame nil)
    (setq org-noter-auto-save-last-location t)
    (setq org-noter-separate-notes-from-heading t)
    )
  )

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-page))
