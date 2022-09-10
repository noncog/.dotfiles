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

(custom-set-faces!
 `(cursor :background ,(doom-color 'magenta)))

(doom/set-frame-opacity 96)

(set-popup-rule! "^brain.org" :side 'right :vslot -1 :width 70 :modeline t :select t :quit nil)
(set-popup-rule! "^*Org Agenda*" :side 'right :vslot 1 :width 70 :modeline t :select t :quit nil)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
(map! :leader :desc "Brain.org" "b t" #'noncog/toggle-brain)
(map! :leader :desc "Kill org noter session" "n k" #'noncog/kill-org-noter-session)

(display-time-mode 1)

(after! emacs
  (use-package! emacs
    :defer
    :config
    (setq scroll-margin 5)
    ;; scroll buffer around point
    (global-set-key (kbd "M-p") #'scroll-down-line)
    (global-set-key (kbd "M-n") #'scroll-up-line)
    (global-set-key (kbd "C-c b") #'noncog/toggle-brain)
    ;(global-set-key (kbd "f5") #'which-key-show-previous-page-cycle)
    ;(global-set-key (kbd "f6") #'which-key-show-next-page-cycle)
    )
  )

(setq org-directory "~/documents/org/")
(setq org-noter-notes-search-path '("~/documents/org/noter"))

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
    ;(add-to-list 'org-modules 'org-tempo t)             ; enable org-tempo
    (setq org-agenda-files (quote ("~/documents/org/brain.org"
                                   "~/documents/org/university/linear-algebra-3720"
                                   "~/documents/org/university/data-structures-and-algorithms-5870"
                                   "~/documents/org/university/network-concepts-and-administration-3723"
                                   "~/documents/org/university/software-engineering-5801"
                                   "~/documents/org/university/information-assurance-3755")))
    ;(setq org-habit-show-habits-only-for-today nil)
    ;(setq org-habit-show-all-today t)
    ;(setq org-agenda-repeating-timestamp-show-all nil)
    (setq org-agenda-start-with-log-mode t)             ; show 'completed' done items in agenda
    (setq org-log-done 'time)                           ; add completion time to DONE items.
    (setq org-log-into-drawer t)                        ; puts log times into a drawer to hide them
    (setq org-return-follows-link t)                    ; enter opens links in org
    (setq org-capture-bookmark nil)                     ; prevent org capture from adding to bookmarks list
    (setq org-insert-heading-respect-content nil)       ; insert the heading at cursor, not at end
    (setq org-imenu-depth 10)
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

(defun noncog/kill-org-noter-session ()
  "Automatically closes pdfs when done noting them."
  (interactive)
  (let ((pdf-fname (buffer-file-name (org-noter--session-doc-buffer org-noter--session))) (ses-id (org-noter--session-id org-noter--session))) (set-window-dedicated-p (get-buffer-window (get-file-buffer pdf-fname)) nil) (call-interactively #'org-noter-kill-session ses-id) (doom/kill-this-buffer-in-all-windows (get-file-buffer pdf-fname))))

(after! org-noter
  (use-package! org-noter
    :config
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

(defun noncog/pulsar-scroll-recenter (&rest _args)
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
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)
  (advice-add 'evil-scroll-up :after #'noncog/pulsar-scroll-recenter)
  (advice-add 'evil-scroll-down :after #'noncog/pulsar-scroll-recenter)
  )

(pulsar-global-mode 1)
