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

;; Unify internal and external window management keybinds.
(use-package windman
  :config
  (setq windman-command "i3-msg "
        windman-focus-command "focus"
        windman-move-command "move"
        windman-direction-left "left"
        windman-direction-right "right"
        windman-direction-up "up"
        windman-direction-down "down")
  ;; I use super as my window manager modifier key.
  ;; These binds allow the window manager windowing keybinds to work within Emacs.
  (map!
   "C-s-h" #'windman-resize-win-left
   "C-s-j" #'windman-resize-win-down
   "C-s-k" #'windman-resize-win-up
   "C-s-l" #'windman-resize-win-right
   (:map global-map
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
         "s-Q" #'evil-quit)
   (:leader "w h" #'windman-focus-win-left
            "w j" #'windman-focus-win-down
            "w k" #'windman-focus-win-up
            "w l" #'windman-focus-win-right
            "w H" #'windman-move-win-left
            "w J" #'windman-move-win-down
            "w K" #'windman-move-win-up
            "w L" #'windman-move-win-right)))


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
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

(use-package magit
  :defer t
  :config
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/development/projects" . 1)
          ("~/development/source" . 1)))
  ;; Load my custom commit linter.
  (require 'magit-lint))
