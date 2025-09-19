;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; TODO: Prevent minibuffer from scrolling windows.
;;       - Stillness-mode
;;       - Mini-ontop
;;       - Sinister
;; TODO: Check out cursor-intangible-mode.

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
(add-to-list 'load-path (expand-file-name "lisp/lib" doom-user-dir))
(add-to-list 'load-path (expand-file-name "lisp/init" doom-user-dir))

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

;;; Load Configuration Files

(load "+ui.el")
(load "+org.el")
(load "+shell.el")
(load "+project.el")
(load "+notes.el")
