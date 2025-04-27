;;; wm.el --- Window manager integration using keybind passthrough -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 noncog
;;
;; Author: noncog
;; Maintainer: noncog
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/wm.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Uses keybind passthrough to allow your window manager's keybinds to work transparently within Emacs.
;;
;; Window management works in both directions moving into and out of Emacs.
;;
;; To use this effectively, your window manager or keybind manager must have some means of sending your window
;; management keybinds directly to Emacs when it is the focused application. Not all window managers have built-in
;; support for this. For example, a small perl script must be used with i3 along with an additional mode dedicated to Emacs
;; where the typical window management keys are unbound while in that mode or focused on Emacs. Others like sxhkd for macOS
;; support this natively to have per-application keybinds. Others require tinkering like using dedicated modes in i3.
;;
;; Currently, it's not possible to move the Emacs frame from within itself, instead it tries to move the focused Emacs window.
;;
;;; Tasks:
;; TODO: Fix directional resizing. (Issue when more than binary windows exist in directions.)
;; NOTE: https://github.com/SqrtMinusOne/dotfiles/blob/master/.emacs.d/init.el#L397

;;; Code:

(require 'windmove)

(defgroup wm nil
  "Integrates window management keybinds with Emacs for transparent window control."
  :link '(url-link "https://github.com/noncog/.dotfiles/home/.config/doom/lisp/wm.el")
  :group 'convenience)

(defcustom wm-command nil
  "The command to use to send commands to your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-focus-command nil
  "The sub-command to use to focus a window for your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-move-command nil
  "The sub-command to use to move a window for your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-direction-left nil
  "The sub-command (name) to use for windows in 'left' direction for your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-direction-right nil
  "The sub-command (name) to use for windows in 'right' direction for your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-direction-up nil
  "The sub-command (name) to use for windows in 'up' direction for your window manager."
  :type '(string)
  :group 'wm)

(defcustom wm-direction-down nil
  "The sub-command (name) to use for windows in 'down' direction for your window manager."
  :type '(string)
  :group 'wm)

(defun wm-win-cmd-in-direction (wm-cmd direction emacs-fn)
  "Wrapper function to execute a window manager command or Emacs command on a window."
  (let ((send-wm (concat wm-command wm-cmd " " direction)))
    (condition-case nil (funcall emacs-fn)
      (user-error (apply #'start-process "wm" nil (split-string send-wm))))))

(defun wm-focus-win-left ()
  "Focus a window in the 'left' direction."
  (interactive)
  (wm-win-cmd-in-direction wm-focus-command wm-direction-left #'windmove-left))

(defun wm-focus-win-right ()
  "Focus a window in the 'right' direction."
  (interactive)
  (wm-win-cmd-in-direction wm-focus-command wm-direction-right #'windmove-right))

(defun wm-focus-win-up ()
  "Focus a window in the 'up' direction."
  (interactive)
  (wm-win-cmd-in-direction wm-focus-command wm-direction-up #'windmove-up))

(defun wm-focus-win-down ()
  "Focus a window in the 'down' direction."
  (interactive)
  (wm-win-cmd-in-direction wm-focus-command wm-direction-down #'windmove-down))

(defun wm-move-win-left ()
  "Move a window in the 'left' direction."
  (interactive)
  (wm-win-cmd-in-direction wm-move-command wm-direction-left #'windmove-swap-states-left))

(defun wm-move-win-right ()
  "Move a window in the 'right' direction."
  (interactive)
    (wm-win-cmd-in-direction wm-move-command wm-direction-right #'windmove-swap-states-right))

(defun wm-move-win-up ()
  "Move a window in the 'up' direction."
  (interactive)
    (wm-win-cmd-in-direction wm-move-command wm-direction-up #'windmove-swap-states-up))

(defun wm-move-win-down ()
  "Move a window in the 'down' direction."
  (interactive)
    (wm-win-cmd-in-direction wm-move-command wm-direction-down #'windmove-swap-states-down))

(defun wm-resize-win-left ()
  "Resize a window in the 'left' direction."
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally 10) (enlarge-window-horizontally -10)))

(defun wm-resize-win-right ()
  "Resize a window in the 'right' direction."
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally -10) (enlarge-window-horizontally 10)))

(defun wm-resize-win-up ()
  "Resize a window in the 'up' direction."
  (interactive)
  (if (window-in-direction 'above nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(defun wm-resize-win-down ()
  "Resize a window in the 'down' direction."
  (interactive)
  (if (window-in-direction 'below nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(provide 'wm)
;;; wm.el ends here
