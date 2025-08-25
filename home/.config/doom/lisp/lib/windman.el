;;; windman.el --- Window manager integration using keybind passthrough -*- lexical-binding: t; -*-
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
;; Integrate your window management keybinds seamlessly into Emacs.
;;
;; Windman is an extension of windmove. It works by calling the equivalent window manager functions
;; whenever windmove commands return nil. To use the same window management keybinds, your window
;; manager must be able to pass the key to Emacs (keybind-passthrough). Not all window managers have
;; built-in support for this. You also must have an IPC interface or shell script that directional
;; based window management commands can be sent to.
;;
;; For use with i3wm, a small script must be used to activate a mode that does not contain the keybinds
;; to be passed through whenever Emacs is the focused window so they can be used within Emacs itself.
;; I've used a small perl script to achieve this. (~/.dotfiles/home/.config/i3/i3-emacs-mode.pl)
;;
;; Although no longer maintained, on macOS SKHD (for use with Yabai) supports pass-through natively.
;;
;; Current limitations:
;; - Directional window resizing doesn't work well when more than two windows exist in a direction.
;; - Currently not possible to move or resize the frame from within itself.
;;
;; Similar implementation:
;; - https://github.com/SqrtMinusOne/dotfiles/blob/master/.emacs.d/modules/sqrt-i3.el
;;
;;; Code:

(require 'windmove)

(defgroup windman nil
  "Integrates window management keybinds with Emacs for transparent window control."
  :link '(url-link "https://github.com/noncog/.dotfiles/home/.config/doom/lisp/lib/windman.el")
  :group 'convenience)

(defcustom windman-command nil
  "The command to use to send commands to your window manager."
  :type '(string)
  :group 'windman)

(defcustom windman-focus-command nil
  "The sub-command to use to focus a window for your window manager."
  :type '(string)
  :group 'windman)

(defcustom windman-move-command nil
  "The sub-command to use to move a window for your window manager."
  :type '(string)
  :group 'windman)

;; The following `direction' variables hold the name that your window manage uses for
;; a given direction. Some use cardinal directions, e.g. north/south. Some use simple
;; directions, e.g. up, down, etc.

(defcustom windman-direction-left nil
  "The sub-command to use for windows in `left' direction for your window manager."
  :type '(string)
  :group 'windman)

(defcustom windman-direction-right nil
  "The sub-command to use for windows in `right' direction for your window manager."
  :type '(string)
  :group 'windman)

(defcustom windman-direction-up nil
  "The sub-command to use for windows in `up' direction for your window manager."
  :type '(string)
  :group 'windman)

(defcustom windman-direction-down nil
  "The sub-command to use for windows in `down' direction for your window manager."
  :type '(string)
  :group 'windman)

(defun windman-cmd-in-direction (windman-cmd direction emacs-fn)
  "Function to execute either a WINDMAN-CMD or EMACS-FN for a DIRECTION."
  (let ((send-windman (concat windman-command windman-cmd " " direction)))
    (condition-case nil (funcall emacs-fn)
      (user-error (apply #'start-process "windman" nil (split-string send-windman))))))

(defun windman-focus-win-left ()
  "Focus a window in the `left' direction."
  (interactive)
  (windman-cmd-in-direction windman-focus-command windman-direction-left #'windmove-left))

(defun windman-focus-win-right ()
  "Focus a window in the `right' direction."
  (interactive)
  (windman-cmd-in-direction windman-focus-command windman-direction-right #'windmove-right))

(defun windman-focus-win-up ()
  "Focus a window in the `up' direction."
  (interactive)
  (windman-cmd-in-direction windman-focus-command windman-direction-up #'windmove-up))

(defun windman-focus-win-down ()
  "Focus a window in the `down' direction."
  (interactive)
  (windman-cmd-in-direction windman-focus-command windman-direction-down #'windmove-down))

(defun windman-move-win-left ()
  "Move a window in the `left' direction."
  (interactive)
  (windman-cmd-in-direction windman-move-command windman-direction-left #'windmove-swap-states-left))

(defun windman-move-win-right ()
  "Move a window in the `right' direction."
  (interactive)
  (windman-cmd-in-direction windman-move-command windman-direction-right #'windmove-swap-states-right))

(defun windman-move-win-up ()
  "Move a window in the `up' direction."
  (interactive)
  (windman-cmd-in-direction windman-move-command windman-direction-up #'windmove-swap-states-up))

(defun windman-move-win-down ()
  "Move a window in the `down' direction."
  (interactive)
  (windman-cmd-in-direction windman-move-command windman-direction-down #'windmove-swap-states-down))

(defun windman-resize-win-left ()
  "Resize a window in the `left' direction."
  (interactive)
  (if (window-in-direction `left nil nil nil nil) (enlarge-window-horizontally 10) (enlarge-window-horizontally -10)))

(defun windman-resize-win-right ()
  "Resize a window in the `right' direction."
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally -10) (enlarge-window-horizontally 10)))

(defun windman-resize-win-up ()
  "Resize a window in the `up' direction."
  (interactive)
  (if (window-in-direction 'above nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(defun windman-resize-win-down ()
  "Resize a window in the `down' direction."
  (interactive)
  (if (window-in-direction 'below nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(provide 'windman)
;;; windman.el ends here
