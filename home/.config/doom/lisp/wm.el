;;; wm.el --- Window manager integration using keybind passthrough -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 18, 2024
;; Modified: October 18, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/wm.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package uses keybind passthrough to allow your window manager's keybinds to work transparently with Emacs.
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
;; TODO: Remove IS-LINUX doom specific macro.
;; TODO: Reduce verbosity by using arguments instead of many functions.
;; TODO: Add defcustoms to make the package more universal/customizable.
;; TODO: Maybe add ability to search for known window manager names.
;; TODO: Maybe set ability to change current window manager.
;; TODO: Fix directional resizing. (Issue when more than binary windows exist in directions.)
;; TODO: Remove autoloads.
;; NOTE: Could use alist like system-settings and set per window manager.
;; NOTE: https://github.com/SqrtMinusOne/dotfiles/blob/master/.emacs.d/init.el#L397
;;; Code:

(defun wm-win-cmd-in-direction (wm-cmd direction emacs-fn)
  (let ((tell-wm (concat (cond (IS-LINUX "i3-msg ") (IS-MAC "yabai -m window --")) wm-cmd " " direction)))
    (condition-case nil (funcall emacs-fn)
      (user-error (apply #'start-process "wm" nil (split-string tell-wm))))))

(defun wm-focus-win-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-left)))

(defun wm-focus-win-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-right)))

(defun wm-focus-win-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-up)))

(defun wm-focus-win-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south"))))
    (wm-win-cmd-in-direction "focus" direction #'windmove-down)))

(defun wm-move-win-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-left)))

(defun wm-move-win-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-right)))

(defun wm-move-win-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-up)))

(defun wm-move-win-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south")))
        (move-cmd (cond (IS-LINUX "move") (IS-MAC "swap"))))
    (wm-win-cmd-in-direction move-cmd direction #'windmove-swap-states-down)))

(defun wm-resize-win-left ()
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally 10) (enlarge-window-horizontally -10)))

(defun wm-resize-win-right ()
  (interactive)
  (if (window-in-direction 'left nil nil nil nil) (enlarge-window-horizontally -10) (enlarge-window-horizontally 10)))

(defun wm-resize-win-up ()
  (interactive)
  (if (window-in-direction 'above nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(defun wm-resize-win-down ()
  (interactive)
  (if (window-in-direction 'below nil nil nil nil) (enlarge-window 5) (enlarge-window -5)))

(provide 'wm)
;;; wm.el ends here
