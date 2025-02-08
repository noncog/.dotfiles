;;; init-keys.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-keys
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(when (featurep :system 'macos)
  (progn
    (setq mac-command-modifier 'control ;; Maps Command -> Control
          mac-control-modifier 'meta    ;; Maps Control -> Alt (Meta)
          mac-option-modifier 'super)   ;; Maps Option -> Super
    ;; Frame fullscreen toggle is broken on macOS, unbind it.
    ;; TODO: Investigate if still an issue:
    (map! "C-s-f" nil
          :leader "t F" nil)))

;; Edit Doom's default toggle keybinds.
;; For some reason, Doom has several 'toggle' binds under the 'help' menu.
;; I move them to the 'toggle' menu and add some others.
(map! :leader "h t" nil
      :leader "t t" #'load-theme
      :leader "h T" nil
      :leader "t T" #'doom/toggle-profiler
      :leader :desc "Menu-bar mode" "t m" #'toggle-menu-bar-mode-from-frame)

(provide 'init-keys)
;;; init-keys.el ends here
