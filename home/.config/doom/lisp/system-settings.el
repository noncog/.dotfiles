;;; system-settings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/system-settings
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;; TODO: Add descriptions.
;; TODO: Add function reference in descriptions.
;; TODO: Add usage in descriptions.
;; TODO: Credit david wilson.
;; TODO: Update all settings.
;; TODO: Add all system names.
;; TODO: Use in all other packages.

;;; Code:
(defvar system-settings-list
      (append
       ;; Put system-specific settings at the front so they're found first.
       (when (string= (system-name) "Callisto")
         '((desktop/dpi . 180)))
       (when (string= (system-name) "Europa")
         '((desktop/dpi . 180)))
       (when (string= (system-name) "Ganymede")
         '((desktop/dpi . 180)))
       '((desktop/dpi . 180)
         (notes-directory . "~/Documents/notes"))))

(defun system-settings-get (setting)
  "Usage system-settings-get SETTING."
  (alist-get setting system-settings-list))

;; Defines an identity for use with GPG, email, and file templates/snippets.
(when (string= user-login-name "noncog")
  (setq user-full-name "Jake Turner"
        user-mail-address "john@doe.com"))

;; Update the shell file when using macos to Bash. Similar done in init-term.el
(when (and (featurep :system 'macos)
          (file-exists-p "/opt/homebrew/bin/bash"))
 (setq shell-file-name "/opt/homebrew/bin/bash"))

(provide 'system-settings)
;;; system-settings.el ends here
