;;; org-roam-include.el --- Temporary -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-include.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides functions to choose what org headings (not files) are included in the org-roam database.
;; The goal is to keep the database clutter free and hopefully exclude superfluous org-ids from it.
;;
;;; Code:

(require 'org)

(defcustom org-roam-include-exclude-files t
  "List of org files to exclude headings from the org-roam database."
  :type '(repeat file)
  :group 'org-roam)

(defcustom org-roam-include-exclude-directories t
  "List of directories to exclude org file headings from the org-roam database."
  :type '(repeat directory)
  :group 'org-roam)

(defun org-roam-include-p ()
  "Function to decide if an org-roam node should be included in the database."
  (let ((file (buffer-file-name)))
    (if (or (if org-roam-include-exclude-files
                (member (file-name-nondirectory file) org-roam-include-exclude-files)
              nil)
            (if org-roam-include-exclude-directories
                (member (file-name-parent-directory file) org-roam-include-exclude-directories) nil))
        (not (org-current-level)) t)))

(provide 'org-roam-include)
;;; org-roam-include.el ends here
