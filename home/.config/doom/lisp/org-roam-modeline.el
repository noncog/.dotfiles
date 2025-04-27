;;; org-roam-modeline.el --- Integrate Org-Roam and Denote file name with Doom Modeline -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-modeline.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides a function to process the buffer file name to remove or reformat
;; the Denote file naming scheme when visiting org-roam buffers using it.
;;
;;; Code:
(require 'denote)

;; TODO: Look into how Teco does this.
(defun org-roam-modeline-process-buffer-file-name (file-name)
  "Return FILE-NAME for buffer filtered if necessary for display in Doom Modeline."
  (when (eq major-mode 'org-mode)
    ;; NOTE: Provides encryption detection.
    (if (denote-filename-is-note-p file-name)
        (replace-regexp-in-string
         (concat denote-id-regexp "--") ""
         (replace-regexp-in-string denote-keywords-regexp ".org" file-name))
      file-name)))

(provide 'org-roam-modeline)
;;; org-roam-modeline.el ends here
