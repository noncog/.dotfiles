;;; org-roam-keyword.el --- Temporary -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-keyword.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The motivation for writing this package was to fix an issue with org-roam's
;; keyword setting functions prior to upstreaming them. Primarily,
;; `org-roam-set-keyword' will alternate creating an empty keyword line if
;; provided an empty string then remove the keyword line if called again.
;;
;; Coming soon:
;; In addition to this fix, the redefined function ensures that the filetags
;; are added to the end of the metadata (front matter) rather than after the
;; property drawer and title keyword line. This is done by using the optional
;; parameter of `org-roam-end-of-meta-data'
;;
;; In the future I would like to add keyword sorting and variables to toggle
;; other related behavior.
;;
;; Comments:
;; All of this ideally needs replaced by another package or complete integration
;; with Denote.
;;
;; The file metadata (front matter) is all capable of being handled by Org and
;; Denote. Org-roam seems to have very broken insertion and no capability of
;; sorting. I'm finding more and more poor code in Org roam. Hence this file.
;;
;; All of this can be replaced with the following:
;; - Consider limiting research forward keyword search.
;; -  Look into and goals:
;;    - org-macro for more reliably finding this and sorting it.
;;    - Rework this to support setting keyword order.
;;    - org-make-options-regexp
;;    - denote-prepend-front-matter
;;    - denote--get-front-matter-components-order
;;    - denote--get-final-components-for-rewrite.
;;    - denote-rewrite-front-matter.
;;    - denote-add-front-matter-prompt.
;;    - denote-rename-file.
;;    - denote-rename-file-using-front-matter.
;;    - denote-front-matter-components-present-even-if-empty-value.
;;    - denote-sort-keywords.
;;    - denote-rename-confirmations.
;;
;;; Code:

;; (require 'org-roam-utils)

(fmakunbound 'org-roam-set-keyword)

(defcustom org-roam-keyword-remove-blank t
  "Remove org keyword line when updated to blank value."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-keyword-filetags-last t
  "Insert filetags keyword after all other keywords."
  :type 'boolean
  :group 'org-roam)

(defun org-roam-keyword-set (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced.

If `org-roam-keyword-remove-blank' is non-nil, when passed
a blank VALUE and the keyword line exists, remove it. If
the keyword line does not exist and VALUE is blank, do not
inset the keyword line. Otherwise, allow blank keyword lines."
  (org-with-point-at 1
    (let ((case-fold-search t)
          (non-blank-value (not (string-blank-p value))))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if non-blank-value
              (replace-match (concat " " value) 'fixedcase nil nil 1)
            (kill-whole-line))
        (if (and org-roam-keyword-filetags-last
                 (string-equal-ignore-case key "filetags"))
            ;; Method to ensure filetags are inserted last in list.
            (org-roam-keyword-end-of-meta-data)
          ;; Default method appears to insert new keywords after #+title.
          (org-roam-end-of-meta-data 'drawers))
        ;;(beginning-of-line)
        ;; (insert "\n" "#+" key ": " value "\n")
        (if (save-excursion (end-of-line) (eobp))
            (progn
              (end-of-line)
              (insert "\n"))
          ;; Hack to ensure correct line spacing for both insert cases.
          (when (not org-roam-keyword-filetags-last)
            (forward-line)
            (beginning-of-line)))
        (when non-blank-value (insert "#+" key ": " value "\n"))))))

(defalias 'org-roam-set-keyword #'org-roam-keyword-set)

(defun org-roam-keyword-end-of-meta-data ()
  "Like `org-roam-end-of-meta-data', but accurately skips to end of keywords."
  (goto-char (point-min))
  ;; Skip property drawer.
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line))
  ;; Skip to end of keywords.
  (while (looking-at org-keyword-regexp)
    (goto-char (match-end 0))
    (forward-line)))

(provide 'org-roam-keyword)
;;; org-roam-keyword.el ends here
