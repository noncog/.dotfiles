;;; org-file.el --- Functions for modifying file-level properties of org files -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: August 27, 2025
;; Modified: August 27, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/lib/org-file.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO: Description.
;;
;; Tasks:
;; TODO: Add keyword sorting.
;; TODO: Develop buffer narrow check macro.
;;
;;; Code:
(require 'org)
(require 'org-element)

(defcustom org-file-tag-agenda nil
  "A file tag that should be added to a file to denote it as an agenda file."
  :group 'org-agenda
  :type 'string :tag "Tag")

(defcustom org-file-agenda-tags nil
  "List of tags a heading can have to denote the file as an agenda file."
  :group 'org-agenda
  :type '(repeat (string :tag "Tag")))

(defcustom org-file-agenda-keywords nil
  "List of TODO keywords a heading can have to denote the file as an agenda file."
  :group 'org-agenda
  :type '(repeat (string :tag "Keyword")))

(defun org-file-property-p (property &optional value)
  "Return non-nil if org buffer has file-level PROPERTY.

When optional argument VALUE is non-nil, only consider an entry
if it contains PROPERTY set to this value. If PROPERTY should be
explicitly set to nil, use string \"nil\" for VALUE."
  (org-with-point-at 1
    (let ((has-prop (org-find-property property value)))
      (if (and has-prop (progn (goto-char has-prop) (org-before-first-heading-p)))
          (if value t (org-entry-get has-prop property nil t)) nil))))

(defun org-file-agenda-elements-p ()
  "Return non-nil if current file buffer has elements considered for the agenda.

Searches for entries in the active buffer containing:
- Active timestamps under headings.
- TODO keywords found in `org-file-agenda-keywords'.
- Tags found in `org-file-agenda-tags'."
  (when (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (h)
            (let ((todo (org-element-property :todo-keyword h))
                  (todo-type (org-element-property :todo-type h)))
              (or (when org-file-agenda-keywords
                    ;; Any headline with selected todo keywords.
                    ;; Could add fallback to keywords of type todo.
                    (seq-contains-p org-file-agenda-keywords todo))
                  (when org-file-agenda-tags
                    ;; Any headline with selected tags.
                    (seq-intersection (org-element-property :tags h) org-file-agenda-tags))
                  (and
                   ;; Any non-todo headline with an active timestamp.
                   (not (eq 'done todo-type))
                   (org-element-property :contents-begin h)
                   (save-excursion
                     (goto-char (org-element-property :contents-begin h))
                     ;; Must look for active timestamps only before next
                     ;; heading, even if it's a child, but org-element-property
                     ;; :contents-end includes all children.
                     (let ((end (save-excursion
                                  (or (re-search-forward
                                       org-element-headline-re
                                       (org-element-property :contents-end h) ':noerror)
                                      (org-element-property :contents-end h)))))
                       (re-search-forward org-ts-regexp end 'noerror))))))) nil 'first-match) t))

(defun org-file-agenda-p ()
  "Return non-nil if current file buffer should be considered for the agenda.

Searches for elements considered by `org-file-agenda-elements-p'.

Force inclusion or exclusion with file property `AGENDA' set to t or nil."
  (interactive)
  (let ((agenda-property (org-file-property-p "agenda")))
    (cond ((and agenda-property (string= "t" agenda-property)) t)
          ((and agenda-property (string= "nil" agenda-property)) nil)
          (t (if (buffer-narrowed-p)
                 (org-with-wide-buffer
                  (org-file-agenda-elements-p))
               (org-file-agenda-elements-p))))))

(defun org-file-end-of-meta-data ()
  "Skip to end of properties drawer and keywords in file.

Similar to `org-end-of-meta-data' but for file level meta-data."
  (if (buffer-narrowed-p)
      (user-error "Unable to move to top-level (file) meta-data in narrowed buffer")
    (goto-char (point-min))
    (when (looking-at org-property-drawer-re)
      (goto-char (match-end 0))
      (forward-line))
    ;; Skip to end of keywords.
    (while (looking-at org-keyword-regexp)
      (goto-char (match-end 0))
      (forward-line))))

(defun org-file-get-keyword (name)
  "Return value of first occurence of keyword NAME in file."
  (org-macro--find-keyword-value name))

(provide 'org-file)
;;; org-file.el ends here
