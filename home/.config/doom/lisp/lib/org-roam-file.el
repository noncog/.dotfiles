;;; org-roam-file.el --- Integrate Denote and automatic org-roam file updates -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/org-roam-file.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file is used in conjunction with `org-roam-filetags.el' and `org-roam-agenda.el'
;; to provide support for automatically updating the filetags keyword and the org file's
;; name to be used in conjunction with Denote's file naming scheme.
;;
;; Tasks
;; - [ ] TODO: Integrate with org-roam-extract-subtree to support ID and Filetags.
;; - [ ] TODO: Add file-level property automatic updating.
;; - [ ] TODO: Add org-roam-file-rename-fn to enable using different functions.
;; - [ ] TODO: Develop Denote based rename function that can better handle various cases.
;;       - org-roam-file-denote-rename
;;       - cases: no id, no date, mismatched frontmatter, special tag casing @PersonName,
;;       - parent directory based tagging.
;;       - tag based refiling, tag based moving.
;;
;;; Code:

(require 'org-roam-utils)
(require 'org-roam-filetags) ;; NOTE: Also loads org-roam-agenda.
(require 'denote)

(defcustom org-roam-file-rename-file t
  "Non-nil means automatically rename org-roam buffer file on save."
  :type 'boolean
  :group 'org-roam)

;; TODO: Use defcustom and make this a list of strings.
(defcustom org-roam-file-rename-exclude nil
  "List of org files to exclude from being renamed using Denote file naming scheme.
When nil, always update file name."
  :type '(repeat 'file)
  :group 'org-roam)

(defun org-roam-file-name-update (title filetags date id)
  "Update org-roam node's filename using Denote's file naming scheme.

Used by the function `org-roam-file-pre-save-update' to update the filename
upon saving the buffer. The function provides the arguments TITLE, FILETAGS,
DATE, and ID.

Someday there will be the ability to use these arguments to call your own
file renaming function which could use these to conditionally rename the file.

Prior to updating the file name, the file tags are updated first to allow
their accurate state to be used for Denote compatible filenames which
includes filetags."
  (ignore title)
  (ignore filetags)
  (ignore date)
  (ignore id)
  ;; Rename file conditionally.
  (when (org-roam-file-p)
    (if org-roam-file-rename-exclude
        (let ((exclude-file-list (list)))
          ;; Expand file names before checking if file should be excluded.
          (dolist (file-name org-roam-file-rename-exclude exclude-file-list)
            (push (expand-file-name file-name) exclude-file-list))
          (unless (member (buffer-file-name) exclude-file-list)
            (denote-rename-file-using-front-matter (buffer-file-name))))
      (denote-rename-file-using-front-matter (buffer-file-name)))))

(defun org-roam-file-pre-save-h ()
  "Update filetags, file name, and `org-agenda-files' when node is saved.

This function is added to `before-save-hook' to automatically update the
filetags, file name, and possibly add it to the agenda files list."
  (let* ((title (org-roam--get-keyword "title"))
         (filetags (org-roam--get-keyword "filetags"))
         (date (org-roam--get-keyword "date"))
         (id (save-excursion (goto-char (point-min)) (org-roam-id-at-point))))
    (when (and (not (active-minibuffer-window))
               (org-roam-buffer-p))
      (org-roam-filetags-update title filetags)
      (when org-roam-file-rename-file
        (org-roam-file-name-update title filetags date id))
      (org-roam-agenda-files-update-h))))

;; Enable file renaming.
;; Uses anonymous function to hook before-save-hook after org-mode is loaded.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'org-roam-file-pre-save-h nil t)))

;; NOTE: Replaces org-roam's default file name using underscores to dashes
;;       for Denote file format integration.
;; TODO: Investigate if node can be ignored.
;; See: https://github.com/org-roam/org-roam/pull/1544
(cl-defmethod org-roam-node-slug :around ((node org-roam-node))
  "Return the slug of NODE."
  (string-replace "_" "-" (cl-call-next-method)))

(defun org-roam-file-property-p (property &optional value)
  "Return non-nil if buffer has file-level PROPERTY.

When optional argument VALUE is non-nil, only consider an entry
if it contains PROPERTY set to this value. If PROPERTY should be
explicitly set to nil, use string \"nil\" for VALUE.

If narrowing is in effect, only searches the visible part of the buffer."
  (let ((has-prop (org-find-property property value)))
    (if has-prop (org-with-point-at has-prop (org-before-first-heading-p)) nil)))

;; (defun my/org-roam-add-new-property-file-date ()
;;   "Set the 'DATE' file property using the 'buffer-file-name'."
;;   (interactive)
;;   (save-excursion
;;     (unless (my/org-roam-file-property-p "DATE")
;;       (org-set-property "DATE" (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time (denote-retrieve-filename-identifier (buffer-file-name))))))))

;; ;; TODO: Add ability to modify category based on other contexts such as log files.
;; ;; - This should ultimately work based off of a parent directory name list. Or use manual correction...

;; (defun my/org-roam-add-new-property-category ()
;;   "Set the 'CATEGORY' file property using the 'buffer-file-name'."
;;   (interactive)
;;   (save-excursion
;;     (unless (my/org-roam-file-property-p "CATEGORY")
;;       (org-set-property "CATEGORY" (org-get-title)))))

;; (add-hook 'org-roam-capture-new-node-hook #'my/org-roam-add-new-property-file-date 100)
;; (add-hook 'org-roam-capture-new-node-hook #'my/org-roam-add-new-property-category 100)

;; TODO: Somehow ensure that Denote's keyword (tag) front-matter is being added too!
;; - may be possible to define org-roam-node-${*} function to expand these variables.
;; (setq org-roam-extract-new-file-path (concat denote-id-format "--${slug}.org"))

(provide 'org-roam-file)
;;; org-roam-file.el ends here
