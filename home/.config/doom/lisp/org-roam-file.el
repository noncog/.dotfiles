;;; org-roam-file.el --- Integrate Denote and automatic org-roam file updates -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/org-roam-file
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; TODO: Add id and date automatic "figuring out" whatever word I'm looking for here.
;;
(require 'org-roam-filetags) ;; NOTE: Also loads org-roam-agenda.

(defvar org-roam-file-rename-file t
  "Non-nil means rename file using Denote's file naming scheme.")

(defvar org-roam-file-rename-exclude nil
  "List of org files to exclude from being renamed using Denote file naming scheme.
When nil, always update file name.")

(defun org-roam-file-name-update (title filetags date id)
  "Updates org-roam node's filename according to flags."
  ;; NOTE: Accepts information from the node to handle renaming when Denote cannot.
  ;; TODO: Implement advanced renaming over Denote's simple renaming using file name.
  ;; TODO: Handle case of no date, no-id, no correct file frontmatter.
  ;;       Or find a method to let Denote do it like denote prompt funcs.
  ;; NOTE: Could make another function org-roam-file-denote-rename
  ;; TODO: Consider adding date property or file tag automatic.
  ;; TODO: this breaks if file name is not set correctly. Check if has and update then do denote?
  ;; NOTE: Consider creating org-roam-file-rename-fn and call it separately.
  (ignore title)
  (ignore filetags)
  (ignore date)
  (ignore id)
  ;; Rename file conditionally.
  (when (eq major-mode 'org-mode)
    (when org-roam-file-rename-file
      (if org-roam-file-rename-exclude
          (let (exclude-file-list)
            (dolist (file-name org-roam-file-rename-exclude exclude-file-list)
              (push (expand-file-name file-name) exclude-file-list))
            (unless (member (buffer-file-name) exclude-file-list)
              (denote-rename-file-using-front-matter (buffer-file-name))))
        (denote-rename-file-using-front-matter (buffer-file-name))))))

;; TODO: Look into hook naming format.
(defun org-roam-file-pre-save-hook ()
  "Update filetags, file name, and `org-agenda-files' when node is saved."
  (let* ((title (org-roam--get-keyword "title"))
         (filetags (org-roam--get-keyword "filetags"))
         (date (org-roam--get-keyword "date"))
         (id (save-excursion (goto-char (point-min)) (org-roam-id-at-point))))
  (when (and (not (active-minibuffer-window))
             (org-roam-buffer-p))
    (progn (org-roam-filetags-update title filetags)
           (org-roam-file-name-update title filetags date id)
           (org-roam-agenda-files-update)))))

;; Enable file renaming.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'org-roam-file-pre-save-hook nil t)))

;; NOTE: Replaces org-roam's default file name using underscores to dashes
;;       for Denote file format integration.
;; See: https://github.com/org-roam/org-roam/pull/1544
;; TODO: Investigate if node can be ignored.
(cl-defmethod org-roam-node-slug :around ((node org-roam-node))
  (string-replace "_" "-" (cl-call-next-method)))

;; TODO: Somehow ensure that Denote's keyword (tag) front-matter is being added too!
;; (setq org-roam-extract-new-file-path (concat denote-id-format "--${slug}.org"))

;; TODO: Add property hooks and switches:
;;
;; This develops the ability to add properties, primarily the "DATE" property from Denote for newly created Org Roam nodes.
;; (defun my/org-roam-file-property-p (property)
;;   (let ((has-prop (org-find-property property)))
;;     (when has-prop (org-with-point-at has-prop (org-before-first-heading-p)))))

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

(provide 'org-roam-file)
;;; org-roam-file.el ends here
