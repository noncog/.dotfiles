;;; org-roam-agenda.el --- Integrate org-roam and org-agenda using filetags -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-agenda.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides functions for detecting and maintaining if an org-roam node is an agenda file
;;  and finding all nodes tagged as agenda files managed in conjunction with org-roam-filetags.el.
;;;
;;; Code:

;; TODO: Add setting for updating the database based on user cache settings.
;;       For example, if cache is set to not work on save, manually update cache,
;;       perhaps even in the background using async to ensure agenda files are up-to-date.
(defun org-roam-agenda-file-p ()
  "Return non-nil if current buffer has todo keywords or scheduled items.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks. The only exception is headings tagged as REFILE."
  ;; TODO: Add ignoring of certain directories/files.
  ;; TODO: Add more conditions.
  (interactive)
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (let ((todo-type (org-element-property :todo-type h)))
        (or
         ;; any headline with some todo keyword
         (eq 'todo todo-type)
         ;; any headline with REFILE tag (no inheritance)
         (seq-contains-p (org-element-property :tags h) "REFILE")
         ;; any non-todo headline with an active timestamp
         (and
          (not (eq 'done todo-type))
          (org-element-property :contents-begin h)
          (save-excursion
            (goto-char (org-element-property :contents-begin h))
            (let ((end (save-excursion
                         ;; we must look for active timestamps only
                         ;; before then next heading, even if it's
                         ;; child, but org-element-property
                         ;; :contents-end includes all children
                         (or
                          (re-search-forward org-element-headline-re
                                             (org-element-property :contents-end h)
                                             ':noerror)
                          (org-element-property :contents-end h)))))
              (re-search-forward org-ts-regexp end 'noerror)))))))
    nil 'first-match))

;; NOTE: Could use a flag to rely on either ripgrep against file name or query the database.
;; NOTE: Could also use vulpea compatibility.
(defun org-roam-agenda-files ()
  "Return a list of node files containing the 'agenda' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"agenda\"%"))]))))

;; TODO: Look into standard hook naming conventions.
(defun org-roam-agenda-files-update (&rest _)
  "A hook function to update the value of `org-agenda-files'."
  (setq org-agenda-files (org-roam-agenda-files)))

(advice-add 'org-agenda :before #'org-roam-agenda-files-update)
(advice-add 'org-todo-list :before #'org-roam-agenda-files-update)

;;; Unorganized:
;; Infer Org Agenda Categories
;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12(org-roam-agenda-category)%?-12t% s")
;;         (todo . " %i %-12(org-roam-agenda-category) ")
;;         (tags . " %i %-12(org-roam-agenda-category) ")
;;         (search . " %i %-12(org-roam-agenda-category) ")))

;; (defun org-roam-agenda-category ()
;;     "Get category of item at point for agenda.

;; Category is defined by one of the following items:

;; - CATEGORY property
;; - TITLE keyword
;; - TITLE property
;; - filename without directory and extension

;; Usage example:

;;   (setq org-agenda-prefix-format
;;         '((agenda . \" %(org-roam-agenda-category) %?-12t %12s\")))

;; Refer to `org-agenda-prefix-format' for more information."
;;     (let* ((file-name (when buffer-file-name
;;                         (file-name-sans-extension
;;                          (file-name-nondirectory buffer-file-name))))
;;            (title (org-roam--get-keyword "title"))
;;            (category (org-get-category)))
;;       (or (if (and title (string-equal category file-name)) title category) "")))

(provide 'org-roam-agenda)
;;; org-roam-agenda.el ends here
