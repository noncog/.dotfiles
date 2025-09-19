;;; org-roam-agenda.el --- Integrate org-roam and org-agenda using filetags -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-agenda.el
;; Package-Requires: ((emacs "29.1"))
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
;; TODO: Consider moving to org-ql if it is faster than org-element-property for parsing the file.
;; TODO: Look into adding org-roam-agenda-category.

(require 'org-element)
(require 'org-roam-db)
(require 'org-agenda)

;; NOTE: Could use a flag to rely on either ripgrep against file name or query the database.
;; NOTE: Could also use vulpea compatibility. Could copy how vulpea does query by tags some.
;; NOTE: Consider adding project tag to always include them.
(defun org-roam-agenda-files ()
  "Return a list of org-roam node files containing the `agenda' tag."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"agenda\"%"))]))))

;; TODO: Look into honoring value of org-agenda-files.
;; TODO: Look into ensuring certain agenda files are always kept.
(defun org-roam-agenda-files-update-h (&rest _)
  "A hook function to update the value of `org-agenda-files'."
  (setq org-agenda-files (org-roam-agenda-files)))

(advice-add 'org-agenda :before #'org-roam-agenda-files-update-h)
(advice-add 'org-todo-list :before #'org-roam-agenda-files-update-h)
(add-to-list 'org-tags-exclude-from-inheritance "agenda")

;; Infer Org Agenda Categories

(defun org-roam-agenda-category ()
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Refer to `org-agenda-prefix-format' for more information."
  ;; (when buffer-file-name
  ;;   (let* ((file-name (when buffer-file-name
  ;;                       (file-name-sans-extension
  ;;                        (file-name-nondirectory buffer-file-name))))
  ;;          (title (org-roam--get-keyword "title"))
  ;;          ;; NOTE: Cannot use org-get-category in Org 9.7+
  ;;          (category (org-get-category)))
  ;;     (message "file name: %s" file-name)
  ;;     (message "title: %s" title)
  ;;     (message  "category: %s" category)
  ;;     (or (if (and title (string-equal category file-name)) title category) ""))))

  ;; NOTE: This kind of works, but isn't great.
  (when buffer-file-name
    (with-current-buffer (find-buffer-visiting buffer-file-name)
      (let* ((file-name (when buffer-file-name
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name))))
             (title (org-get-title (current-buffer)))
             ;; NOTE: Cannot use org-get-category in Org 9.7+
             (category (org-get-category)))
        ;; (message "file name: %s" file-name)
        ;; (message "title: %s" title)
        ;; (message  "category: %s" category)
        (or (if (and title (string-equal category file-name)) title category) "")))))
;;   (let ((category (org-agenda-get-category))
;;         (title (when buffer-file-name
;;                  (with-current-buffer (find-buffer-visiting buffer-file-name) (org-roam--get-keyword "title")))))
;;     buffer-file-name))
;; (or (if (and title (string-equal category file-name)) title category) "")))
;; (let* ((file-name "")
;;        (title "")
;;        (category ""))
;;   (when buffer-file-name
;;     (message "file name: %s" buffer-file-name))
;;                                       ;(setq title (with-current-buffer (find-buffer-visiting buffer-file-name) (org-roam--get-keyword "title"))))
;;   (setq category (org-agenda-get-category))

;;   (message "title: %s" title)
;;   (message  "category: %s" category)
;;   (or (if (and title (string-equal category file-name)) title category) " ")))

;; (when buffer-file-name (file-name-sans-extension
;;                      (file-name-nondirectory buffer-file-name)))
;; (let* ((file-name "")
;;        (title "")
;;        (category ""))
;;   (when buffer-file-name
;;     (setq file-name
;;           (file-name-sans-extension
;;            (file-name-nondirectory buffer-file-name)))
;;     (with-current-buffer (find-buffer-visiting buffer-file-name)
;;       (setq title (org-roam--get-keyword "title"))
;;       )
;;     )

;;   (setq category (org-agenda-get-category))
;;   (message "file name: %s" file-name)
;;   (message "title: %s" title)
;;   (message  "category: %s" category)
;;   (or (if (and title (string-equal category file-name)) title category) " ")
;;   ))
;; (let* ((file-name (when buffer-file-name
;;                     (file-name-sans-extension
;;                      (file-name-nondirectory buffer-file-name))))
;;        (title (org-roam--get-keyword "title"))
;;        ;; NOTE: Cannot use org-get-category in Org 9.7+
;;        (category (org-agenda-get-category)))
;; (message "file name: %s" file-name)
;; (message "title: %s" title)
;; (message  "category: %s" category)
;; (or (if (and title (string-equal category file-name)) title category) " ")))


(provide 'org-roam-agenda)
;;; org-roam-agenda.el ends here
