;;; org-roam-tags.el --- Description -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 26, 2024
;; Modified: October 26, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/org-roam-tag
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO: No way to remove other than manually.
;; TODO: Add org-roam-tags-sort-fn.
;; TODO: Make variable to enable/disable this.
;; TODO: Make sure I understand this.
;;
;;; Code:

(defun org-roam-tags-node-insert-hook (id link-description)
  "Update @PersonName tag when linking to their node in a task heading."
  (ignore link-description) ;; Make the byte-compiler happy.
  (let* ((node (org-roam-node-from-id id))
         (filetags (org-roam-node-tags node))
         (title (concat "@" (s-replace " " "" (org-roam-node-title node)))))
    (when (seq-contains-p filetags "person")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags (seq-uniq (cons title (org-get-tags nil t))))))))))

(add-hook 'org-roam-post-node-insert-hook #'org-roam-tags-node-insert-hook)

(provide 'org-roam-tags)
;;; org-roam-tag.el ends here
