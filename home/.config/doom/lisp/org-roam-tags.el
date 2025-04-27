;;; org-roam-tags.el --- Functions for adding tags on node insert -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 26, 2024
;; Modified: October 26, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-tags.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides functions to automatically insert tags to org-roam nodes upon linking others.
;;
;; Known Issues:
;; - [ ] No method to automatically remove tags/links.
;;
;; Tasks:
;; - [ ] TODO: Add org-roam-tags-sort-fn.
;; - [ ] TODO: Extend to support more tagging uses.
;;
;;; Code:

(require 's)
(require 'org-roam-node)

(defcustom org-roam-tags-add-person-to-task t
  "Add a `@PersonName' tag to a task when linking their org-roam node.

Used by `org-roam-tags-node-insert-update' to automatically
add a `person' tag of the form `@PersonName' when linking to their
node under a task heading. The tag format is automatically generated
from the #+title of the linked node if it has the `person' tag."
  :type `boolean
  :group 'org-roam)

(defun org-roam-tags-node-insert-h (id link-description)
  "Update parent node tags when an Org-roam node is inserted as an Org link.

Recieves ID and LINK-DESCRIPTION from `org-roam-post-node-insert-hook' to
update the parent node tags according to the value of yet to be implemented
options.

Currently only supports adding a `person' tag of the form `@PersonName'
generated from a the title of a node that has the `person' tag."
  (ignore link-description) ;; Make the byte-compiler happy.
  (let* ((node (org-roam-node-from-id id))
         (filetags (org-roam-node-tags node))
         (title (concat "@" (s-replace " " "" (org-roam-node-title node)))))
    (when org-roam-tags-add-person-to-task
      (when (seq-contains-p filetags "person")
        (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (when (eq 'todo (org-element-property
                             :todo-type
                             (org-element-at-point)))
              (org-set-tags (seq-uniq (cons title (org-get-tags nil t)))))))))))

(add-hook 'org-roam-post-node-insert-hook #'org-roam-tags-node-insert-h)

(provide 'org-roam-tags)
;;; org-roam-tags.el ends here
