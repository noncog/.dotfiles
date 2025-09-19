;;; org-roam-filetags.el --- Automatic org-roam filetag updates -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/org-roam-filetags.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides functions for updating org-roam node filetags and file names based on conditions.
;;  - Used to tag a node as 'agenda' or a person's name with '@PersonName' when tagged as a 'person' file.
;;  - Provides headline tagging when inserting a reference to a person, adding their '@PersonName' tag.
;;  - Provides integration for renaming node files using Denote's file naming scheme.
;;  - Provides automatic sorting of filetags and ability to set custom sorting function.
;;  - Allows excluding files from being renamed.
;;
;; Sources:
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
;; https://github.com/hieutkt/dotfiles/tree/main/emacs/.doom.d
;; https://github.com/d12frosted/environment/blob/f48e6d91be48b7e1d6126cde6f39c39a92a288ab/emacs/lisp/lib-vulpea-agenda.el
;; https://github.com/d12frosted/environment/blob/f48e6d91be48b7e1d6126cde6f39c39a92a288ab/emacs/lisp/lib-vulpea.el
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/#build-your-org-agenda-from-org-roam-notes
;;
;;; Code:

;; TODO: Fix case where filetags is added with space after only #+title and no other keywords.
;; TODO: Add moving file with sets of tags to locations.
;; TODO: Add inheriting parent directory name tag.
;; TODO: Consider adding ability to choose different tag names for each.
;; TODO: Keep org tag alist up-to-date with org-roam tags.
;; TODO: Consider person tag having ability to get from hidden property somehow the preferred name or alias to use for them.
;; TODO: Make variable to turn on and off sorting.
;; TODO: Consider moving to org-roam-tag.el
;; TODO: Make variables defcustom.
;; TODO: Handle inheritance for person tag.

(require 's)
(require 'org-roam-utils)
;;(require 'org-roam-keyword)
(require 'org-roam-agenda)
(require 'org-file)

(add-to-list 'org-tags-exclude-from-inheritance "person")

(defvar org-roam-filetags-update-person nil
  "Non-nil adds filetag of node's title as a person's name, like `@PersonName'.

Primarily used when linking a person's node into a task, their tag is added
to the task heading.")

(defvar org-roam-filetags-sort-fn #'string<
  "Function used to sort org-roam filetags. When set to nil, disables sorting.
Expects a hash quoted name of function name that accepts two arguments.
See `sort' for documentation on this function's expected behavior.")

(defun org-roam-filetags-sort (string1 string2)
  "Function used by `org-roam-filetags-update' to call `org-roam-filetags-sort-fn'.

Accepts STRING1 and STRING2 and"
  (and org-roam-filetags-sort-fn (funcall org-roam-filetags-sort-fn string1 string2)))

(defun org-roam-filetags-update (title filetags)
  "Add or remove FILETAGS to the current node based on conditions.

Used by `org-roam-file-pre-save-h' to enable automatic update
to the FILETAGS keyword using the TITLE based on user settings of
org-roam-filetags-update-* variables."
  (let* ((title-as-tag (concat "@" (s-replace " " "" title)))
         (tags-list (if filetags
                        (split-string
                         (replace-regexp-in-string
                          "[[:blank:]]" "" filetags) ":" 'omit-nulls) nil))
         (new-tags tags-list)
         (add-tags (list))
         (remove-tags (list)))
    ;; Ensure the agenda tag is set when appropriate.
    ;; See org-roam-agenda-file-p for conditions.
    (if (org-file-agenda-p)
        (cl-pushnew "agenda" add-tags :test #'string=)
      (cl-pushnew "agenda" remove-tags :test #'string=))
    ;; Ensure that @PersonName tag is set when node is tagged as a person.
    (when org-roam-filetags-update-person
      (if (seq-contains-p tags-list "person")
          (cl-pushnew title-as-tag add-tags :test #'string=)
        (cl-pushnew title-as-tag remove-tags :test #'string=)))
    ;; Update the list of filetags.
    (setq new-tags (seq-union new-tags add-tags))
    (setq new-tags (seq-difference new-tags remove-tags))
    (when (not (seq-set-equal-p tags-list new-tags))
      (setq new-tags (seq-uniq new-tags)))
    ;; Apply latest filetags to node.
    (org-roam-set-keyword
     "filetags" (org-make-tag-string
                 (if org-roam-filetags-sort-fn
                     (sort new-tags #'org-roam-filetags-sort) new-tags)))))

(provide 'org-roam-filetags)
;;; org-roam-filetags.el ends here
