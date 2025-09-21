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
;; A library containing a set of functions to modify and update file-level properties
;; of org documents.
;;
;; Reasons for existence:
;; - Org doesn't support explicit file-level tag, property, or keyword modification.
;; - Org-Roam doesn't work correctly for the above.
;; - Denote does not cleanly abstract it's core and org extensions.
;;
;; Features:
;; - Org file-level equivalent functions for tags, properties, and drawers.
;; - Automatic file name synced with file front-matter.
;; - Automatic filetags updated on save.
;; - Automatic customizable filetag sorting.
;;
;;; Tasks:
;;
;; - [ ] Front matter keyword sorting.
;; - [ ] function: org-file-set-property
;; - [ ] org-agenda category functions.
;;       - means of building the list, cache, roam, db, always include, ripgrep, etc.
;;       - if org-file or fref, denote, etc. should be the lib for getting files and category.
;;       - settings for updating database cache on user cache settings.
;;         - if cache set to not work on save, manual update, perhaps async update.
;; - [ ] Install info for org-file-update-on-save-enable.
;; - [ ] Complex file front-matter sync handler.
;;       - Mismatched front-matter.
;;       - Filename vs buffer as source of truth.
;;       - Parent directory based auto tagging.
;;       - Tag based refiling/moving.
;;       - Special tags.
;;       - file-level property auto updating.
;; - [ ] Integrate org-roam-extract-subtree to support ID and Filetags.
;; - [ ] Support multiple agenda tags.
;;
;;; Code:

(require 'org)
(require 'org-element)

;;; File Metadata

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

(defun org-file-property-p (property &optional value)
  "Return non-nil if org buffer has file-level PROPERTY.

When optional argument VALUE is non-nil, only consider an entry
if it contains PROPERTY set to this value. If PROPERTY should be
explicitly set to nil, use string \"nil\" for VALUE."
  (org-with-point-at 1
    (let ((has-prop (org-find-property property value)))
      (if (and has-prop (progn (goto-char has-prop) (org-before-first-heading-p)))
          (if value t (org-entry-get has-prop property nil t)) nil))))

;; TODO: Implement my own form of completing read for this.
;; completion-ignore-case
;; (defun org-file-set-property (property value)
;;   "Set org buffer file-level PROPERTY to VALUE.

;; If VALUE is a blank string, removes the property instead."
;;   (interactive (list nil nil))
;;   (org-with-wide-buffer
;;    (goto-char (point-min))
;;    (let* ((property (or property (org-read-property-name)))
;;           (value (or value (org-read-property-value property (point) ""))))
;;      (if (string-blank-p value)
;;          (org-delete-property property)
;;        (funcall-interactively #'org-set-property property value)
;;        )
;;      )

;;    )
;;   )

(defun org-file-get-keyword (name)
  "Return value of first occurence of keyword NAME in file."
  ;; NOTE: Consider using implementation from org-roam--get-keyword as it's faster.
  (org-macro--find-keyword-value name))

(defun org-file-set-keyword (key value)
  "Set org file-level keyword KEY to VALUE.

If the property is already set, the value is replaced.
If VALUE string is blank, removes the keyword line."
  (org-with-point-at 1
    (let ((case-fold-search t))
      ;; TODO: See if can wrap re-search-forward, limit depth, or standardize regex.
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          ;; If keyword line found, update it.
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) t nil nil 1))
        ;; Otherwise, add keyword line at end of meta-data.
        (org-file-end-of-meta-data)
        (when (and (not (or (bolp) (bobp))) (eolp)) (insert "\n"))
        (insert "#+" key ": " value "\n")))))

;;; Agenda

(defcustom org-file-tag-agenda nil
  "A file tag that should be added to a file to denote it as an agenda file.

A nil value disables automatically tagging the file as an agenda file."
  :group 'org-agenda
  :type 'string :tag "Tag")

(defcustom org-file-agenda-keywords nil
  "List of TODO keywords a heading can have to denote the file as an agenda file."
  :group 'org-agenda
  :type '(repeat (string :tag "Keyword")))

(defcustom org-file-agenda-tags nil
  "List of tags a heading can have to denote the file as an agenda file."
  :group 'org-agenda
  :type '(repeat (string :tag "Tag")))

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

;;; File Updates

(defun org-file-rename-p ()
  "Return t if current org file is not marked as exempt from renaming.

If file property `RENAME' is nil, return nil, otherwise return t."
  ;; TODO: Add more checks, org-directory, etc.
  (not (org-file-property-p "RENAME" "nil")))

(defcustom org-file-rename-fn nil
  "Function to use to sort org filetags. If nil, disables sorting.
Expects the hash quoted name of a function that accepts two arguments.
See `sort' for documentation on this function's expected behavior."
  :group 'org-file
  :type 'function)

(defcustom org-file-update-name t
  "Automatically update the org file name on save."
  :group 'org-file
  :type 'boolean)

(defcustom org-file-update-tags t
  "Automatically update the org filetags keyword on save."
  :group 'org-file
  :type 'boolean)

(defcustom org-file-tags-sort-fn #'string<
  "Function to use to sort org filetags. If nil, disables sorting.
Expects the hash quoted name of a function that accepts two arguments.
See `sort' for documentation on this function's expected behavior."
  :group 'org-file
  :type 'function)

(defcustom org-file-update-agenda t
  "Automatically update `org-agenda-files' variable on save."
  :group 'org-file
  :type 'boolean)

(defcustom org-file-update-agenda-fn nil
  "Function to use to update org-agenda-files."
  :group 'org-file
  :type 'function)

(defun org-file--tags-update (filetags)
  "Update the FILETAGS keyword in the current org file conditionally.

Used by `org-file-pre-save-h' to enable automatic updates."
  (let* ((tags-list (when filetags
                      (split-string filetags ":" t "[ \f\t\n\r\v]+")))
         (new-tags tags-list)
         (add-tags (list))
         (remove-tags (list)))
    ;; Ensure 'agenda' tag.
    (when org-file-tag-agenda
      (if (org-file-agenda-p)
          (cl-pushnew "agenda" add-tags :test #'string=)
        (cl-pushnew "agenda" remove-tags :test #'string=)))
    ;; Calculate the list of filetags.
    (setq new-tags (seq-difference
                    (seq-union new-tags add-tags) remove-tags))
    ;; If the tags have changed, update the list, possibly sorting.
    (when (not (seq-set-equal-p tags-list new-tags))
      (setq new-tags (seq-uniq new-tags))
      (when (functionp org-file-tags-sort-fn)
        (setq new-tags (sort new-tags org-file-tags-sort-fn)))
      (org-file-set-keyword
       "filetags" (org-make-tag-string new-tags)))))

(defun org-file--name-update (title filetags id)
  "Update the current org file's name on save.

Used by `org-file-pre-save-h' to update the filename upon saving
the buffer using the TITLE, FILETAGS, and ID."
  ;; Rename file conditionally.
  (when (and (org-file-rename-p) (functionp org-file-rename-fn))
    (funcall org-file-rename-fn (buffer-file-name) title filetags id)))

(defun org-file-pre-save-h ()
  "Update org file attributes on save.

This function is added to `before-save-hook' by `org-file-update-on-save-enable'
to automatically run a set of updates for the current org file."
  (org-with-wide-buffer
   (let ((title (org-file-get-keyword "title"))
         (filetags (org-file-get-keyword "filetags"))
         (id (progn (goto-char (point-min)) (org-id-get))))
     ;; Update front matter. (file properties, keywords, etc.)
     (when org-file-update-tags
       (org-file--tags-update filetags))
     ;; Update file name.
     (when org-file-update-name
       (org-file--name-update title filetags id))
     ;; Update agenda files.
     (when (and org-file-update-agenda
                (functionp org-file-update-agenda-fn))
       (funcall org-file-update-agenda-fn)))))

(defun org-file-update-on-save-enable ()
  "Enable checking and updating an org document on saving."
  (add-hook 'before-save-hook 'org-file-pre-save-h t t))

(provide 'org-file)
;;; org-file.el ends here
