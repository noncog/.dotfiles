;;; org-bookmark.el --- Functions for maintaining bookmarks in Org -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/org-bookmark.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This code is a rewrite and extension of of orca.el by abo-abo.
;; https://github.com/abo-abo/orca/blob/master/orca.el
;;
;; Development Notes:
;; TODO: Add more handlers.
;; TODO: Define the bookmarklet:
;; javascript:location.href='org-protocol://capture?template=b&url=%27 + encodeURIComponent(location.href) + %27&title=%27 + encodeURIComponent(document.title) + %27&body=%27 + encodeURIComponent(window.getSelection())
;; TODO: Advanced bookmarklet capabilities.
;; TODO: Build deduplication workflow.
;; TODO: Build out archiving.
;;
;;; Code:
(require 'org-url)
(require 'org-capture)

(defvar org-bookmark-search-directory org-directory
  "List of directories to search for bookmarks.")

(defvar org-bookmark-link-format "[%s]"
  "Format a link should have when stored as a bookmark.")

(defvar org-bookmark-location-handlers nil
  "List of handlers by priority.

Each item is a function of zero arguments that opens an
appropiriate file and returns non-nil on match.")

(defun org-bookmark-link-p (link)
  "Return non-nil if LINK is a valid link."
  (org-url-p link))

;; TODO: Add multiple directory searching.
;; TODO: Check valid directory.
(defun org-bookmark-find-link (link)
  "Search for LINK in `org-bookmark-search-directory'."
  (when (org-bookmark-link-p link)
  (split-string
   (shell-command-to-string
    (format (concat "rg -Fn '" org-bookmark-link-format "' %s")
            link org-bookmark-search-directory)) "\n" t)))

(defun org-bookmark-handle-location ()
  "Find a location for a link dynamically."
  (message "in handle location")
  (let ((hands org-bookmark-location-handlers)
        hand)
    (while (and (setq hand (pop hands))
                (null
                 (apply (car hand) (cdr hand)))))))

(defun org-bookmark-handler-file-heading (file heading)
  "Open or create the FILE and HEADING to insert a bookmark at."
  (set-buffer (org-capture-target-buffer file))
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode"
             (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote heading)) nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " heading "\n")
    (beginning-of-line 0)) t)

(defun org-bookmark-capture ()
  "Dynamically select a location to capture a bookmark."
  (when-let* ((old-links (org-bookmark-find-link (caar org-stored-links)))
              (count (length old-links)))
    (if (and (eq 1 (length old-links))
             (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" (car old-links)))
        (let* ((old-link (car old-links))
              (file (match-string 1 old-link))
              (line (string-to-number (match-string 2 old-link))))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line))
          ;; TODO: Does not support inserting new notes or anything.
          (user-error "Opened link heading"))
      ;; TODO: Do more advanced handling of situation.
      (user-error "%d old link(s)" (length old-links))))
  (org-bookmark-handle-location))

(defun org-bookmark-format-link ()
  "Return a pretty-printed top of `org-stored-links'."
  (let* ((link (caar org-stored-links))
         (title (cl-cadar org-stored-links)))
    (org-url-format-link link title)))

(provide 'org-bookmark)
;;; org-bookmark.el ends here
