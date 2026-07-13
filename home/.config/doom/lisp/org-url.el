;;; org-url.el --- Functions for building Org links from urls -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/org-url.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functions to format Org links from urls.
;;
;; Development Notes:
;; NOTE: url-handler-regexp vs. url-p
;; TODO: Create format link at point function.
;;; Code:

(require 'ol)
(require 'url-parse)

(defvar org-url-title-formatters (make-hash-table :test #'equal)
  "Hash table mapping a bookmark DOMAIN to a TITLE-TRANSFORMER applied to it.")

(defun org-url-add-title-formatter (domain title-transformer)
  "Add TITLE-TRANSFORMER function for DOMAIN to org-bookmark-formatters."
  (puthash domain title-transformer org-url-title-formatters))

(defun org-url-replace-in-title (regex replacement)
  "Replace REGEX of a link's title with REPLACEMENT."
  (lambda (title)
    (replace-regexp-in-string regex replacement title)))

(defun org-url-retrieve-title (url)
  "Return the page title for a given URL."
  (with-temp-buffer
    (when (ignore-errors (url-insert-file-contents url))
      (goto-char (point-min))
      (when-let* ((start (search-forward "<title>" nil t))
                  (end (search-forward "</title>" nil t)))
        (replace-regexp-in-string
         "&\\(copy\\|amp\\|lt\\|gt\\);?"
         (lambda (m)
           (pcase (match-string 1 m)
             ("copy" "(c)")
             ("amp"  "&")
             ("lt"   "<")
             ("gt"   ">")))
        (buffer-substring start (match-beginning 0)))))))

;; TODO: Review logic of optional title.
(defun org-url-format-link (link &optional title)
  "Return a formatted org LINK string with optional explicit TITLE."
  (when-let* ((plink (url-generic-parse-url link))
              (title (or title (org-url-retrieve-title link)))
              (domain (concat (url-type plink) "://" (url-host plink))))
    (org-link-make-string link (if-let ((formatter (gethash domain org-url-title-formatters)))
                                  (funcall formatter title) title))))

(provide 'org-url)
;;; org-url.el ends here
