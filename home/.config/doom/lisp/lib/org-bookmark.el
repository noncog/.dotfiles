;;; org-bookmark.el --- Functions for maintaining bookmarks in Org -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/home/.config/doom/lisp/lib/org-bookmark.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Temporary notes:
;; - Functions for automatically putting types of links in different locations.
;; - Functions for formatting link titles.
;; - Eventually functions for archiving different types of media from links.
;;
;;; Code:

(defvar org-bookmark--format-hash (make-hash-table :test #'equal)
  "A hash of (DOMAIN TITLE-FORMATTER) to be applied to link titles.")

(defun org-bookmark-add-link-formatter (domain title-transformer)
  "Adds a DOMAIN and associated TITLE-TRANSFORMER to the org-bookmark--format-hash"
  (puthash domain title-transformer org-bookmark--format-hash))

(defun org-bookmark-replace-in-link-title (regex replacement)
  "Replace REGEX of a link's title with REPLACEMENT."
  (lambda (title)
    (replace-regexp-in-string regex replacement title)))

(defun org-bookmark-format-stored-link ()
  "Return a pretty-printed top of `org-stored-links'."
  (let* ((link (caar org-stored-links))
         (title (cl-cadar org-stored-links))
         (parsed-link (url-generic-parse-url link))
         (domain (concat (url-type parsed-link) "://" (url-host parsed-link)))
         (formatter (gethash domain org-bookmark--format-hash)))
    (progn (unless org-link-keep-stored-after-insertion (pop org-stored-links))
           (org-link-make-string link (if formatter (funcall formatter title) title)))))

;; (defun org-bookmark-retrieve-title (url)
;;   (when (member (url-type (url-generic-parse-url url))
;;                 '("http" "https"))
;;     (with-temp-buffer
;;       (url-insert-file-contents url)
;;       (goto-char (point-min))
;;       (when (search-forward "<title>" nil t)
;;         (let ((start (point)))
;;           (when (search-forward "</title>" nil t)
;;             (buffer-substring start (match-beginning 0))))))))

(defun org-bookmark-retrieve-title (url)
  (when (member (url-type (url-generic-parse-url url))
                '("http" "https"))
    ;; (with-temp-buffer
    ;;   (url-insert-file-contents url)
    ;;   (goto-char (point-min))
    ;;   (when (search-forward "<title>" nil t)
    ;;     (let ((start (point)))
    ;;       (when (search-forward "</title>" nil t)
    ;;         (buffer-substring start (match-beginning 0))))))
    (let ((response-buffer (url-retrieve-synchronously url t t)))
      (when response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (when (search-forward "<title>" nil t)
            (let ((start (point)))
              (when (search-forward "</title>" nil t)
                (buffer-substring start (match-beginning 0))))))))
    ))

(defun org-bookmark--format-link (url title)
  (if title (format "[[%s][%s]]" url title)
    (format "[[%s]]" url)))

(defun org-bookmark-format-link ()
  (if org-stored-links
      (org-bookmark-format-stored-link)
    (let* ((url (if (caar org-stored-links)
                    (org-bookmark-format-stored-link)
                  (string-trim (substring-no-properties (current-kill 0))))))
      (org-bookmark--format-link url (org-bookmark-retrieve-title url)))))

(defvar org-bookmark-location-handlers nil
  "List of bookmark location handlers by priority.

Each item is a function of zero arguments that opens an
appropiriate file/line and returns non-nil on match.")

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
    (beginning-of-line 0))
  t)

(defun org-bookmark-handler-match-url (regex file heading)
  "For link matching REGEX select FILE at HEADING."
  (if
      (caar org-stored-links)
      (when (string-match regex (caar org-stored-links))
        (org-bookmark-handler-file-heading file heading))
    (when (string-match regex (string-trim (substring-no-properties
                                            (current-kill 0))))
      (org-bookmark-handler-file-heading file heading))))

(defvar org-bookmark-append-style 'beginning)

(defun org-bookmark-append-style-valid-p ()
  (if (and (symbolp org-bookmark-append-style)
           (member org-bookmark-append-style (list 'nil 'beginning 'end)))
      t nil))

;; TODO: These need some work, steal from org capture insertion commands.
(defun org-bookmark-append-content (initial-content)
  (unless (org-bookmark-append-style-valid-p)
    (error "Invalid org-bookmark appending style '%s'" org-bookmark-append-style))
  (when (eq org-bookmark-append-style 'beginning)
    (progn
      (org-end-of-meta-data t)
      (insert initial-content "\n")))
  (when (eq org-bookmark-append-style 'end)
    (progn
      (outline-next-heading)
      (beginning-of-line 0)
      (insert initial-content "\n"))))

(defun org-bookmark-find-rg ()
  (unless (executable-find "rg")
    (error "Org-bookmark could not find rg in your path!")))

(defun org-bookmark-find-link ()
  (let* ((link (if (caar org-stored-links)
                   (caar org-stored-links)
                 (string-trim (substring-no-properties (current-kill 0)))))
         (old-links
          (split-string (shell-command-to-string
                         (format "rg -Fn '[%s]' %s" link org-directory))
                        "\n" t)))
    (if old-links
        (let ((old-link (car old-links)))
          (if (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" old-link)
              (let ((file (match-string 1 old-link))
                    (line (string-to-number (match-string 2 old-link))))
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line))
                (if (string-match-p "https://www.youtube.com" link)
                    (not (y-or-n-p "old link: redo?"))
                  (message "%d old link(s)" (length old-links)) t))
            (error "Could not match %s" old-link)))
      nil)))

(defun org-bookmark-capture-kill ()
  (let ((initial-content (plist-get org-store-link-plist :initial)))
    (when (and org-bookmark-append-style initial-content (not (string-empty-p initial-content)))
      (org-bookmark-append-content initial-content))
    (unless org-link-keep-stored-after-insertion (pop org-stored-links))
    (org-capture-kill)))

(defun org-bookmark-handle-location ()
  (let ((hands org-bookmark-location-handlers)
        hand)
    (while (and (setq hand (pop hands))
                (null
                 (apply (car hand) (cdr hand)))))))

(defun org-bookmark-location ()
  "Selects a location to store the current bookmark link."
  (if (org-bookmark-find-link)
      (org-bookmark-capture-kill)
    (org-bookmark-handle-location)))

(org-bookmark-add-link-formatter "https://github.com" (org-bookmark-replace-in-link-title ":[ ].*$?" ""))

(setq org-bookmark-location-handlers '((org-bookmark-handler-file-heading org-inbox-file "Bookmarks")))

(defun org-bookmark-symbol ()
  (let ((symbol (symbol-name (helpful--read-symbol
                              "Symbol: " (helpful--symbol-at-point)
                              #'helpful--bound-p))))
    (my/org-store-help-link symbol)
    (apply #'org-bookmark-handler-file-heading '("~/Documents/notes/area/20231006T042354--emacs.org" "Useful Symbols and Keybinds"))))

;; (use-package! orca
;;   :after org-protocol
  ;;; Variables
  ;;; (setq orca-handler-list
  ;;;       '((orca-handler-match-url
  ;;;          "https://www.reddit.com"         "/home/noncog/Documents/notes/wiki/emacs.org"
  ;;;          "\\ * Reddit")))
                                        ;(setq orca-handler-list
                                        ;      '((orca-handler-match-url
                                        ;         "https://www.reddit.com/emacs/"
                                        ;         "~/Documents/notes/wiki/emacs.org"
                                        ;         "Reddit")
                                        ;        (orca-handler-match-url
                                        ;         "https://emacs.stackexchange.com/"
                                        ;         "~/Documents/notes/wiki/emacs.org"
                                        ;         "\\* Questions")
                                        ;        (orca-handler-current-buffer
                                        ;         "\\* Tasks")))
;; )


(provide 'org-bookmark)
;;; org-bookmark.el ends here
