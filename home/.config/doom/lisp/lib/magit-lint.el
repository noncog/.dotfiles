;;; magit-lint.el --- Add Angular and Conventional commit linting to Magit -*- lexical-binding: t; -*-
;;
;; Author: noncog
;; Maintainer: noncog
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/lib/magit-lint.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides a custom Git commit linter for Magit based on Angular and Conventional commits.
;;
;; The code is based on and highly modified from:
;; - https://www.adventuresinwhy.com/post/commit-message-linting
;;
;; This code is an extension of `git-commit-style-convention-checks' from Magit.
;;
;; Requires two files to define a list of words to check against the commit message summary:
;; - commit-types
;; - imperative-verbs
;;
;;; Code:

(require 'magit)

;; Set commit summary max length.
;; This length seems to be a popular value compatible with conventional commits.
(setq git-commit-summary-max-length 50)

;; Set commit body max length.
;; Magit/Doom use fill column and git-commit-mode-hook to wrap the body length.
;; (setq-hook! 'git-commit-mode-hook fill-column 72)

(defvar magit-lint-commit-types-file "~/.config/git/commit/commit-types"
  "File containing a list of (conventional) commit types checked by magit-lint.

Used in `magit-lint-check-style-conventions' to ensure the commit uses a
valid commit type keyword.")

(defvar magit-lint-imperative-verbs-file "~/.config/git/commit/imperative-verbs"
  "File containing a list of imperative commit verbs checked by magit-lint.

Used in `magit-lint-check-style-conventions' to ensure the commit uses an
imperative verb. If the verb is not in the list, asks to add it.")

(defun magit-lint-get-commit-types ()
  "Return a list of commit types from `magit-lint-commit-types-file'.

Used by `magit-lint-check-style-conventions'."
  (let ((file-path magit-lint-commit-types-file))
    (with-temp-buffer
      (insert-file-contents file-path)
      (split-string (buffer-string) "\n" t))))

(defun magit-lint-get-imperative-verbs ()
  "Return a list of imperative verbs from `magit-lint-imperative-verbs-file'.

Used by `magit-lint-check-style-conventions'."
  (let ((file-path magit-lint-imperative-verbs-file))
    (with-temp-buffer
      (insert-file-contents file-path)
      (split-string (buffer-string) "\n" t))))

(defcustom magit-lint-style-convention-checks '(summary-has-type
                                                summary-type-lowercase
                                                summary-has-separator
                                                summary-scope-lowercase
                                                summary-title-starts-with-lowercase
                                                summary-title-uses-imperative-verb
                                                summary-title-not-end-in-punctuation)
  "List of checks performed by `magit-lint-check-style-conventions'.

Valid members are `summary-has-type',  `summary-type-lowercase',
`summary-has-separator', `summary-scope-lowercase',
`summary-title-starts-with-lowercase', `summary-title-uses-imperative-verb', and
`summary-title-not-end-in-punctuation'."
  :options '(summary-has-type
             summarty-type-lowercase
             summary-has-separator
             summary-scope-lowercase
             summary-title-starts-with-lowercase
             summary-title-uses-imperative-verb
             summary-title-not-end-in-punctuation)
  :type '(list :convert-widget custom-hook-convert-widget)
  :group 'magit-lint)

(defun magit-lint-check-style-conventions (&optional force)
  "Check for violations of conventional commit style conventions.

For each violation ask the user if she wants to proceed anyway.
Option `magit-lint-check-style-conventions' controls which
conventions are checked.

The function is used by `git-commit-finish-query-function' and is
provided one argument: FORCE. If non-nil, that indicates that the
user used a prefix argument to force the finishing of the commit
session despite issues.

This package relies on Magit to check `overlong-summary-line' and
`non-empty-second-line' in `git-commit-style-convention-checks' to
avoid redundant redefinition of those checks. Ensure these checks
are also set."
  (or force
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (git-commit-summary-regexp) nil t)
        (let* ((summary (match-string 1))
               (commit-type (substring summary 0 (string-match-p "[()!:]" summary)))
               (commit-separator-check (string-match-p "^.*[^[:blank:]]:" summary))
               (commit-separator-location (string-match-p ":" summary))
               (commit-scope (if (string-match-p "(.*)[!:]" summary)
                                 (substring summary (1+ (string-match-p "(" summary)) (string-match-p ")!?:?" summary)) nil))
               (commit-title (if commit-separator-check (substring summary (+ commit-separator-location 2)) "undetectable-title"))
               (lowercase-title-first-word (downcase (substring commit-title 0 (string-match-p "[[:blank:]]" commit-title)))))
          (and (or (not (memq 'summary-type-lowercase magit-lint-style-convention-checks))
                   (let ((case-fold-search nil)) (string-match-p "^[[:lower:]]*$" commit-type))
                   (y-or-n-p "Commit type is not lowercase. Commit anyway?"))
               (or (not (memq 'summary-has-type magit-lint-style-convention-checks))
                   (car (member commit-type (magit-lint-get-commit-types)))
                   (when (y-or-n-p "Commit type is invalid. Commit anyway?")
                     (when (y-or-n-p (format "Add `%s' to list of commit types?" commit-type))
                       (with-temp-buffer
                         (insert commit-type)
                         (insert "\n")
                         (write-region (point-min) (point-max) magit-lint-commit-types-file t))) t))
               (or (not (memq 'summary-has-separator magit-lint-style-convention-checks))
                   (not (null commit-separator-check))
                   (y-or-n-p "Commit title has invalid separator. Commit anyway?"))
               (or (not (memq 'summary-scope-lowercase magit-lint-style-convention-checks))
                   (when (or (not commit-scope) (string-empty-p commit-scope)) (y-or-n-p "Commit scope is empty. Commit anyway?"))
                   (when commit-scope (if (let ((case-fold-search nil)) (string-match-p "^[[:lower:]]*$" commit-scope)) t
                                        (y-or-n-p "Commit scope invalid. Only lowercase letters allowed. Commit anyway?"))))
               (or (not (memq 'summary-title-starts-with-lowercase magit-lint-style-convention-checks))
                   (if (not commit-separator-check) (y-or-n-p "Title undetectable. Commit anyway?") (let ((case-fold-search nil))
                                                                                                      (string-match-p "^[[:lower:]]" commit-title)))
                   (y-or-n-p "Commit title does not start with lowercase letter. Commit anyway?"))
               (or (not (memq 'summary-title-uses-imperative-verb magit-lint-style-convention-checks))
                   (if (not commit-separator-check) (y-or-n-p "Imperative verb undetectable. Commit anyway?")
                     (car (member lowercase-title-first-word (magit-lint-get-imperative-verbs))))
                   (when (y-or-n-p "Commit title should use imperative verb. Does it?")
                     (when (y-or-n-p (format "Add `%s' to list of commit types?" lowercase-title-first-word))
                       (with-temp-buffer
                         (insert lowercase-title-first-word)
                         (insert "\n")
                         (write-region (point-min) (point-max) magit-lint-imperative-verbs-file t))) t))
               (or (not (memq 'summary-title-not-end-in-punctuation magit-lint-style-convention-checks))
                   (not (string-match-p "[\\.!\\?;,:]$" commit-title))
                   (y-or-n-p "Commit title ends with punctuation. Commit anyway?")))))))

(add-to-list 'git-commit-finish-query-functions #'magit-lint-check-style-conventions)

;; TODO: Verify this works and if there is a better way to use after-save-hook in contexts.
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(provide 'magit-lint)
;;; magit-lint.el ends here
