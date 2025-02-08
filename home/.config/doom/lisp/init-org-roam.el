;;; init-org-roam.el --- Configure Org-Roam -*- lexical-binding: t; -*-
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/noncog/.dotfiles/dotfiles/.config/doom/lisp/init-org-roam.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; My configuration of the Org-Roam package.
;;
;;; Code:
;; TODO: See: https://www.orgroam.com/manual.html#Customizing-Node-Caching
;; TODO: Consider adding date property.
;; TODO: Consider n r t to make a node a typed node. instead of a node.
;; TODO: Consider developing unbind lib for removing all bad/deprecated functions.
;; TODO: Develop private node hiding method.
;; TODO: org-refile targets integration
;; TODO: Add extended functionality for project tags.
;; TODO: Develop i3/org-roam-ui and backlinks buffer in new frame/sidebar functionality.
;; TODO: Add vulepa integration.

(use-package! org-roam
  ;; Org Roam is a plain-text knowledge management system.
  ;; It provides a solution for non-hierarchical note-taking and linking.
  :defer t
  :init
  (setq org-roam-directory org-directory
        org-roam-db-location (expand-file-name ".org.db" org-roam-directory))

        ;; TODO: Add when less than emacs 29
        ;; org-roam-database-connector 'emacsql-sqlite)
  :config
  (require 'org-roam-file) ;; Integrates org-roam with org-agenda and denote.
  (require 'org-roam-tags) ;; Automatic tag insertion after node insertion.
  (add-to-list 'org-tags-exclude-from-inheritance "agenda")
  (add-to-list 'org-tags-exclude-from-inheritance "person")
  (require 'init-org-roam-capture) ;; Integrate org-roam-capture.
  (when (modulep! :ui modeline)    ;; Integrate Doom-Modeline with the file naming
    (require 'org-roam-modeline)   ;; scheme of Denote and Org Roam.
    (setq doom-modeline-buffer-file-name-function #'org-roam-modeline-process-buffer-file-name
          doom-modeline-buffer-file-truename-function #'org-roam-modeline-process-buffer-file-name)))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
