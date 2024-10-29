;;; init-project.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-project
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! projectile
  ;; A project interaction library for Emacs. It provides easy project management and navigation.
  :defer t
  :init
  ;; Bind a key to list projects which have uncommited changes.
  (map! :map project-prefix-map
        :leader
        :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects)
  :config
  (setq projectile-project-search-path '("~/Code" "~/Projects" ("~/.dotfiles" . 0))
        ;; Disable auto discover due to long-standing issue where ignored directories aren't upheld.
        projectile-auto-discover nil
        projectile-track-known-projects-automatically nil))


(provide 'init-project)
;;; init-project.el ends here
