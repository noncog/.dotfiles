;;; +project.el --- My personal project and version control configuration -*- lexical-binding: t; -*-

(use-package! projectile
  :defer t
  :config
  (setq projectile-auto-discover nil
        projectile-track-known-projects-automatically nil
        projectile-project-search-path
        '(("~/.dotfiles" . 0)
          ("~/dev/projects" . 1)   ; My projects.
          ("~/dev/source" . 1)))   ; Other's code.
  (map! :map project-prefix-map
        :leader :desc "List dirty projects"
        "p l" #'projectile-browse-dirty-projects))

(use-package! magit
  :defer t
  :config
  (setq magit-repository-directories
        '(("~/.dotfiles" . 0)
          ("~/dev/projects" . 1)
          ("~/dev/source" . 1)))
  ;; Load my custom commit linter.
  (require 'magit-lint))
