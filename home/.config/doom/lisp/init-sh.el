;;; init-sh.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-sh
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! sh-script
  ;; A built-in library that provides a major mode for editing shell scripts.
  :defer t
  :init
  (setq-default shell-file-name (executable-find "bash")
                sh-shell-file shell-file-name)
  :config
  ;; Set formatting rules.
  ;; Doom and Apheleia provide basic arguments, so I add my full set.
  (set-formatter! 'shfmt
                  '("shfmt" "-filename" filepath "-ci" "-bn" "-sr" "-ln"
                    (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))
                    (when apheleia-formatters-respect-indent-level
                      (list "-i"
                            (number-to-string
                             (cond
                              (indent-tabs-mode 0)
                              ((boundp 'sh-basic-offset)
                               sh-basic-offset)
                              (t 4))))))
                  :modes '(sh-mode)))

(provide 'init-sh)
;;; init-sh.el ends here
