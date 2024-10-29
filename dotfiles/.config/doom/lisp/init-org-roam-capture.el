;;; init-org-roam-capture.el --- Org Roam capture templates compatible with Denote -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jake Turner
;;
;; Author: Jake Turner <john@doe.com>
;; Maintainer: Jake Turner <john@doe.com>
;; Created: October 29, 2024
;; Modified: October 29, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/noncog/init-org-roam-capture
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! org-roam-capture
;; Org Roam Capture extends the built-in Org Capture functionality with Org Roam
;; specific template expansions, integration with node creation, and Org Roam
;; Reference captures using Org Roam Ref Protocol!
  :config
  (setq org-roam-capture-templates
        '(("n" "node" plain "%?"
           :target (file+head "node/%<%Y%m%dT%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

;; TODO: Make sure this is auto-loaded.
(use-package! org-roam-protocol
;; Org Roam Protocol is an extension to org-roam for capturing content from
;; external applications such as web browsers via org-protocol. It extends
;; org-protocol with two more protocols: roam-node and roam-ref.
  :config
  ;; (setq org-roam-protocol-store-links t)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :target (file+head "resource/%<%Y%m%dT%H%M%S>--${slug}.org" "#+title: ${title}\n\n${body}")
           :unnarrowed t))))

;; TODO: Change the category property and file name to be cat: log and filename: simple date.
(use-package! org-roam-dailies
  ;; Org Roam Dailies is an extension to org-roam to provide journaling
  ;; capabilities akin to org-journal but better integrated.
  :init
  (setq org-roam-dailies-directory "log/")
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y%m%dT%H%M%S>--log.org" "#+title: Log\n")))))

(provide 'init-org-roam-capture)
;;; init-org-roam-capture.el ends here
