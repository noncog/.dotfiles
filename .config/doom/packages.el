;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! beacon
  :recipe (:host github :repo "Malabarba/beacon")
  :pin "85261a928ae0ec3b41e639f05291ffd6bf7c231c")
(package! cape
  :recipe (:host github :repo "minad/cape")
  :pin "116063b9ee912cbaa7318dbe6597ade4a62b3f59")

(package! denote
  :recipe (:host github :repo "protesilaos/denote")
  :pin "95a9824ff8d1003985bfd83114ea8a16265047dc")

;(unpin! org-roam) ; Update to latest version to support org-roam-ui
;; The following version for some reason began crashing my Doom... (6/10/23)
;; (package! org-roam
;;  :recipe (:host github :repo "org-roam/org-roam-ui")
;;  :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))
(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea")
  :pin "de199a16e294056e2368a2e031b19008cf9f9e52")
(package! org-edna)
(package! org-gtd
  :recipe (:host github :repo "Trevoke/org-gtd.el")
  :pin "218fba0931cfd666591b09273f9ea8f6a6d8d8f9")
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "eb9f9db40aa529fe4b977235d86494b115281d17")
(package! org-fragtog
  :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")
(package! org-modern
  :pin "afa7d44282d62dbba84afec2a1a6c2a3ee41e7b9")
(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "2c72619195d489a4f6d5370f70dd7f6aa11801c3")
