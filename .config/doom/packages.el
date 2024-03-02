;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! beacon
  :recipe (:host github :repo "Malabarba/beacon")
  :pin "85261a928ae0ec3b41e639f05291ffd6bf7c231c")
(package! doom-modeline ; See: #673
  :recipe (:host github :repo "seagle0128/doom-modeline")
  :pin "cfc7bcf1494cb0634a1464dc40a4a1a1e337cb37")
(package! denote
  :recipe (:host github :repo "protesilaos/denote")
  :pin "95a9824ff8d1003985bfd83114ea8a16265047dc")

;; Downgrade to 9.6.9 due to incompatibility with org-ql
(package! org :pin "806abc5a2bbcb5f884467a0145547221ba09eb59")

(package! org-ql
  :recipe (:host github :repo "alphapapa/org-ql")
  :pin "9606aaf81230d1faf2c7f54925b45e527fa32bf0")
;(unpin! org-roam) ; Update to latest version to support org-roam-ui
;; The following version for some reason began crashing my Doom... (6/10/23)
;; (package! org-roam
;;  :recipe (:host github :repo "org-roam/org-roam-ui")
;;  :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))
(package! orca
  :recipe (:host github :repo "abo-abo/orca")
  :pin "0687f416a5573f63b691d384454f5a793266ed97")
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
  :pin "418b1adeec936e42abe1e3151633951055909ea4")
(package! orglink)
(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "2c72619195d489a4f6d5370f70dd7f6aa11801c3")
