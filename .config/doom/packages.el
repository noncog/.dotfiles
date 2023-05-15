;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! beacon
  :recipe (:host github :repo "Malabarba/beacon")
  :pin "85261a928ae0ec3b41e639f05291ffd6bf7c231c")
(unpin! org-roam) ; NOTE: org-roam-ui requires latest org-roam.
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "eb9f9db40aa529fe4b977235d86494b115281d17")
(package! org-fragtog
  :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")
(package! org-modern
  :pin "b614ad31d72c49f9eb29d3836995fd7fb0d1fd4a")
(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent")
  :pin "c5a50f302dc1053d5b498e0ea2bc0ee233e8e1b8")
(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "2c72619195d489a4f6d5370f70dd7f6aa11801c3")
