;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! beacon
  :recipe (:host github :repo "Malabarba/beacon")
  :pin "85261a928ae0ec3b41e639f05291ffd6bf7c231c")
(package! doom-modeline ; See: #673
  :recipe (:host github :repo "seagle0128/doom-modeline")
  :pin "cb20d1e2347ebe2ffaf5caaa792f6986a8a190cf")
(package! denote
  :recipe (:host github :repo "protesilaos/denote")
  :pin "59e6c43f9bd5ef35bde891b7ec9fab44aa991344")

;(unpin! org-roam) ; Update to latest version to support org-roam-ui
;; The following version for some reason began crashing my Doom... (6/10/23)
;; (package! org-roam
;;  :recipe (:host github :repo "org-roam/org-roam-ui")
;;  :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))
(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar")
  :pin "1e06d1b4ab5f0d09301712cdecb757c9437a7179")

(package! org-super-agenda
  :recipe (:host github :repo "alphapapa/org-super-agenda")
  :pin "51c9da5ce7b791150758984bab469d2222516844")

(package! org-ql
  :recipe (:host github :repo "alphapapa/org-ql")
  :pin "c9370982bfd4df04b590762bd795a7da3012c4dd")
(package! orca
  :recipe (:host github :repo "abo-abo/orca")
  :pin "0687f416a5573f63b691d384454f5a793266ed97")
(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea")
  :pin "de199a16e294056e2368a2e031b19008cf9f9e52")
(package! org-modern
  :pin "c97096b3f82c3bd84859f90a23273dfa2c6dfd7e")
(package! org-edna)
(package! org-gtd
  :recipe (:host github :repo "Trevoke/org-gtd.el")
  :pin "218fba0931cfd666591b09273f9ea8f6a6d8d8f9")
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "81eba5d7a5b74cdb1bad091d85667e836f16b997")
(package! org-fragtog
  :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")
(package! orglink)
(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "2c72619195d489a4f6d5370f70dd7f6aa11801c3")
