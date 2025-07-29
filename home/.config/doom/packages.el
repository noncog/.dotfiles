;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! sinister
  :recipe (:host github :repo "positron-solutions/sinister")
  :pin "c8f24cb8aaf6d772d45f817d8bfd9eaa0b6a254c")

(package! mini-ontop
  :recipe (:host github :repo "hkjels/mini-ontop.el")
  :pin "37a5dd35be310ca2752f04b64b8b17136503f505")

(package! stillness-mode
  :recipe (:host github :repo "neeasade/stillness-mode.el")
  :pin "05029febdb451941ed218e6ddbef5294776e31d4")

(package! nov :pin "b37d9380752e541db3f4b947c219ca54d50ca273")

(package! denote
  :recipe (:host github :repo "protesilaos/denote")
  :pin "3bb05f212cc29fa6953e6d703e5e0c2e982882a9")

(package! orca
  :recipe (:host github :repo "abo-abo/orca")
  :pin "0687f416a5573f63b691d384454f5a793266ed97")

(package! org-ql
  :recipe (:host github :repo "alphapapa/org-ql")
  :pin "98c62ab0a6c084ae4132110e24d9fe1ace91d363")

(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar")
  :pin "1e06d1b4ab5f0d09301712cdecb757c9437a7179")
