;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! beacon
  :recipe (:host github :repo "Malabarba/beacon")
  :pin "85261a928ae0ec3b41e639f05291ffd6bf7c231c")

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "2c72619195d489a4f6d5370f70dd7f6aa11801c3")

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

(unpin! org-roam) ; NOTE: org-roam-ui requires latest org-roam.
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))
