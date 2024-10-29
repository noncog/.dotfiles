;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09")

(package! denote
  :recipe (:host github :repo "protesilaos/denote")
  :pin "3bb05f212cc29fa6953e6d703e5e0c2e982882a9")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "2a630476b3d49d7106f582e7f62b515c62430714")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"))

;; (package! org-sidebar
;;   :recipe (:host github :repo "alphapapa/org-sidebar")
;;   :pin "1e06d1b4ab5f0d09301712cdecb757c9437a7179")

;; (package! org-super-agenda
;;   :recipe (:host github :repo "alphapapa/org-super-agenda")
;;   :pin "51c9da5ce7b791150758984bab469d2222516844")

;; (package! org-ql
;;   :recipe (:host github :repo "alphapapa/org-ql")
;;   :pin "c9370982bfd4df04b590762bd795a7da3012c4dd")
;; (package! orca
;;   :recipe (:host github :repo "abo-abo/orca")
;;   :pin "0687f416a5573f63b691d384454f5a793266ed97")
;; (package! org-modern
;;   :pin "c97096b3f82c3bd84859f90a23273dfa2c6dfd7e")
;; (package! org-edna)
;; (package! org-gtd
;;   :recipe (:host github :repo "Trevoke/org-gtd.el")
;;   :pin "218fba0931cfd666591b09273f9ea8f6a6d8d8f9")
