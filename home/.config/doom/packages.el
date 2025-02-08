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

;; ;; FIXME: Temporary re-pinning use-package to a valid commit since it does not appear
;; ;;        to be a correct commit hash on GitHub.
;; (package! use-package
;;   :recipe (:host github :repo "emacs-straight/use-package")
;;   :pin "fc8449bb593de8fce0a8bdd4ee2cb78219cf8c76")

;; ;; FIXME: Temporary move to main branch.
;; (package! emacsql :recipe (:branch "main"))

;; (package! denote
;;   :recipe (:host github :repo "protesilaos/denote")
;;   :pin "3bb05f212cc29fa6953e6d703e5e0c2e982882a9")

(package! doom-themes
  :recipe (:host github :repo "doomemacs/themes"))

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09")

(when (< emacs-major-version 29)
  ;; FIXME: Until Doom fixes this issue, with Emacs 28 on Debian,
  ;; we must use and old version of emacsql for Org-Roam.
  (package! emacsql
    :recipe (:host github :repo "magit/emacsql")
    :pin "fb05d0f72729a4b4452a3b1168a9b7b35a851a53"))

(package! org-modern
  :pin "87df4997da28cb9335dc4f0224e9bcedb595e425")

;; FIXME: Updates to latest, one past current Doom to depend on emacsql 4.0.0
;; (package! org-roam
;;   :recipe (:host github :repo "org-roam/org-roam")
;;   :pin "64e302c1269a1a16c4c0b5a7d1e3baf2d5ded174")


;; (package! org-roam-ui
;;   :recipe (:host github :repo "org-roam/org-roam-ui"))

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
;; (package! org-edna)
;; (package! org-gtd
;;   :recipe (:host github :repo "Trevoke/org-gtd.el")
;;   :pin "218fba0931cfd666591b09273f9ea8f6a6d8d8f9")
