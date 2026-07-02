;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea"))

(package! acp
  :recipe (:host github :repo "xenodium/acp.el"))

(package! shell-maker
  :recipe (:host github :repo "xenodium/shell-maker"))

(package! agent-shell
  :recipe (:host github :repo "xenodium/agent-shell"))

;; Experimental Includes:
(package! vui.el
  :recipe (:host github :repo "d12frosted/vui.el"))

(package! vulpea-ui
  :recipe (:host github :repo "d12frosted/vulpea-ui"))

(package! vulpea-para
  :recipe (:host github :repo "d12frosted/vulpea-para"))

(package! vulpea-journal
  :recipe (:host github :repo "d12frosted/vulpea-journal"))

(package! denote
  :recipe (:host github :repo "protesilaos/denote"))
