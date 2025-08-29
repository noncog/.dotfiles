;;; +shell.el -- My personal shell and terminal configurations -*- lexical-binding: t; -*-

(use-package! vterm
  :defer t
  :config
  ;; Fix M-backspace keybind on macOS. TODO Verify required.
  (evil-define-key* 'insert vterm-mode-map (kbd "<M-backspace>") #'vterm-send-meta-backspace))

(use-package! sh-script
  :defer t
  :init
  (set-file-template! "\\.sh" :trigger "__sh" :mode 'sh-mode)
  :config
  (set-formatter! 'shfmt
    '("shfmt" "-filename" filepath "-ci" "-bn" "-sr" "-ln"
      (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))
      (when apheleia-formatters-respect-indent-level
        (list "-i"
              (number-to-string
               (cond
                (indent-tabs-mode 0)
                ((boundp 'sh-basic-offset)
                 sh-basic-offset)
                (t 4))))))
    :modes '(sh-mode))
  (defun my/bash-info-page ()
    "Go to the Bash info page."
    (interactive)
    (info "Bash")))
