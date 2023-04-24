(setq user-full-name "Jake Turner"
      user-mail-address "john@doe.com")

(setq doom-theme 'doom-oceanic-next)

(when IS-MAC
  (setq doom-font (font-spec :family "Jetbrains Mono" :size 12)
        doom-big-font (font-spec :family "Jetbrains Mono" :size 16)
        doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
        doom-unicode-font (font-spec :family "JuliaMono")
        doom-serif-font (font-spec :family "IBM Plex Mono" :size 10 :weight 'light)))

(when IS-LINUX
  (setq doom-font (font-spec :family "Jetbrains Mono" :size 12)
        doom-big-font (font-spec :family "Jetbrains Mono" :size 16)
        ;;doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
        ;;doom-unicode-font (font-spec :family "JuliaMono")
        ;;doom-serif-font (font-spec :family "IBM Plex Mono" :size 10 :weight 'light)
        ))

(setq-default x-stretch-cursor t)

(add-to-list 'default-frame-alist '(alpha . 93)) ; [0-100]

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type 'visual)

(setq-default evil-scroll-count 5)

(global-auto-revert-mode 1)

(global-subword-mode 1)

(setq evil-want-fine-undo t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(defun wm-focus-on-error (direction move-fn)
    (when IS-LINUX
     (condition-case nil (funcall move-fn)
       (user-error (start-process "wm" nil "i3-msg" "focus" direction))))
     (when IS-MAC
      (condition-case nil (funcall move-fn)
        (user-error (start-process "wm" nil "yabai" "-m" "window" "--focus" direction)))))

(defun wm-window-left ()
  (interactive)
  (let ((direction (cond (IS-LINUX "left") (IS-MAC "west"))))
    (wm-focus-on-error direction #'windmove-left)))

(defun wm-window-right ()
  (interactive)
  (let ((direction (cond (IS-LINUX "right") (IS-MAC "east"))))
    (wm-focus-on-error direction #'windmove-right)))

(defun wm-window-up ()
  (interactive)
  (let ((direction (cond (IS-LINUX "up") (IS-MAC "north"))))
    (wm-focus-on-error direction #'windmove-up)))

(defun wm-window-down ()
  (interactive)
  (let ((direction (cond (IS-LINUX "down") (IS-MAC "south"))))
    (wm-focus-on-error direction #'windmove-down)))

(when IS-MAC
  (map! "s-h" #'wm-window-left)
  (map! "s-j" #'wm-window-down)
  (map! "s-k" #'wm-window-up)
  (map! "s-l" #'wm-window-right)
  (map! "s-=" #'balance-windows)
  (map! "s-Q" #'evil-quit))

(when IS-LINUX
  (map! "s-h" #'wm-window-left)
  (map! "s-j" #'wm-window-down)
  (map! "s-k" #'wm-window-up)
  (map! "s-l" #'wm-window-right)
  (map! "s-Q" #'evil-quit))

(map! :leader "w h" #'wm-window-left)
(map! :leader "w j" #'wm-window-down)
(map! :leader "w k" #'wm-window-up)
(map! :leader "w l" #'wm-window-right)

(when IS-MAC (setq mac-command-modifier 'control ; Maps Command -> Control
                    mac-control-modifier 'meta   ; Maps Control -> Alt (Meta)
                    mac-option-modifier 'super)) ; Maps Option -> Super

<<pre-org>>
(after! org
  <<org-config>>
  )

<<pre-org-agenda>>
(after! org-agenda
  <<org-agenda-config>>
  )

(after! org-capture
  <<org-capture-config>>
  )

<<pre-org-roam>>
(use-package! org-roam
  :config
  <<org-roam-config>>
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  <<org-roam-ui-config>>
  )

(set-popup-rule! "^*xwidget" :ignore t :side 'left :width 120 :vslot 1 :quit t :select t :modeline nil)

(defcustom org-roam-ui-use-webkit t
  "Use embedded webkit to preview.
This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets`
option."
  :type 'boolean
  :group 'roam)

(defun org-roam-ui-browser (url)
  "Use browser specified by user to load URL.
Use default browser if nil."
  (if org-roam-ui-url-browser
      (let ((browse-url-generic-program org-roam-ui-url-browser)
            (browse-url-generic-args roam-url-args))
        (ignore browse-url-generic-program)
        (ignore browse-url-generic-args)
        (browse-url-generic url))
    (browse-url url)))

(defun org-roam-ui-open-url (url)
  "Ask the browser to load URL.
Use default browser unless `xwidget' is available."
  (if (and org-roam-ui-use-webkit
           (featurep 'xwidget-internal))
      (progn
        (save-window-excursion (xwidget-webkit-browse-url url))
        (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
          (when (buffer-live-p buf) (and (eq buf (current-buffer)) (quit-window)))
          (display-buffer buf)))
            (let (display-buffer-alist) (org-roam-ui-browser url))))

;;;###autoload
(define-minor-mode org-roam-ui-open-in-browser
  "open org-roam-ui in the browser"
 :lighter "roam"
 (org-roam-ui-open-url "http://127.0.0.1:35901"))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  <<org-modern-config>>
  )

(use-package! org-modern-indent
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 1))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  <<org-appear-config>>
  )

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! ox
  <<ox-config>>)

(map! :map org-mode-map
      :localleader
      :desc "View exported file"
      "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any),
found, using `org-view-output-file-extensions'."
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Find output files with these extensions, in order, viewing the first match.")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(after! ox-latex
  
  (setq org-latex-src-block-backend 'engraved)
  (add-to-list 'org-latex-classes
               '("notes"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  )

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! engrave-faces-html
  :after ox-html)

(after! org-noter
  <<org-noter-config>>
  )

(after! projectile
  <<projectile-config>>
  )

(after! plantuml-mode
  <<plantuml-mode-config>>
  )

(after! pdf-tools
  <<pdf-tools-config>>
  (pdf-tools-install)
  )

(after! yasnippet
  (use-package! yasnippet
    :config
    <<yasnippet-config>>
    ))

(after! magit
  <<magit-linter>>
  <<magit-config>>
  )

(after! company
  <<company-config>>
  )

(after! vterm
  (use-package! vterm
    :config
    <<vterm-config>>
    ))

;; Add an extra line to work around bug in which-key imprecise
(after! which-key
  (defun add-which-key-line (f &rest r) (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))))
  (advice-add 'which-key--show-popup :around #'add-which-key-line)
  )

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(after! persp-mode
  <<pers-config>>
  )

(defun noncog/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . noncog/org-mode-visual-fill))

(after! treemacs
  <<treemacs-config
  )

(after! vertico
  <<vertico-config>>
  )

(map! :after vertico
      :map vertico-map
      "C-d" #'scroll-up-command)

(map! :after vertico
      :map vertico-map
      "C-u" #'scroll-down-command)

(after! doom-modeline
  <<doom-modeline-config>>
  )

(setq doom-modeline-height 35)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name t)
(setq doom-modeline-persp-icon t)

(after! nav-flash
  <<nav-flash-config>>
  )

(after! doom-dashboard
  <<doom-dashboard-config>>
  )

(after! lsp-clangd
  <<lsp-clangd-config>>
  )
