;;; +org-roam.el --- My org-roam configuration and extensions until later replacement -*- lexical-binding: t; -*-

;; NOTE: org-roam-tag-* functions use org-collect-keywords, I don't like it's speed. It's 4x slower.

(use-package! org-roam
  :defer t
  :after org
  :init
  (setq org-roam-directory org-directory
        org-roam-db-location (expand-file-name "data/org-roam.db" org-roam-directory))
  :config
  ;; Modify org-roam filename to be compatible with filename scheme (Denote).
  (cl-defmethod org-roam-node-slug :around ((node org-roam-node))
    "Return the slug of NODE."
    (ignore node)
    (string-replace "_" "-" (cl-call-next-method)))

  (defun org-roam-agenda-files ()
    "Return a list of org-roam node files containing the `agenda' tag."
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"agenda\"%"))]))))

  (defun org-roam-agenda-files-update (&rest args)
    "A function used to get and set a list of org-agenda-files."
    (ignore args)
    (setq org-agenda-files (org-roam-agenda-files)))

  (setq org-file-update-agenda-fn #'org-roam-agenda-files-update)

  ;; TODO: Move to org-file or something.
  (advice-add 'org-agenda :before #'org-roam-agenda-files-update)
  (advice-add 'org-todo-list :before #'org-roam-agenda-files-update)


  (require 'org-roam-include)    ;; Prevents certain files with org headings with org-id from being included in database.
  (setq org-roam-link-auto-replace nil) ;; Prevent org-roam link reformatter from running on auto-save since using v2 instead of v1.
  (setq org-roam-db-node-include-function #'org-roam-include-p
        org-roam-include-exclude-directories (list org-inbox-directory)
        org-roam-include-exclude-files (list "agenda.org" "bookmarks.org"))

  ;; Exclude nodes tagged as a person from being inherited.
  (add-to-list 'org-tags-exclude-from-inheritance "person")
  (require 'org-roam-tags))      ;; Automatic tag insertion after node insertion.

(use-package! org-roam-capture
  :defer t
  :config
  (setq org-roam-capture-templates
        '(("n" "node" plain "%?"
           :target (file+head "node/${id}--${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :target (file+head "resource/person/${id}--${slug}.org"
                              "#+title: ${title}\n#+filetags: :person:\n")
           :unnarrowed t)
          ("P" "project" plain "%?"
           :target (file+head "project/${id}--${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:\n")
           :unnarrowed t))))

(use-package! org-roam-protocol
  :defer t
  :config
  (setq org-roam-protocol-store-links t)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "*%?"
           :target (file+head "resource/reference/${id}--${slug}.org" "#+title: ${title}\n\n${body}")
           :unnarrowed t)
          ("b" "Bookmark" plain "%?"
           :target (file+olp "resource/bookmarks.org" ("*Testing"))
           :unnarrowed t
           :empty-lines-before 1))))

(use-package! org-roam-dailies
  :defer t
  :config
  ;; (set-popup-rule! "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)" :side 'right :vslot 1 :width 60 :modeline nil :select t :quit nil)
  (setq org-roam-dailies-directory "log/journal/"
        ;; TODO: Investigate if this format will show up in Calendar. Due to org-id and timestamp modifications.
        org-roam-dailies-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${id}--log.org" "#+title: Log\n#+date: %u\n#+filetags:\n\n* Log")))))
