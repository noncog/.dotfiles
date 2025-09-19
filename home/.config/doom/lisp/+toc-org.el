;;; +toc-org.el --- Hacks to toc-org until I rewrite. -*- lexical-binding: t; -*-

(use-package! toc-org
  :defer t
  :config
  (setq org-toc-default-depth 3)
  ;; Extend org-toc to add folding to table of contents using HTML.
  (defconst toc-org-fold-tag-regexp
    ":fold:\\(\\(\s+-->\\)?$\\|[^ ]*?:\\(\s+-->\\)?$\\)"
    "Regexp to find the heading with the :fold: tag")
  (defun toc-org-insert-toc (&optional dry-run)
    "Update table of contents in heading tagged :TOC:.

When DRY-RUN is non-nil, the buffer is not modified, only the
internal hash-table is updated to enable `org-open-at-point' for
TOC links.

The table of contents heading may also be set with these tags:

- :TOC_#: Sets the maximum depth of the headlines in the table of
          contents to the number given, e.g. :TOC_3: for
          3 (default for plain :TOC: tag is 2).

- :TOC_#_gh: Sets the maximum depth as above and also uses
             GitHub-style anchors in the table of contents (the
             default).  The other supported style is :TOC_#_org:,
             which is the default org style.

Headings may be excluded from the TOC with these tags:

- :noexport: Exclude this heading.

- :noexport_#: Exclude this heading's children with relative
               level greater than number given (e.g. :noexport_1:
               causes all child headings to be excluded).

Note that :noexport: is also used by Org-mode's exporter, but
not :noexport_#:."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search t)
             (markdown-syntax-p (derived-mode-p 'markdown-mode))
             (heading-symbol-regexp (if markdown-syntax-p "^#" "^\\*")))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward (concat heading-symbol-regexp toc-org-toc-org-regexp) (point-max) t)
          (let* ((tag (match-string 2))
                 ;; is there a better way to convert char to number?
                 (depth (if tag (- (aref tag 1) ?0) toc-org-max-depth))
                 (hrefify-tag (if (and tag (>= (length tag) 4))
                                  (downcase (substring tag 3)) toc-org-hrefify-default))
                 (hrefify-string (concat "toc-org-hrefify-" hrefify-tag))
                 (hrefify (intern-soft hrefify-string))
                 (put-quote (save-match-data (string-match toc-org-quote-tag-regexp (match-string 0))))
                 (put-fold (save-match-data (string-match toc-org-fold-tag-regexp (match-string 0))))
                 (toc-prefix (if put-quote (if markdown-syntax-p "```\n" "#+BEGIN_QUOTE\n") ""))
                 (toc-suffix (if put-quote (if markdown-syntax-p "```\n" "#+END_QUOTE\n") "")))
            (if hrefify
                (let ((new-toc
                       (concat (if put-fold "#+html:<details><summary>Table of Contents</summary>\n" "")
                               toc-prefix
                               (toc-org-hrefify-toc
                                (toc-org-flush-subheadings (toc-org-raw-toc markdown-syntax-p) depth)
                                hrefify
                                markdown-syntax-p
                                (when toc-org-hrefify-hash
                                  (clrhash toc-org-hrefify-hash)))
                               toc-suffix
                               (if put-fold "#+html:</details>\n" ""))))
                  (unless dry-run
                    (newline (forward-line 1))
                    ;; skip drawers
                    (let ((end (save-excursion ;; limit to next heading
                                 (search-forward-regexp heading-symbol-regexp (point-max) 'skip))))
                      (while (re-search-forward toc-org-drawer-regexp end t)
                        (skip-chars-forward "[:space:]")))
                    (beginning-of-line)
                    ;; insert newline if TOC is currently empty
                    (when (looking-at heading-symbol-regexp)
                      (open-line 1))
                    ;; find TOC boundaries
                    (let ((beg (point))
                          (end
                           (save-excursion
                             (when (search-forward-regexp heading-symbol-regexp (point-max) 'skip)
                               (forward-line -1))
                             (end-of-line)
                             (point))))
                      ;; update the TOC, but only if it's actually different
                      ;; from the current one
                      (unless (equal (buffer-substring-no-properties beg end) new-toc)
                        (delete-region beg end)
                        (insert new-toc)))))
              (message (concat "Hrefify function " hrefify-string " is not found")))))))))
