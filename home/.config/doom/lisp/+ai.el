;;; -*- lexical-binding: t; -*-

(use-package agent-shell
  ;; investigate if after evil needs to happen.
  :defer t
  :config
  (setq agent-shell-preferred-agent-config (agent-shell-opencode-make-agent-config)
        agent-shell-opencode-default-model-id "llama.cpp/unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_M"
        agent-shell-opencode-default-session-mode-id "plan")
  ;; Fix: Tighten agent-shell subdirectory creation from polluting entire drive when project can't be detected.
  (defun my/agent-shell-dot-subdir (subdir)
    "Return absolute path to subdirectory where agent shell data is stored."
    (let* ((org-dir (or (when (boundp 'org-directory) org-directory)
                        (expand-file-name "~/documents/org/")))
           (fallback (expand-file-name (format ".agent-shell/%s" subdir) org-dir))
           (cwd (or (ignore-errors (agent-shell-cwd))
                    default-directory))
           (cwd-trimmed (file-name-as-directory cwd))
           (base-dirs
            (seq-uniq
             (append
              (when (boundp 'magit-repository-directories)
                (mapcar (lambda (e) (car e)) magit-repository-directories))
              (when (boundp 'projectile-project-search-path)
                (mapcar (lambda (e) (car e)) projectile-project-search-path))))))
      (catch 'found
        (dolist (base base-dirs)
          (let ((dir (file-name-as-directory (expand-file-name base))))
            (when (string-prefix-p dir cwd-trimmed)
              (throw 'found (expand-file-name (format ".agent-shell/%s" subdir) cwd)))))
      fallback)))

  ;; Update timestamp to use better one.
  (defun my/agent-shell--default-transcript-file-path ()
  "Generate a transcript file path in for agent-shell."
    (let* ((dir (agent-shell--dot-subdir "transcripts"))
           ;; TODO: Use fref, denote, org-id, etc. add variables to control timestamp.
           (filename (format-time-string "%Y%m%dT%H%M%S"))
           (filepath (expand-file-name filename dir)))
      filepath))
  (setq agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir
        agent-shell-transcript-file-path-function #'my/agent-shell--default-transcript-file-path)
  ;; TODO: Look into shell-maker bindings.
  ;; Fix (Re-bind) broken default binds on Doom.
  (map! :map agent-shell-mode-map
        :m [tab] #'agent-shell-next-item
        :n [return] #'agent-shell-ui-toggle-fragment
        :localleader :desc "Cycle session mode"
        "m"  #'agent-shell-cycle-session-mode))
