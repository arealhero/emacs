;;; vlad-projects.el --- Manage projects.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)
(setq project-list-file (vlad/get-cache-file "projects.eld"))

(general-def 'normal '(general-default-keymaps dired-mode-map)
  :prefix "SPC"

  ;; "pa" 'projectile-add-known-project
  ;; "pc" 'projectile-compile-project
  ;; "pd" 'projectile-remove-known-project
  ;; "pi" 'projectile-invalidate-cache
  ;; "pp" 'projectile-switch-project

  "pc" 'project-compile
  "pd" 'project-forget-project
  "pp" 'project-switch-project

  ;; FIXME(vlad): this should be something like projectile-ripgrep.
  ;; "ps" 'projectile-ripgrep
  "ps" 'rgrep

  ;; "SPC" 'projectile-find-file
  "SPC" 'project-find-file
  )

;; (defconst vlad/projects-list-files-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git")

;; (defun vlad/projects-list-files (project-root-directory)
;;   (interactive)
;;   (let ((default-directory project-root-directory))
;;     (message "root: %s\n" project-root-directory)
;;     (with-temp-buffer
;;       (shell-command vlad/projects-list-files-command t "*vlad-projects-errors*")
;;       (let ((shell-output (buffer-substring (point-min) (point-max))))
;;         (split-string (string-trim shell-output) "\0" t)))))

(provide 'vlad-projects)
;;; vlad-projects.el ends here
