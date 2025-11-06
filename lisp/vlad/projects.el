;;; vlad/projects.el --- Package for project management.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar vlad/projects-cache-directory (vlad/get-cache-dir "projects"))
(defvar vlad/projects-list (make-hash-table :test 'equal))
(defvar vlad/project-list-files-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git")

;; FIXME(vlad): support per-project directories blacklist.
;; ((nil . ((vlad/project-list-files-command . "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git --exclude idm"))))

;; (use-package projectile
;;   :straight t
;;   :diminish
;;   :config
;;   (projectile-mode)
;;   (setq projectile-indexing-method 'alien ;; To be explicit
;;         projectile-generic-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git"
;;         projectile-track-known-projects-automatically nil
;;         projectile-switch-project-action 'projectile-dired)

;;   (defconst vlad/projectile-cache-dir (vlad/get-cache-dir "projectile"))
;;   (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" vlad/projectile-cache-dir))

;;   ;; NOTE(vlad): projectile unconditionally searches git submodules. This tricks projectile to run /usr/bin/true,
;;   ;;             essentially returning empty submodules list.
;;   (setq projectile-git-submodule-command "true"))

(defun vlad/add-project (directory name)
  "Add DIRECTORY to the list of projects with NAME as its alias."
  (interactive "DDirectory: \nsName: ")
  (let ((existing-directory (gethash name vlad/projects-list)))
    (when existing-directory
      (error "Project '%s' already exists: %s" name existing-directory)))
  (puthash name (file-name-as-directory (expand-file-name directory)) vlad/projects-list)
  (message "Project '%s' added" name)
  (vlad/save-projects-info))

(defun vlad/remove-project (name)
  "Remove project with alias NAME."
  (interactive
   (list (completing-read "Project: " (hash-table-keys vlad/projects-list))))
  (unless (gethash name vlad/projects-list)
    (error "Project %s does not exist" name))
  (remhash name vlad/projects-list)
  (message "Project '%s' removed" name)
  (vlad/save-projects-info))

(defun vlad/get-project-info ()
  "Get current project information: directory and name."
  (interactive)
  (let ((current-directory (file-name-as-directory (expand-file-name default-directory)))
        (deepest-root nil)
        (project-name nil)
        (max-depth 0))
    (maphash (lambda (name project-root)
               (when (string-prefix-p project-root current-directory)
                 (let ((depth (length project-root)))
                   (when (> depth max-depth)
                     (setq max-depth depth)
                     (setq project-name name)
                     (setq deepest-root project-root)))))
             vlad/projects-list)
    (list project-name deepest-root)))

(defun vlad/get-project-name () (nth 0 (vlad/get-project-info)))
(defun vlad/get-project-root () (nth 1 (vlad/get-project-info)))

;; ;;;###autoload
;; (defun project-compile ()
;;   "Run `compile' in the project root."
;;   (declare (interactive-only compile))
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t)))
;;         (compilation-buffer-name-function
;;          (or project-compilation-buffer-name-function
;;              compilation-buffer-name-function)))
;;     (call-interactively #'compile)))

;; FIXME(vlad): store previous command for this project.
;; FIXME(vlad): set `compilation-buffer-name-function' to something like "*`project-alias' - compile*"
(defun vlad/compile-project ()
  "Compile current project.

Runs `compile' at the current project's root."
  (interactive)
  (let* ((project-root (vlad/get-project-root))
         (default-directory project-root))
    (unless project-root
      (error "Current buffer does not belong to any known project."))
    (call-interactively #'compile)))

;; FIXME(vlad): print directories alongside names.
(defun vlad/switch-project (project-name)
  (interactive
   (let ((current-project-name (vlad/get-project-name)))
     (list (completing-read "Switch to: "
                            (seq-filter (lambda (name) (not (eq name current-project-name)))
                                        (hash-table-keys vlad/projects-list))))))
  (let ((project-root (gethash project-name vlad/projects-list)))
    (unless project-root (error "Project '%s' does not exist" project-name))
    (dired project-root)))

;; Serialization
(defun vlad/save-projects-info ()
  (let ((cache-file (expand-file-name "projects.eld" vlad/projects-cache-directory)))
    (unless (file-writable-p cache-file)
      (error "Cache file '%s' is not writeable" cache-file))
    (with-temp-file cache-file
      (insert (let (print-length) (prin1-to-string vlad/projects-list))))))

(defun vlad/load-projects-info ()
  (let ((cache-file (expand-file-name "projects.eld" vlad/projects-cache-directory)))
    (with-demoted-errors
        "Failed to load projects: %S"
      (when (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (setq vlad/projects-list (read (buffer-string))))))))

(defun vlad/list-project-files ()
  (interactive)
  (let ((default-directory (vlad/get-project-root)))
    (unless default-directory
      (error "Current buffer does not belong to any known project."))
    (with-temp-buffer
      (shell-command vlad/project-list-files-command t "*vlad-projects-errors*")
      (let ((shell-output (buffer-substring (point-min) (point-max))))
        (split-string (string-trim shell-output) "\0" t)))))

(defun vlad/find-project-file (file)
  (interactive
   (list (completing-read "Find file: " (vlad/list-project-files))))
  (find-file (expand-file-name file (vlad/get-project-root))))

(define-minor-mode vlad-projects-mode
  "My minor mode for project management."
  :group 'projects
  :require 'projects
  :global t
  (cond (vlad-projects-mode (vlad/load-projects-info))
        (t (vlad/save-projects-info))))

(general-def 'normal '(general-default-keymaps dired-mode-map)
  :prefix "SPC"

  "pa" 'vlad/add-project
  "pc" 'vlad/compile-project
  "pd" 'vlad/remove-project
  "pp" 'vlad/switch-project

  ;; FIXME(vlad): this should be something like projectile-ripgrep.
  ;; "ps" 'projectile-ripgrep
  "ps" 'rgrep

  "SPC" 'vlad/find-project-file
  )

(vlad/load-projects-info)
(add-hook 'kill-emacs-hook 'vlad/save-projects-info)

(vlad-projects-mode 1)

(provide 'vlad/projects)
;;; vlad/projects.el ends here
