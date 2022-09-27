;; TODO: fix run-fzf function
;;       It uses relative paths, so it cannot open a file in the root folder
(defun vlad/run-fzf ()
  (interactive)
  (let ((process-environment
	 (cons (concat "FZF_DEFAULT_COMMAND=rgf")
	       process-environment)))
    (fzf/start default-directory #'fzf/action-find-file)))

(defun vlad/update-packages ()
  (interactive)
  (package-refresh-contents)
  (package-update-all nil)
  (message "Update completed."))

(defun vlad/display-current-time ()
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun vlad/open-file (filename directory)
  (find-file (expand-file-name filename directory)))

(defun vlad/open-org-file (filename)
  (vlad/open-file filename vlad/org-directory))

(defun vlad/open-config-file (filename)
  (vlad/open-file filename user-emacs-directory))

(defun vlad/open-diary ()
  (interactive)
  (vlad/open-org-file "diary.org"))

(defun vlad/open-plan ()
  (interactive)
  (vlad/open-org-file "plan.org"))

(defun vlad/open-todo ()
  (interactive)
  (vlad/open-org-file "todo.org"))

(defun vlad/open-config ()
  ;; TODO: test if file exists
  (interactive)
  (vlad/open-config-file "config.el"))

(defun vlad/font-adjust (increment)
  (let ((inhibit-message t))
    (global-text-scale-adjust increment)))

(defun vlad/font-inc ()
  (interactive)
  (vlad/font-adjust +5))

(defun vlad/font-dec ()
  (interactive)
  (vlad/font-adjust -5))
