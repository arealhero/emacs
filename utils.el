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
  ;; TODO: test if the file exists
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

