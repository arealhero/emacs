;;; vlad/emacs/utils.el --- Helpful functions & variables.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst vlad/data-dir (vlad/normalize-directory
                         (vlad/get-env-var "XDG_DATA_DIR" "~/data"))) ;; TODO: this is not XDG's

(defconst vlad/package-cache-dir (vlad/get-cache-file "package-cache"))
(defconst vlad/org-latex-preview-cache-dir (vlad/get-cache-dir "org-latex-preview"))

(defconst vlad/org-files-dir (vlad/normalize-directory "org" vlad/data-dir))

;; By default Emacs creates backup files (e.g. test.c~) and autosave
;; files (e.g. #test.c#) in the same directory the file belongs to.
;; The next code block moves those files to the separate directory.
(defconst vlad/emacs-lock-files-dir (vlad/get-cache-dir "lock-files"))
(defconst vlad/emacs-backup-dir (vlad/get-cache-dir "backups"))

(defun vlad/macos-system-p ()
  "Return non-nil if Emacs was built for a Darwin system (macOS)."
  (eq system-type 'darwin))

(defun vlad/get-current-time-string ()
  "Get current time as string."
  (interactive)
  (format-time-string "%H:%M:%S" (current-time)))

(defun vlad/open-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun vlad/open-plan ()
  (interactive)
  (find-file (expand-file-name "plan.org" vlad/org-files-dir)))

(defun vlad/close-other-tabs ()
  (interactive)
  (tab-bar-close-other-tabs)
  (tab-bar-mode -1))

(defun vlad/kill-other-buffers ()
  (interactive)
  (when tab-bar-mode (vlad/close-other-tabs))
  (let* ((killable-major-modes '(dired-mode
                                 ibuffer-mode
                                 help-mode
                                 magit-status-mode
                                 magit-process-mode
                                 magit-diff-mode))
         (killable-buffer-p (lambda (buffer)
                              (or (buffer-file-name buffer)
                                  (memq (buffer-local-value 'major-mode buffer)
                                        killable-major-modes)))))
    (mapc 'kill-buffer
          (delq (current-buffer)
                (cl-remove-if-not killable-buffer-p (buffer-list))))
    (delete-other-windows)))

(provide 'vlad/emacs/utils)
;;; vlad/emacs/utils.el ends here
