;;; vlad/emacs/early-init-utils.el --- Functions and variables that are needed during startup.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun vlad/get-env-var (variable default-value)
  "Get env variable `VARIABLE' or return `DEFAULT-VALUE' if `VARIABLE' is not set."
  (if-let (value (getenv variable))
      value
    default-value))

(defun vlad/normalize-directory (directory &optional default-directory)
  (file-name-as-directory (expand-file-name directory default-directory)))

(defconst vlad/cache-dir (vlad/normalize-directory
                          (vlad/get-env-var "XDG_CACHE_HOME" "~/.cache")))
(defconst vlad/emacs-cache-dir (vlad/normalize-directory "emacs" vlad/cache-dir))

;; NOTE(vlad): functions to retrieve normalized cache files and directories.
(defun vlad/get-cache-file (filename)
  "Get `FILENAME' in Emacs' cache directory."
  (expand-file-name filename vlad/emacs-cache-dir))

(defun vlad/get-cache-dir (dir)
  "Get `DIR' in Emacs' cache directory (creates it if needed)."
  (let ((cache-dir (vlad/normalize-directory (vlad/get-cache-file dir))))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir))
    cache-dir))

(defconst vlad/emacs-eln-cache-dir (vlad/get-cache-dir "eln-cache"))

(provide 'vlad/emacs/early-init-utils)
;;; vlad/emacs/early-init-utils.el ends here
