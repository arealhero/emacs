;; vlad/packages/straight.el --- Bootstrap straight.el package.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar bootstrap-version)
(setq straight-base-dir vlad/emacs-cache-dir)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default 't
              straight-vc-git-default-clone-depth 1)

(straight-use-package 'use-package)

(provide 'vlad/packages/straight)
;;; vlad/packages/straight.el ends here
