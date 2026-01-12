;; vlad/emacs/dired.el --- My Dired configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dired)

(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-dwim-target t)

(defun vlad/open-current-directory-in-dired ()
  (interactive)
  (dired "."))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'vlad/emacs/dired)
;;; vlad/emacs/dired.el ends here
