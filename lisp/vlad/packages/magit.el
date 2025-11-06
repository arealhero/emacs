;; vlad/packages/magit.el --- My magit settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --- git ---
(use-package magit
  :straight t
  :init
  (setq magit-delete-by-moving-to-trash nil)
  (setq transient-history-file (expand-file-name "transient-history" vlad/package-cache-dir)))

(provide 'vlad/packages/magit)
;;; vlad/packages/magit.el ends here
