;; vlad/packages/undo-tree.el --- My undo-tree configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package undo-tree
  :straight t
  :config
  (defconst vlad/undo-tree-cache-dir (vlad/get-cache-dir "undo-tree"))

  (setq undo-tree-history-directory-alist
        `((".*" . ,vlad/undo-tree-cache-dir)))

  (global-undo-tree-mode 1))

(provide 'vlad/packages/undo-tree)
;;; vlad/packages/undo-tree.el ends here
