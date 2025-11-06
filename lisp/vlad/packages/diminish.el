;; vlad/packages/diminish.el --- My diminish configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package diminish
  :straight t)

(diminish 'auto-revert-mode)
(diminish 'company-mode)
(diminish 'evil-commentary-mode)
(diminish 'hs-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'whitespace-mode)

(provide 'vlad/packages/diminish)
;;; vlad/packages/diminish.el ends here
