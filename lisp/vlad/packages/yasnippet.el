;; vlad/packages/yasnippet.el --- My yasnippet configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :straight t
  :config
  (yas-reload-all)
  (yas-global-mode))

(provide 'vlad/packages/yasnippet)
;;; vlad/packages/yasnippet.el ends here
