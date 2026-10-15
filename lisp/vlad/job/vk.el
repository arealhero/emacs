;;; vlad/job/vk.el --- VK-specific settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package cmake-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(provide 'vlad/job/vk)
;;; vlad/job/vk.el ends here
