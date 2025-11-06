;;; vlad/lang/yaml.el --- My Yaml settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yaml-ts-mode
  :after treesit
  :defer t
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.clang-format\\'" . yaml-ts-mode)
         ("\\.clang-tidy\\'" . yaml-ts-mode)
         ("\\.clangd\\'" . yaml-ts-mode)))

(provide 'vlad/lang/yaml)
;;; vlad/lang/yaml.el ends here
