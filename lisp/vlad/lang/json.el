;;; vlad/lang/json.el --- My JSON settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package json-ts-mode
  :after treesit
  :defer t
  :config
  (add-hook 'json-ts-mode-hook (lambda () (setq standard-indent 2))))

(provide 'vlad/lang/json)
;;; vlad/lang/json.el ends here
