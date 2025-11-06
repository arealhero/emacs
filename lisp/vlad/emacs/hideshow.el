;; vlad/emacs/hideshow.el --- My hideshow configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hideshow)

(add-hook 'c-ts-mode-hook 'hs-minor-mode)
(add-hook 'c++-ts-mode-hook 'hs-minor-mode)

(provide 'vlad/emacs/hideshow)
;;; vlad/emacs/hideshow.el ends here
