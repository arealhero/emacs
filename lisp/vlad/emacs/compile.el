;; vlad/emacs/compile.el --- My compilation-mode configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'compile)

(setq compilation-max-output-line-length nil)

(if (vlad/windows-system-p)
    (setq compile-command "build.bat")
  (setq compile-command "./build.sh"))

(provide 'vlad/emacs/compile)
;;; vlad/emacs/compile.el ends here
