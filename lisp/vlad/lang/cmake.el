;;; vlad/lang/cmake.el --- My CMake settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-ts-mode
  :after treesit
  :defer t
  :mode (("ya\\.make\\'" . cmake-ts-mode))) ;; FIXME(vlad): move to vlad-ya.el

(provide 'vlad/lang/cmake)
;;; vlad/lang/cmake.el ends here
