;;; vlad/lang/cmake.el --- My CMake settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-ts-mode
  :after treesit
  :defer t
  :mode (("\\.cmake\\'" . cmake-ts-mode)
         ("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ;; FIXME(vlad): move to vlad-ya.el
         ("ya\\.make\\'" . cmake-ts-mode)))

(provide 'vlad/lang/cmake)
;;; vlad/lang/cmake.el ends here
