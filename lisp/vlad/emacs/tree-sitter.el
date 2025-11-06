;; vlad/emacs/tree-sitter.el --- My tree-sitter configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'treesit)

(defconst vlad/treesit-cache-dir (vlad/get-cache-dir "tree-sitter"))

(setq treesit-extra-load-path (list vlad/treesit-cache-dir))
(setq treesit-font-lock-level 4)
(setq treesit-language-source-alist
      '(
        ;; (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        ;; (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        ;; (css "https://github.com/tree-sitter/tree-sitter-css")
        ;; (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        ;; (go "https://github.com/tree-sitter/tree-sitter-go")
        ;; (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
        ;; (html "https://github.com/tree-sitter/tree-sitter-html")
        ;; (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json "https://github.com/tree-sitter/tree-sitter-json")
        ;; (lua "https://github.com/Azganoth/tree-sitter-lua")
        ;; (make "https://github.com/alemuller/tree-sitter-make")
        ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        ;; (r "https://github.com/r-lib/tree-sitter-r")
        ;; (rust "https://github.com/tree-sitter/tree-sitter-rust")
        ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
        ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
        ;;      "master"
        ;;      "tsx/src")
        ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
        ;;             "master"
        ;;             "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        ))
;; (setq treesit-load-name-override-list '((js "libtree-sitter-js" "tree_sitter_javascript")))

(if (vlad/macos-system-p)
    (defconst vlad/dynamic-library-suffix ".dylib")
  (defconst vlad/dynamic-library-suffix ".so"))

(dolist (pair treesit-language-source-alist)
  (let ((language (car pair)))
    (unless (file-regular-p (expand-file-name
                             (concat "libtree-sitter-"
                                     (symbol-name language)
                                     vlad/dynamic-library-suffix)
                             vlad/treesit-cache-dir))
      (treesit-install-language-grammar language vlad/treesit-cache-dir))))

(setq major-mode-remap-alist
 '((c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   (c-or-c++-mode . c-or-c++-ts-mode)
   (js-json-mode . json-ts-mode)
   (python-mode . python-ts-mode)))

;; (setq c-ts-mode-indent-style nil)
(setq c-ts-mode-indent-offset 4)

(require 'semantic/symref/grep)
(add-to-list 'semantic-symref-filepattern-alist '(c-ts-mode "*.[ch]"))

;; FIXME(vlad): move to the `lang' folder.
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
(use-package json-ts-mode
  :after treesit
  :defer t
  :config
  (add-hook 'json-ts-mode-hook (lambda () (setq standard-indent 2))))

(use-package cmake-ts-mode
  :after treesit
  :defer t
  :mode (("\\.cmake\\'" . cmake-ts-mode)
         ("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ;; FIXME(vlad): move to vlad-ya.el
         ("ya\\.make\\'" . cmake-ts-mode)))

(use-package yaml-ts-mode
  :after treesit
  :defer t
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.clang-format\\'" . yaml-ts-mode)
         ("\\.clang-tidy\\'" . yaml-ts-mode)
         ("\\.clangd\\'" . yaml-ts-mode)))

(provide 'vlad/emacs/tree-sitter)
;;; vlad/emacs/tree-sitter.el ends here
