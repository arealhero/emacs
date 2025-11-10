;; vlad/emacs/eglot.el --- My Eglot configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; NOTE(vlad): clangd initial arguments.

(when-let* ((clangd (seq-find #'executable-find '("clangd")))
            ;; NOTE(vlad): This has to match the tool string in `compile-commands.json'.
            ;; clangd will then use these tools to get system header paths.
            (init-args "--query-driver=/**/*"))
  (when (eq window-system 'w32)
    (setq init-args "--query-driver=*:\\**\\*"))
  (add-to-list 'eglot-server-programs
               `((c-ts-mode c++-ts-mode) ,clangd ,init-args)))

(provide 'vlad/emacs/eglot)
;;; vlad/emacs/eglot.el ends here
