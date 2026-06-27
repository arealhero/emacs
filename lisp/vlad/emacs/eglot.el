;; vlad/emacs/eglot.el --- My Eglot configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eglot)

(add-to-list 'eglot-server-programs
            '((c-mode c++-mode) . ("clangd" "--header-insertion=never")))

(setq eglot-ignored-server-capabilities
      '(
        :documentHighlightProvider
        :documentOnTypeFormattingProvider ;; NOTE(vlad): Stop messing up my code as I write it.
        ))

(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

(provide 'vlad/emacs/eglot)
;;; vlad/emacs/eglot.el ends here
