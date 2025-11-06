;; -*- lexical-binding: t; -*-

(setq process-adaptive-read-buffering nil)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 1 1024 1024)))

(setq gc-cons-threshold (* 200 1024 1024))
(setq jit-lock-defer-time 0)

(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %.2f seconds with %d garbage collections."
                              (float-time (time-subtract after-init-time before-init-time))
                              gcs-done)))

;; NOTE(vlad): general Emacs configuration.
(require 'vlad/emacs/utils)
(require 'vlad/emacs/config)
(require 'vlad/emacs/theme)

;; NOTE(vlad): Emacs built-in packages configuration.
(require 'vlad/emacs/dired)
(require 'vlad/emacs/hideshow)
(require 'vlad/emacs/tree-sitter)

;; NOTE(vlad): use straight to fetch packages.
(require 'vlad/packages/straight)

(require 'vlad/packages/general)
(require 'vlad/packages/company)
(require 'vlad/packages/consult)
(require 'vlad/packages/evil)
(require 'vlad/packages/magit)
(require 'vlad/packages/minibuffer)
(require 'vlad/packages/org)
(require 'vlad/packages/undo-tree)

;; FIXME(vlad): flycheck vs flymake.
;; FIXME(vlad): enable flycheck for some modes like `emacs-lisp'?
;; (use-package flycheck
;;   :straight t
;;   :config
;;   :hook
;;   (after-init . global-flycheck-mode))

;; NOTE(vlad): diminish fetched packages.
(require 'vlad/packages/diminish)

;; NOTE(vlad): language-specific configuration.
(require 'vlad/lang/cxx)

;; NOTE(vlad): my packages.
(require 'vlad/fixme)
(require 'vlad/projects)

;; (require 'vlad-ya)

(provide 'init)

;;; init.el ends here
