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
(require 'vlad/emacs/compile)
;; (require 'vlad/emacs/tree-sitter)

;; NOTE(vlad): use straight to fetch packages.
(require 'vlad/packages/straight)

(require 'vlad/packages/general)
(require 'vlad/packages/company)
(require 'vlad/packages/consult)
(require 'vlad/packages/evil)
(require 'vlad/packages/magit)
(require 'vlad/packages/minibuffer)
;;(require 'vlad/packages/org)
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
(require 'vlad/lang/cmake)
(require 'vlad/lang/cxx)
(require 'vlad/lang/json)
(require 'vlad/lang/yaml)

;; NOTE(vlad): my packages.
(require 'vlad/fixme)
(require 'vlad/projects)
(require 'vlad/debug)

;; TODO(vlad): move to `vlad/emacs/octave.el'.
;; FIXME(vlad): wrap `octave-source-file' so it could open inferior octave buffer in a split.
;;              And also it should probably `erase-buffer' before sourcing file
;;              also clearing and clearvars-ing existing octave session.
(setq inferior-octave-startup-args '("-i" "-q" "--no-line-editing"))

;; (require 'vlad-ya)

;; NOTE(vlad): `emacsclient' is used as a default editor, so the server needs to be started here.
(server-start)

;; NOTE(vlad): I don't know why but requiring eglot messes something up real badly. For example, M-x starts failing
;;             due to `set-local' function being undefined. Clean rebuild didn't help. The funny thing is that
;;             if I require eglot after initialising my straight.el packages then all works just fine.
;;
;;             I have no idea why that happens and I don't care. That said, I decided to move the eglot initialisation
;;             here.
(require 'vlad/emacs/eglot)

(provide 'init)

;;; init.el ends here
