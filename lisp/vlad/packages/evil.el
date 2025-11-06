;; vlad/packages/evil.el --- My evil-mode configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package evil
  :after undo-tree
  :straight t
  :init

  ;; NOTE(vlad): make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  (setq evil-want-C-u-scroll t
        evil-v$-excludes-newline t
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search)

  :config

  (evil-set-undo-system 'undo-tree)

  ;; FIXME(vlad): move to `lang/vlad-cxx'?
  (evil-define-operator vlad/clang-format-region (beg end)
    :move-point nil
    :type line
    (evil-ensure-column
      (save-restriction
        (goto-char beg)
        (clang-format-region beg end)
        (evil-force-normal-state))))

  (evil-mode 1))

;; (use-package evil-anzu
;;   :after evil
;;   :straight t
;;   :config
;;   (global-anzu-mode))

;; FIXME(vlad): use `comment-line' instead. I don't use motions when commenting.
;;              Although it does not work well with visual mode (like `magit-stash') -- it comments out one more line
;;              than highlighted.
(use-package evil-commentary
  :after evil
  :straight t
  :config
  (evil-commentary-mode 1))


(provide 'vlad/packages/evil)
;;; vlad/packages/evil.el ends here
