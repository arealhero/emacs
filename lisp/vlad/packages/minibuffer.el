;; vlad/packages/minibuffer.el --- My minibuffer configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; FIXME(vlad): use ido?
(use-package vertico
  :straight t
  :config
  (vertico-mode t)

  (setq vertico-count 20)

  (defun vlad/minibuffer-backward-kill (arg)
    "Ivy-like behavior for completing file-name with vertico.
When minibuffer is completing a file name delete up to parent folder,
otherwise delete `ARG' characters backward."
    (interactive "p")
    (if (and minibuffer-completing-file-name (string-suffix-p "/" (minibuffer-contents)))
        (vertico-directory-up)
      (delete-char -1 arg)))

  (keymap-set minibuffer-mode-map "DEL" 'vlad/minibuffer-backward-kill))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package marginalia
;;   :straight t
;;   :commands marginalia-mode
;;   :config
;;   (marginalia-mode))

(provide 'vlad/packages/minibuffer)
;;; vlad/packages/minibuffer.el ends here
