;; vlad/packages/company.el --- My company configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package company
  :straight t
  :config
  (setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.2)))
  (setq company-global-modes '(not erc-mode message-mode eshell-mode))

  (add-hook 'c-ts-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (company-mode -1))))

  ;; XXX(vlad): do I want to be able to see documentation? Probably not, but not sure.
  ;; (define-key company-active-map (kbd "C-h") nil)

  ;; TODO(vlad): disable these keys in org-mode only.
  ;; (define-key company-active-map (kbd "RET") nil)
  ;; (define-key company-active-map (kbd "<return>") nil)
  ;; (define-key company-active-map (kbd "C-j") 'company-complete-selection)

  (global-company-mode 1))

(provide 'vlad/packages/company)
;;; vlad/packages/company.el ends here
