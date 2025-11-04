;; vlad-git.el --- My magit settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --- git ---
(use-package magit
  :straight t

  :config
  (setq magit-delete-by-moving-to-trash nil)

  (setq transient-history-file (expand-file-name "transient-history" vlad/package-cache-dir))

  :general-config
  (general-def 'normal '(general-default-keymaps
                         dired-mode-map)
    :prefix "SPC"
    "gg" 'magit)

  (general-def 'emacs '(magit-mode-map
                        magit-file-section-map
                        magit-status-mode-map)
    "j" 'magit-next-line
    "k" 'magit-previous-line
    "l" 'right-char
    "h" 'left-char

    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line

    "gt" 'evil-tab-next
    "gT" 'tab-bar-switch-to-prev-tab

    "V" 'evil-visual-screen-line

    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up

    "C-w h" 'evil-window-left
    "C-w j" 'evil-window-down
    "C-w k" 'evil-window-up
    "C-w l" 'evil-window-right

    "C-j" 'magit-section-forward
    "C-k" 'magit-section-backward

    "/" 'evil-ex-search-forward
    "n" 'evil-ex-search-next
    "p" 'evil-ex-search-previous)

  (general-def 'visual '(magit-mode-map
                         magit-file-section-map
                         magit-status-mode-map)
    "s" 'magit-stage)

  ;; FIXME(vlad): move to vlad-describe.el
  (general-def 'normal '(magit-mode-map
                         magit-file-section-map
                         magit-status-mode-map)
    :prefix "SPC"
    "h f" 'describe-function
    "h k" 'describe-key
    "h m" 'describe-mode
    "h v" 'describe-variable)
  )

(provide 'vlad-git)
;;; vlad-git.el ends here
