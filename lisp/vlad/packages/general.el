;; vlad/packages/general.el --- My general.el configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @ref: https://github.com/noctuid/general.el
(use-package general
  :after evil
  :straight t
  :config

  (general-evil-setup)

  (general-def
    "<escape>" 'keyboard-escape-quit)

  (general-def 'minibuffer-mode-map
    "M-v" 'yank
    "s-v" 'yank
    "C-w" 'backward-kill-word
    "C-ц" 'backward-kill-word
    "<backspace>" 'vlad/minibuffer-backward-kill
    "DEL" 'vlad/minibuffer-backward-kill)

  (general-def 'normal
   :prefix "SPC"
   "bc" 'vlad/kill-other-buffers

   "fs" 'grep-find

   "hf" 'describe-function
   "hk" 'describe-key
   "hv" 'describe-variable
   "hm" 'describe-mode

   "kr" (lambda () (interactive) (yank-pop))

   "mt" 'tab-bar-new-tab
   "mc" 'vlad/close-other-tabs

   "gg" 'magit

   "oc" 'vlad/open-config

   "pv" 'vlad/open-current-directory-in-dired

   ;; FIXME(vlad): modify `consult-themes' to remove Emacs' junk like `light-blue' and whatnot.
   "ht" 'consult-theme

   "up" 'straight-pull-all

   "<" 'consult-buffer

   "." 'find-file)

  (general-def 'insert
    "M-v" 'yank
    "s-v" 'yank
    "C-ц" 'evil-delete-backward-word)

  (general-def 'visual
    "M-v" 'yank)

  (general-def 'normal
    "s-n" 'next-error
    "s-p" 'previous-error

    "]g" 'flymake-goto-next-error
    "[g" 'flymake-goto-prev-error

    ;; FIXME(vlad): don't call `execute-extended-command' if one was already called
    "s-x" 'execute-extended-command

    "s-!" 'shell-command
    "s-&" 'async-shell-command

    "C-x C-b" 'ibuffer

    "C-S-j" 'vlad/font-dec
    "C-О" 'vlad/font-dec
    "C-S-k" 'vlad/font-inc
    "C-Л" 'vlad/font-inc
    "M-v" 'yank
    "M-n" 'evil-buffer-new

    ;; FIXME(vlad): use these: evil's alternatives do not respect visual lines.
    ;;              Also don't forget to change them in `vlad-git.el'
    ;; "C-u" 'scroll-down-command
    ;; "C-d" 'scroll-up-command
    )

  (general-def 'normal profiler-report-mode-map
    "TAB" 'profiler-report-toggle-entry)

  (general-def 'normal xref--xref-buffer-mode-map
    "RET" 'xref-show-location-at-point)

  (general-def 'normal dired-mode-map
    "0" 'evil-beginning-of-line
    "$" 'evil-end-of-line

    "-" 'dired-up-directory

    "gg" (lambda () (interactive) (goto-line (point-min)))
    "G" (lambda () (interactive) (goto-line (point-max)))
    "gt" 'tab-bar-switch-to-next-tab
    "gT" 'tab-bar-switch-to-prev-tab

    "w" 'evil-forward-word-begin)

  (general-def 'normal dired-mode-map
    :prefix "SPC"

    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable
    "hm" 'describe-mode

    "ht" 'consult-theme

    "mt" 'tab-bar-move-window-to-tab

    "gg" 'magit

    "oc" 'vlad/open-config
    "op" 'vlad/open-plan

    "<" 'consult-buffer

    "." 'find-file)

  ;; ----- org-mode-map -----
  (general-def 'normal org-mode-map
    "TAB" 'org-cycle
    "C-k" 'org-timestamp-up
    "C-j" 'org-timestamp-down)

  (general-def 'visual org-mode-map
    "M-h" 'org-metaleft
    "M-l" 'org-metaright)

  (general-def 'normal org-mode-map
   :prefix "SPC"
   "aa" 'org-attach-attach
   "c" 'org-ctrl-c-ctrl-c
   "il" 'org-insert-link
   "lt" 'vlad/org-log-current-time
   "oo" 'org-open-at-point
   "t" 'org-todo)

  (general-def 'normal org-mode-map
   :prefix "SPC"
   "nn" 'org-roam-dailies-goto-next-note
   "np" 'org-roam-dailies-goto-previous-note

   "aa" 'org-roam-alias-add
   "ar" 'org-roam-alias-remove

   "nc" 'org-id-get-create
   )

  ;; ----- org-mode-map -----
  (general-def 'normal
   :prefix "SPC"
   "dt" 'org-roam-dailies-goto-today
   "dy" 'org-roam-dailies-goto-yesterday

   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "ns" 'org-roam-db-sync
   "nt" 'org-roam-buffer-toggle
   )

  (general-def 'insert
   "C-c C-i" 'org-roam-node-insert
   )

  (general-def 'normal dired-mode-map
    :prefix "SPC"

    "dt" 'org-roam-dailies-goto-today
    "dy" 'org-roam-dailies-goto-yesterday

    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert
    "ns" 'org-roam-db-sync
    "nt" 'org-roam-buffer-toggle)

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
    "s" 'magit-stage
    "u" 'magit-unstage)

  ;; FIXME(vlad): move to vlad-describe.el
  (general-def 'normal '(magit-mode-map
                         magit-file-section-map
                         magit-status-mode-map)
    :prefix "SPC"
    "h f" 'describe-function
    "h k" 'describe-key
    "h m" 'describe-mode
    "h v" 'describe-variable)

  (general-def 'normal '(c-ts-mode-map c++-ts-mode-map)
    "=" 'vlad/clang-format-region)

  (general-def 'visual '(c-ts-mode-map c++-ts-mode-map)
    "=" 'vlad/clang-format-region)

  (general-def 'normal '(c-ts-mode-map c++-ts-mode-map)
    :prefix "SPC"
    "s" 'vlad/cxx-switch-between-header-and-source-files
    "t" 'vlad/cxx-open-unit-test-file)

  (general-def 'normal compilation-mode-map
    "s-n" 'next-error)
  )

(provide 'vlad/packages/general)
;;; vlad/packages/general.el ends here
