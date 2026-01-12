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

    "s-v" (lambda () (interactive) (yank-pop))

    ;; FIXME(vlad): use these: evil's alternatives do not respect visual lines.
    ;;              Also don't forget to change them in `vlad-git.el'
    ;; "C-u" 'scroll-down-command
    ;; "C-d" 'scroll-up-command
    )

  (general-def 'normal '(general-default-keymaps
                         dired-mode-map
                         magit-mode-map
                         magit-file-section-map
                         magit-status-mode-map)
   :prefix "SPC"
   "bc" 'vlad/kill-other-buffers

   "dt" 'org-roam-dailies-goto-today
   "dy" 'org-roam-dailies-goto-yesterday

   "fs" 'grep-find

   "gg" 'magit

   "hf" 'describe-function
   "hk" 'describe-key
   "hm" 'describe-mode
   "ht" 'consult-theme
   "hv" 'describe-variable

   "mc" 'vlad/close-other-tabs
   "mt" 'tab-bar-new-tab

   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "ns" 'org-roam-db-sync
   "nt" 'org-roam-buffer-toggle

   "oc" 'vlad/open-config
   "op" 'vlad/open-plan

   "pv" 'vlad/open-current-directory-in-dired

   "up" 'straight-pull-all

   "w" 'vlad/toggle-visual-line-mode

   "<" 'consult-buffer

   "." 'find-file)

  (general-def 'insert
    "C-c C-i" 'org-roam-node-insert

    "M-v" 'yank
    "s-v" 'yank

    "C-ц" 'evil-delete-backward-word)

  (general-def 'visual
    "M-v" 'yank
    ;; FIXME(vlad): enable this and remove `evil-commentary' dependency.
    ;; "gc" 'comment-or-uncomment-region

    "C-x C-;" 'comment-or-uncomment-region)

  ;; ----- Profiler -----
  (general-def 'normal profiler-report-mode-map
    "TAB" 'profiler-report-toggle-entry)

  (general-def 'normal xref--xref-buffer-mode-map
    "RET" 'xref-show-location-at-point)

  ;; ----- Minibuffer -----
  (general-def 'minibuffer-mode-map
    "M-v" 'yank
    "s-v" 'yank
    "C-w" 'backward-kill-word
    "C-ц" 'backward-kill-word
    "<backspace>" 'vlad/minibuffer-backward-kill
    "DEL" 'vlad/minibuffer-backward-kill)

  ;; ----- Compilation -----
  (general-def 'normal compilation-mode-map
    "s-n" 'next-error)

  ;; ----- Dired -----
  (general-def 'normal dired-mode-map
    "0" 'evil-beginning-of-line
    "$" 'evil-end-of-line

    "-" 'dired-up-directory

    "gg" (lambda () (interactive) (goto-line (point-min)))
    "G" (lambda () (interactive) (goto-line (point-max)))
    "gt" 'tab-bar-switch-to-next-tab
    "gT" 'tab-bar-switch-to-prev-tab

    "w" 'evil-forward-word-begin)

  ;; ----- org-mode -----
  (general-def 'normal org-mode-map
    "TAB" 'org-cycle
    "C-k" 'org-timestamp-up
    "C-j" 'org-timestamp-down)

  (general-def 'normal org-mode-map
   :prefix "SPC"

   ;; FIXME(vlad): resolve this collision.
   ;; "aa" 'org-attach-attach
   "aa" 'org-roam-alias-add
   "ar" 'org-roam-alias-remove

   "c" 'org-ctrl-c-ctrl-c

   "il" 'org-insert-link

   "lt" 'vlad/org-log-current-time

   "nc" 'org-id-get-create
   "nn" 'org-roam-dailies-goto-next-note
   "np" 'org-roam-dailies-goto-previous-note

   "oo" 'org-open-at-point

   "t" 'org-todo)

  (general-def 'visual org-mode-map
    "M-h" 'org-metaleft
    "M-l" 'org-metaright)

  ;; ----- Magit -----
  (general-def 'emacs '(magit-mode-map
                        magit-file-section-map
                        magit-status-mode-map)
    "/" 'evil-ex-search-forward

    "G" 'evil-goto-line
    "gg" 'evil-goto-first-line

    "V" 'evil-visual-screen-line

    "gT" 'tab-bar-switch-to-prev-tab
    "gt" 'evil-tab-next

    "h" 'left-char
    "j" 'magit-next-line
    "k" 'magit-previous-line
    "l" 'right-char

    "n" 'evil-ex-search-next
    "p" 'evil-ex-search-previous

    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up

    "C-w h" 'evil-window-left
    "C-w j" 'evil-window-down
    "C-w k" 'evil-window-up
    "C-w l" 'evil-window-right

    "C-j" 'magit-section-forward
    "C-k" 'magit-section-backward)

  (general-def 'visual '(magit-mode-map
                         magit-file-section-map
                         magit-status-mode-map)
    "s" 'magit-stage
    "u" 'magit-unstage)

  ;; ----- C/C++ -----

  (general-def 'normal '(c-mode-map c++-mode-map)
    :prefix "SPC"
    "dp" 'vlad/gdb-print
    "ds" 'vlad/start-debugger
    "dk" 'vlad/stop-debugger
    "s" 'vlad/cxx-switch-between-header-and-source-files
    "t" 'vlad/cxx-open-unit-test-file)

  (general-def 'normal '(c-mode-map c++-mode-map)
    "<f5>" 'gud-run
    "<f9>" 'vlad/toggle-breakpoint
    "<f10>" 'gud-next
    "<f11>" 'gud-step
    "<f12>" 'gud-finish)

  ;; (general-def 'visual '(c-mode-map c++-mode-map)
  ;;   "=" 'vlad/clang-format-region)

  ;; ----- Projects -----

  (defun vlad/compile ()
    (interactive)
    (call-interactively #'compile))

  (general-def 'normal '(general-default-keymaps dired-mode-map)
    :prefix "SPC"

    "c" 'vlad/compile

    "pa" 'vlad/add-project
    "pc" 'vlad/compile-project
    "pd" 'vlad/remove-project
    "pp" 'vlad/switch-project

    ;; FIXME(vlad): this should be something like projectile-ripgrep.
    ;; "ps" 'projectile-ripgrep
    "ps" 'rgrep

    "SPC" 'vlad/find-project-file
    )

  ;; ----- Compilation -----

  (general-def 'motion '(compilation-mode-map)
   :prefix "SPC"
   "bc" 'vlad/kill-other-buffers

   "dt" 'org-roam-dailies-goto-today
   "dy" 'org-roam-dailies-goto-yesterday

   "fs" 'grep-find

   "gg" 'magit

   "hf" 'describe-function
   "hk" 'describe-key
   "hm" 'describe-mode
   "ht" 'consult-theme
   "hv" 'describe-variable

   "mc" 'vlad/close-other-tabs
   "mt" 'tab-bar-new-tab

   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "ns" 'org-roam-db-sync
   "nt" 'org-roam-buffer-toggle

   "oc" 'vlad/open-config
   "op" 'vlad/open-plan

   "pv" 'vlad/open-current-directory-in-dired

   "up" 'straight-pull-all

   "w" 'vlad/toggle-visual-line-mode

   "<" 'consult-buffer

   "." 'find-file)
  )

(provide 'vlad/packages/general)
;;; vlad/packages/general.el ends here
