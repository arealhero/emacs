;;; config.el --- My config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (locate-user-emacs-file "custom.el"))

(when window-system
  (menu-bar-mode -1)

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(blink-cursor-mode -1)

(defun darwin-system-p ()
  "Return non-nil if Emacs was built for a Darwin system (macOS)."
  (eq system-type 'darwin))

;; NOTE: pixelwise resizing of windows and frames.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(defun display-startup-echo-area-message ()
  "Hide advertisement from minibuffer."
  (message ""))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; Personal information
(setq user-full-name "Vladislav Sharshukov"
      user-mail-address "vsharshukov@gmail.com")

(defconst xdg/data-dir (getenv "XDG_DATA_DIR")) ;; TODO: this is not XDG's

(defun vlad/get-cache-file (filename)
  "Get `FILENAME' in Emacs' cache directory."
  (expand-file-name filename vlad/emacs-cache-dir))

(defun vlad/get-cache-dir (dir)
  "Get `DIR' in Emacs' cache directory."
  (file-name-as-directory (vlad/get-cache-file dir)))

(defconst vlad/package-cache-dir (vlad/get-cache-file "package-cache"))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq sentence-end-double-space nil)

(global-hl-line-mode 1)

(require 'display-fill-column-indicator)
(setq-default fill-column 120)
(global-display-fill-column-indicator-mode)

(column-number-mode)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(use-package diminish
  :straight t)

(use-package consult
  :straight t
  :diminish)

;; FIXME(vlad): use ido?
(use-package vertico
  :straight t
  :config
  (vertico-mode t)

  (setq vertico-count 20)

  (defun vlad/minibuffer-backward-kill (arg)
    "Ivy-like behavior for completing file-name with vertico.
When minibuffer is completing a file name delete up to parent folder,
otherwise delete ARG characters backward."
    (interactive "p")
    (if (and minibuffer-completing-file-name (string-suffix-p "/" (minibuffer-contents)))
        (vertico-directory-up)
      (delete-char -1 arg)))

  (keymap-set minibuffer-mode-map "DEL" 'vlad/minibuffer-backward-kill))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package marginalia
;;   :straight t
;;   :commands marginalia-mode
;;   :config
;;   (marginalia-mode))

(use-package savehist
  :config
  (savehist-mode)

  (setq savehist-file (expand-file-name "savehist-history" vlad/package-cache-dir)))

(require 'vlad-tree-sitter)

;; @ref: https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow/603#603
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

(add-hook 'c-ts-mode-hook 'hs-minor-mode)
(add-hook 'c++-ts-mode-hook 'hs-minor-mode)

(add-hook 'hs-minor-mode-hook (lambda () (interactive) (diminish 'hs-minor-mode)))
;; (use-package hideshow
;;   :straight t
;;   :diminish
;;   :hook
;;   (c++-ts-mode . hs-minor-mode)
;;   (c-ts-mode . hs-minor-mode))

(when (darwin-system-p)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

(setq delete-by-moving-to-trash nil)
(setq ring-bell-function 'ignore)

(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10)

(setq confirm-kill-emacs nil)

;; By default Emacs creates backup files (e.g. test.c~) and autosave
;; files (e.g. #test.c#) in the same directory the file belongs to.
;; The next code block moves those files to the separate directory.
(defconst vlad/emacs-lock-files-dir (vlad/get-cache-dir "lock-files"))
(defconst vlad/emacs-backup-dir (vlad/get-cache-dir "backups"))

(setq backup-directory-alist
      `((".*" . ,vlad/emacs-backup-dir)))

(setq auto-save-file-name-transforms `((".*" ,vlad/emacs-backup-dir t))
      auto-save-list-file-prefix vlad/emacs-backup-dir
      lock-file-name-transforms `((".*" ,vlad/emacs-lock-files-dir t)))

(setq bookmark-default-file (vlad/get-cache-dir "bookmarks"))

;; Indentation settings
(setq standard-indent 4)

(setq-default indent-tabs-mode nil)

;; --- Theme customization ---

(load-theme 'modus-vivendi-tinted t)
;; (set-frame-font "Fira Code 18" nil t)
(set-frame-font "Hack 18" nil t)
(set-face-italic 'italic nil) ;; Looks bad IMO

;; (setq-default truncate-lines t)

(global-visual-line-mode)
(diminish 'visual-line-mode)

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search)

  ;; NOTE(vlad): make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  :config
  (evil-mode)

  (evil-define-operator vlad/clang-format-region (beg end)
    :move-point nil
    :type line
    (evil-ensure-column
      (save-restriction
        (goto-char beg)
        (clang-format-region beg end)
        (evil-force-normal-state)))))

(use-package undo-tree
  :after evil
  :straight t
  :diminish
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)

  (defconst vlad/undo-tree-cache-dir (vlad/get-cache-dir "undo-tree"))
  (setq undo-tree-history-directory-alist
        `((".*" . ,vlad/undo-tree-cache-dir))))

;; (use-package evil-anzu
;;   :after evil
;;   :straight t
;;   :config
;;   (global-anzu-mode))

;; FIXME(vlad): use `comment-line' instead. I don't use motions when commenting.
;;              Although it does not work well with visual mode (like `magit-stash').
(use-package evil-commentary
  :straight t
  :diminish
  :hook
  (after-init . evil-commentary-mode))

;; --- C++ ---
(use-package clang-format
  :straight t
  :defer t
  :config
  (setq clang-format-style "file"))

;; FIXME(vlad): support buffer names like `file.c<folder>'
(defun vlad/c-switch-between-header-and-source-files ()
  "Switch between header and source C files."
  (interactive)
  (let* ((current-file (buffer-name))
         (other-file (if (string-equal (file-name-extension current-file) "c")
                         (file-name-with-extension current-file "h")
                       (file-name-with-extension current-file "c"))))
    (if (file-exists-p other-file)
        (find-file other-file)
      (message "File '%s' does not exist" other-file))))

(require 'dired)
(setq dired-listing-switches "-alhv --group-directories-first")

(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'auto-revert-mode-hook (lambda () (interactive) (diminish 'auto-revert-mode)))

;; --- utils ---

(defun vlad/open-config ()
  (interactive)
  (find-file (expand-file-name "config.el" user-emacs-directory)))

(defun vlad/font-adjust (increment)
  (let ((inhibit-message t))
    (global-text-scale-adjust increment)))

(defun vlad/font-inc ()
  (interactive)
  (vlad/font-adjust +2))

(defun vlad/font-dec ()
  (interactive)
  (vlad/font-adjust -2))

(defun vlad/kill-other-buffers ()
  (interactive)
  (when tab-bar-mode
    (tab-bar-close-other-tabs)
    (tab-bar-mode -1))
  (let* ((killable-major-modes '(dired-mode
                                 ibuffer-mode
                                 help-mode
                                 magit-status-mode
                                 magit-process-mode
                                 magit-diff-mode))
         (killable-buffer-p (lambda (buffer)
                              (or (buffer-file-name buffer)
                                  (memq (buffer-local-value 'major-mode buffer)
                                        killable-major-modes)))))
    (mapc 'kill-buffer
          (delq (current-buffer)
                (cl-remove-if-not killable-buffer-p (buffer-list))))
    (delete-other-windows)))

;; FIXME(vlad): move all general's bindings to a separate file.
;; Documentation: https://github.com/noctuid/general.el
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

  (general-def 'insert
    "M-v" 'yank
    "s-v" 'yank
    "C-ц" 'evil-delete-backward-word)

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

   "oc" 'vlad/open-config

   "pv" (lambda () (interactive) (dired "."))

   ;; FIXME(vlad): modify `consult-themes' to remove Emacs' junk like `light-blue' and whatnot.
   "ht" 'consult-theme

   "up" 'straight-pull-all

   "<" 'consult-buffer

   "." 'find-file)

  (general-def 'visual
    "M-v" 'yank)

  (general-def 'normal
    "s-n" 'next-error
    "s-p" 'previous-error

    ;; FIXME(vlad): don't call `execute-extended-command' if one was already called
    "s-x" 'execute-extended-command

    "s-!" 'shell-command
    "s-&" 'async-shell-command

    "C-x C-b" 'ibuffer
    "C-x c" 'execute-extended-command

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

    "]g" 'next-error
    "[g" 'previous-error
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

    "mt" 'tab-bar-move-window-to-tab

    "oc" 'vlad/open-config

    "<" 'consult-buffer

    "." 'find-file)

  (general-def 'normal '(c-ts-mode-map c++-ts-mode-map)
    :mode 'normal
    "=" 'vlad/clang-format-region

    :mode 'visual
    "=" 'vlad/clang-format-region
    )

  ;; (general-def 'visual '(c-ts-mode-map c++-ts-mode-map)
  ;;   "=" 'vlad/clang-format-region)

  (general-def 'normal c-ts-mode-map
    :prefix "SPC"
    "s" 'vlad/c-switch-between-header-and-source-files)

  (general-def 'normal compilation-mode-map
    "s-n" 'next-error)
  )

(require 'vlad-projects)
(require 'vlad-git)

;; (use-package projectile
;;   :straight t
;;   :diminish
;;   :config
;;   (projectile-mode)
;;   (setq projectile-indexing-method 'alien ;; To be explicit
;;         projectile-generic-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git"
;;         projectile-track-known-projects-automatically nil
;;         projectile-switch-project-action 'projectile-dired)

;;   (defconst vlad/projectile-cache-dir (vlad/get-cache-dir "projectile"))
;;   (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" vlad/projectile-cache-dir))

;;   ;; NOTE(vlad): projectile unconditionally searches git submodules. This tricks projectile to run /usr/bin/true,
;;   ;;             essentially returning empty submodules list.
;;   (setq projectile-git-submodule-command "true"))

(use-package company
  :straight t
  :diminish
  :hook
  (after-init . global-company-mode)
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
  )

;; FIXME(vlad): enable flycheck for some modes like `emacs-lisp'?
;; (use-package flycheck
;;   :straight t
;;   :config
;;   :hook
;;   (after-init . global-flycheck-mode))

;; --- Misc ---
(electric-pair-mode 1)
;; (setq electric-pair-preserve-balance nil)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; (global-display-line-numbers-mode 1)
;; See https://libreddit.tiekoetter.com/r/emacs/comments/8pfdlb/weird_shifting_problem_with_new_emacs_line_numbers/
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width-start t)

(global-eldoc-mode -1)

;; NOTE(vlad): this package is used to highlight tabs and trailing spaces.
(require 'whitespace)
(diminish 'whitespace-mode)
(setq whitespace-style '(face
                         tabs
                         trailing
                         space-before-tab
                         indentation
                         space-after-tab
                         tab-mark
                         missing-newline-at-eof))
(global-whitespace-mode)

(provide 'config)
;;; config.el ends here
