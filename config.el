;;; Code:

;; Personal information
(setq user-full-name "Vladislav Sharshukov"
      user-mail-address "vsharshukov@gmail.com")

(defconst xdg/cache-home (getenv "XDG_CACHE_HOME"))
(defconst xdg/data-dir (getenv "XDG_DATA_DIR")) ;; TODO: this is not XDG's

(defconst vlad/org-directory (expand-file-name "org" xdg/data-dir))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq sentence-end-double-space nil)

;; Reference: https://emacs.stackexchange.com/questions/28121/osx-switching-to-virtual-desktop-doesnt-focus-emacs/28296#28296
(menu-bar-mode t)
(global-hl-line-mode 1)

(use-package display-fill-column-indicator
  :config
  (setq-default fill-column 120)
  (global-display-fill-column-indicator-mode))

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

(column-number-mode)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(use-package consult
  :straight t
  :diminish)

(use-package vertico
  :straight t
  :config
  (vertico-mode)
  (setq vertico-count 20))

(defun vlad/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      (vertico-directory-up)
    ;; ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
    ;; (if (string-match-p "/." (minibuffer-contents))
    ;;     (zap-up-to-char (- arg) ?/)
    ;;   (delete-minibuffer-contents))
    (delete-backward-char arg)))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :commands marginalia-mode
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hideshow
  :straight t
  :diminish
  :hook
  (c++-mode . hs-minor-mode)
  :config
  (add-hook 'hs-minor-mode-hook
	    (lambda () (diminish 'hs-minor-mode))))

;; (display-battery-mode 1)
;; (setq-default display-time-24hr-format t
;;               display-time-day-and-date t)
;; (display-time-mode 1)

(setq mac-option-modifier 'meta)

(setq-default delete-by-moving-to-trash nil
              magit-delete-by-moving-to-trash nil)

(setq ring-bell-function 'ignore)

;; Indentation settings
(setq standard-indent 2)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq-default standard-indent 4
                          c-basic-offset 4)))

(setq-default indent-tabs-mode nil)

(vlad/load-config-file "ya.el")

;; --- themes ---

(use-package ef-themes
  :straight t)

(load-theme 'ef-night t)

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :defer t
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil))

(use-package doom-themes
  :straight t
  :defer t
  :config
  (setq doom-themes-enable-italic nil))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package nerd-icons
  :straight t
  :if (display-graphic-p))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-nil)
  (doom-modeline-check-simple-format t)
  (doom-modeline-check-icon t)
  (doom-modeline-major-mode-icon nil)
  :config
  (doom-modeline-def-modeline 'vlad/main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info process vcs major-mode check time))
  (defun vlad/setup-doom-modeline ()
    (doom-modeline-set-modeline 'vlad/main 'default))

  (add-hook 'doom-modeline-mode-hook 'vlad/setup-doom-modeline)
  )

(set-frame-font "Iosevka Nerd Font 18" nil t)
(set-face-italic 'italic nil) ;; Looks bad

(use-package ligature
  :straight t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (setq-default truncate-lines t)
(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search)
  (global-visual-line-mode)
  :config
  (evil-mode)

  (evil-define-operator vlad/clang-format-region (beg end)
    :move-point nil
    :type line
    (evil-ensure-column
      (save-restriction
        (goto-char beg)
        (clang-format-region beg end)
        (evil-force-normal-state))))
  )

(use-package evil-anzu
  :after evil
  :straight t
  :hook (evil-mode-hook . global-anzu-mode)
  :config
  (with-eval-after-load 'evil
    (require 'evil-anzu)))

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
    "C-w" 'backward-kill-word
    "C-ц" 'backward-kill-word
    "<backspace>" 'vlad/minibuffer-backward-kill
    )

  (general-def 'insert
    "M-v" 'yank
    "C-ц" 'evil-delete-backward-word)

  (general-def 'insert web-mode-map
    "TAB" 'hippie-expand)

  (general-def 'normal
   :prefix "SPC"
   "dt" 'org-roam-dailies-goto-today
   "dy" 'org-roam-dailies-goto-yesterday

   "e" 'elfeed

   "fs" 'grep-find

   "gg" 'magit

   "hf" 'describe-function
   "hk" 'describe-key
   "hv" 'describe-variable
   "hm" 'describe-mode

   "lh" 'lsp-inlay-hints-mode

   "mt" 'tab-bar-move-window-to-tab

   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "nt" 'org-roam-buffer-toggle

   "oa" 'org-agenda

   "oc" 'vlad/open-config

   "op" 'vlad/open-plan
   "ot" 'vlad/open-todo

   "pa" 'projectile-add-known-project
   "pc" 'projectile-compile-project
   "pd" 'projectile-remove-known-project
   "pi" 'projectile-invalidate-cache
   "pp" 'projectile-switch-project
   "pv" (lambda () (interactive) (dired "."))

   "ro" 'citar-open

   "ht" 'consult-theme

   "up" 'straight-pull-all

   "ya" 'ya/open-in-arcadia
   "ycf" 'ya/copy-file-name
   "ycc" (lambda () (interactive) (ya/copy-file-name :as-c++-include t))
   "ycp" (lambda () (interactive) (ya/copy-file-name :as-python-import-from t))
   "ycP" (lambda () (interactive) (ya/copy-file-name :as-python-import t))
   "ytf" (lambda () (interactive) (ya/for-each-test 'toggle))
   "ytc" (lambda () (interactive) (ya/for-each-test 'close))
   "yto" (lambda () (interactive) (ya/for-each-test 'open))

   "<" 'switch-to-buffer

   "SPC" 'projectile-find-file)

  (general-def 'visual
    "M-v" 'yank)

  (general-def 'normal
    "C-S-j" 'vlad/font-dec
    "C-О" 'vlad/font-dec
    "C-S-k" 'vlad/font-inc
    "C-Л" 'vlad/font-inc
    "M-v" 'yank
    "M-n" 'evil-buffer-new

    "]g" 'flycheck-next-error
    "[g" 'flycheck-previous-error)

  (general-def 'normal profiler-report-mode-map
    "TAB" 'profiler-report-toggle-entry)

  (general-def 'normal xref--xref-buffer-mode-map
    "RET" 'xref-show-location-at-point)

  (general-def 'normal org-mode-map
    "TAB" 'org-cycle)

  (general-def 'normal org-mode-map
    :prefix "SPC"
    "ri" 'citar-insert-citation)

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
    "dt" 'org-roam-dailies-goto-today

    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable
    "hm" 'describe-mode

    "mt" 'tab-bar-move-window-to-tab

    "oc" 'vlad/open-config

    "pa" 'projectile-add-known-project
    "pd" 'projectile-remove-known-project
    "pf" 'projectile-find-file
    "pi" 'projectile-invalidate-cache
    "pp" 'projectile-switch-project

    "<" 'switch-to-buffer

    "f" 'find-file
    "." 'find-file
    "SPC" 'projectile-find-file)

  (general-def 'normal c++-mode-map
    :prefix "SPC"
    "s" 'lsp-clangd-find-other-file)

  (general-def 'normal lsp-mode-map
    "K" 'lsp-describe-thing-at-point)
  (general-def 'normal lsp-mode-map
    :prefix "SPC"
    "rf" 'lsp-find-references
    "ca" 'lsp-execute-code-action
    "cr" 'lsp-rename)

  (general-def 'normal c++-mode-map
    "=" 'vlad/clang-format-region)
  (general-def 'visual c++-mode-map
    "=" 'vlad/clang-format-region)

  (general-def 'normal rust-mode-map
    :prefix "SPC"
    "rc" 'lsp-rust-analyzer-open-cargo-toml)

  (general-def 'insert web-mode-map
    "TAB" 'emmet-expand-line
    "C-j" 'emmet-next-edit-point
    "C-о" 'emmet-next-edit-point
    "C-k" 'emmet-prev-edit-point
    "C-л" 'emmet-prev-edit-point))

(setq dired-listing-switches "-alh")

(use-package projectile
  :straight t
  :diminish
  :config
  (projectile-mode)
  (setq projectile-indexing-method 'alien) ;; To be explicit
  (setq projectile-generic-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git")
  (setq projectile-track-known-projects-automatically nil)
  (add-to-list 'projectile-project-root-files "ya.make")
  (setq projectile-switch-project-action 'projectile-dired)
  ;; (setq projectile-globally-ignored-directories '(".cache/clangd" "node_modules" ".next" "build")) ;; NOTE: does not work with `alien' indexing method
  )

(use-package which-key
  :straight t
  :diminish
  :init
  (which-key-mode))

;; --- git ---
(use-package magit
  :straight t
  :init

  :general-config
  (general-def 'emacs '(magit-mode-map magit-file-section-map magit-status-mode-map)
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

  (general-def 'visual '(magit-mode-map magit-file-section-map magit-status-mode-map)
    "s" 'magit-stage)

  (general-def 'normal '(magit-mode-map magit-file-section-map magit-status-mode-map)
    :prefix "SPC"
    "h f" 'describe-function
    "h k" 'describe-key
    "h m" 'describe-mode
    "h v" 'describe-variable)
  )

(use-package protobuf-mode
  :straight t)

;; --- tree-sitter ---

(use-package tree-sitter
  :straight t
  :diminish
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :straight t)

(setq-default treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    ;; (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    ;; (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    ;; (go "https://github.com/tree-sitter/tree-sitter-go")
    ;; (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    ;; (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    ;; (lua "https://github.com/Azganoth/tree-sitter-lua")
    ;; (make "https://github.com/alemuller/tree-sitter-make")
    ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    ;; (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         "master"
         "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                "master"
                "typescript/src")
    ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    ))

(add-to-list 'major-mode-remap-alist `(json-mode . json-ts-mode))
(use-package json-ts-mode
  :defer t
  :config
  (add-hook 'json-ts-mode-hook (lambda () (setq standard-indent 2)))
  )

(use-package typescript-ts-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook 'typescript-ts-mode-hook #'lsp)
  (add-hook 'tsx-ts-mode-hook #'lsp)
  )

(use-package cmake-ts-mode
  :defer t
  :mode ("\\.cmake\\'" . cmake-ts-mode)
  )

(setq major-mode-remap-alist
      '((json-mode . json-ts-mode)))

;; --- Lsp ---
(use-package lsp-mode
  :straight t
  :custom
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)

  :config
  (setq lsp-semantic-tokens-enable t
	lsp-inlay-hint-enable nil
	lsp-lens-enable t
	lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-enable-file-watchers nil) ;; TODO(arealhero): disable watchers for pyright only

  (setq lsp-clients-clangd-args
        '("-j=6"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"))

  (setq lsp-pylsp-plugins-flake8-max-line-length nil)

  (add-hook 'c++-mode-hook
	    (lambda ()
	      (tree-sitter-hl-mode)
	      (lsp)))

  (add-hook 'rust-mode-hook 'lsp-deferred)
  )

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
			  (tree-sitter-hl-mode)
                          (lsp))))  ; or lsp-deferred

(use-package company
  :straight t
  :diminish
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay
	(lambda () (if (company-in-string-or-comment) nil 0.2)))
  (setq company-global-modes '(not erc-mode message-mode eshell-mode)))

(use-package flycheck
  :straight t
  :config
  :hook
  (after-init . global-flycheck-mode))

(use-package lsp-ui
  :after lsp-mode
  :straight t
  :custom
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-update-mode 'line))

;; --- C++ ---
(use-package clang-format
  :straight t
  :config
  (setq clang-format-style "file"))

;; --- Misc ---
(electric-pair-mode 1)
;; (setq electric-pair-preserve-balance nil)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; See https://libredd.it/r/emacs/comments/l42oep/comment/gkmnh3y/
(setq-default comp-async-report-warnings-errors nil)
(setq-default native-comp-async-report-warnings-errors nil)

(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000)

(setq confirm-kill-emacs nil)

;; (global-display-line-numbers-mode 1)
;; See https://libreddit.tiekoetter.com/r/emacs/comments/8pfdlb/weird_shifting_problem_with_new_emacs_line_numbers/
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width-start t)

;; By default Emacs creates backup files (e.g. test.c~) and autosave
;; files (e.g. #test.c#) in the same directory the file belongs to.
;; The next code block moves those files to the separate directory.
(defconst vlad/emacs-cache-dir (expand-file-name "emacs" xdg/cache-home))
(defconst vlad/emacs-lock-files-dir (expand-file-name "lock-files" vlad/emacs-cache-dir))
(make-directory vlad/emacs-cache-dir t)
(make-directory vlad/emacs-lock-files-dir t)

(setq backup-directory-alist
      `((".*" . ,vlad/emacs-cache-dir)))

(setq auto-save-file-name-transforms `((".*" ,vlad/emacs-cache-dir t))
      auto-save-list-file-prefix vlad/emacs-cache-dir
      lock-file-name-transforms `((".*" ,vlad/emacs-lock-files-dir t)))

(use-package evil-commentary
  :straight t
  :diminish
  :hook
  (after-init . evil-commentary-mode))

(use-package org
  :defer t
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)

  :commands (org-previous-visible-heading org-set-property)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "IN_ASSESSMENT(a)" "IN_PROGRESS(p)" "HABIT(h)" "|" "DONE(d)" "WONTFIX(w)" "DROPPED(D)" "BACKLOG(b)")))

  :general-config
  (general-def 'normal org-mode-map
   :prefix "SPC"
   "aa" 'org-attach-attach
   "c" 'org-ctrl-c-ctrl-c
   "il" 'org-insert-link
   "lt" 'vlad/org-log-current-time
   "oo" 'org-open-at-point
   "t" 'org-todo)

  (general-def 'normal org-mode-map
   "C-k" 'org-timestamp-up
   "C-j" 'org-timestamp-down)

  :config
  (defun vlad/get-current-time-string ()
    "Get current time as string."
    (interactive)
    (format-time-string "%H:%M:%S" (current-time)))

  (defun vlad/org-log-current-time ()
    "Add current time to the nearest header."
    (interactive)
    (let ((saved-position (point)))
      (org-previous-visible-heading 1)
      (org-set-property "TIME" (vlad/get-current-time-string))
      (goto-char saved-position)))

  ;;; Ricing org agenda
  ;; Set span for agenda to be just daily
  (setq org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t)

  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-current-time-string "")
  (setq org-agenda-time-grid '((daily) () "" ""))

  (setq org-agenda-hide-tags-regexp ".*")

  (setq org-agenda-prefix-format '(
                                   (agenda . "  %?-2i %t ")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")
                                   ))
  (if (display-graphic-p)
      (setq org-agenda-category-icon-alist
            `(("university" ,(list (all-the-icons-faicon "university" :height 0.8 :v-adjust 0)) nil nil :ascent center)
              ("work" ,(list (all-the-icons-material "work" :height 0.9)) nil nil :ascent center)
              ("science" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8 :v-adjust 0)) nil nil :ascent center)
              ("personal" ,(list (all-the-icons-material "person" :height 0.9)) nil nil :ascent center)
              ("birthday" ,(list (all-the-icons-faicon "birthday-cake" :height 0.8 :v-adjust 0)) nil nil :ascent center)
              ("ricing" ,(list (all-the-icons-faicon "cog" :height 0.8 :v-adjust 0)) nil nil :ascent center)
              ))
    )

  (defun vlad/sync-org-agenda-files ()
    "Syncronize org-agenda-files."
    (interactive)
    (setq org-agenda-files (directory-files-recursively vlad/org-directory org-agenda-file-regexp)))

  (setq org-directory vlad/org-directory
        org-attach-use-inheritance t
        org-hide-emphasis-markers nil
        org-startup-indented t
        org-confirm-babel-evaluate nil
        org-deadline-warning-days 0)

  (add-hook 'after-init-hook 'vlad/sync-org-agenda-files)
  (add-hook 'org-roam-mode-hook 'vlad/sync-org-agenda-files)

  ;; PDFs visited in Org-mode are opened in zathura (and not in the default choice).
  ;; See https://stackoverflow.com/a/8836108/789593
  (add-hook 'org-mode-hook
	    #'(lambda ()
	        (delete '("\\.pdf\\'" . default) org-file-apps)
	        (delete '("\\.djvu\\'" . default) org-file-apps)
	        (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
	        (add-to-list 'org-file-apps '("\\.djvu\\'" . "zathura %s"))

	        (delete '("\\.webm\\'" . default) org-file-apps)
	        (delete '("\\.mkv\\'" . default) org-file-apps)
	        (add-to-list 'org-file-apps '("\\.webm\\'" . "mpv --speed=1.75 --fs %s"))
	        (add-to-list 'org-file-apps '("\\.mkv\\'" . "mpv --speed=1.75 --fs %s"))))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq fill-column 80)))

  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  (setq org-latex-packages-alist '())
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t))

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live nil)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)
  )

(use-package org-modern
  :straight t
  :config
  (setq org-modern-fold-stars
        '(("▶" . "▼")
          ("▷" . "▽")
          ("⯈" . "⯆")
          ("▷" . "▽")
          ("▹" . "▿")
          ("▸" . "▾")))

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  )

(use-package org-super-agenda
  :straight (:type git :host github :repo "alphapapa/org-super-agenda")
  :hook (after-init . org-super-agenda-mode)
  :config
  (defconst vlad/org-agenda-overdue-header (concat (all-the-icons-faicon "exclamation" :height 0.8) " Overdue "))
  (defconst vlad/org-agenda-today-header "Today")

  (defun vlad/org-agenda-generate-group (header category)
    `((:name ,(concat (all-the-icons-faicon "exclamation") " " header " ── Deadline")
            :and (:deadline today :category ,category :not (:tag "event"))
            :order 2
            :face 'error)

      (:name ,header
            :and (:category ,category :not (:tag "event"))
            :order 10))
    )

  (setq org-super-agenda-groups
        `((:name ,vlad/org-agenda-overdue-header
                 :scheduled past
                 :order 2
                 :face 'error)

          (:name "Habits"
                 :and (:tag "habit")
                 :order 100)

          ,@(vlad/org-agenda-generate-group "Birthdays" "birthday")
          ,@(vlad/org-agenda-generate-group "Science" "science")
          ,@(vlad/org-agenda-generate-group "Work" "work")
          ,@(vlad/org-agenda-generate-group "University" "university")
          ,@(vlad/org-agenda-generate-group "Personal" "personal")
          ,@(vlad/org-agenda-generate-group "Ricing" "ricing")

          (:name ,vlad/org-agenda-today-header
                 :time-grid t
                 :date today
                 :scheduled today
                 :order 1)))
  )

(use-package elfeed
  :straight t
  :general-config
  (general-def 'normal elfeed-search-mode-map
    "RET" 'elfeed-search-show-entry
    "U" 'elfeed-update
    "u" 'vlad/elfeed-search-toggle-unread
    "q" 'elfeed-search-quit-window
    "s" 'elfeed-search-set-filter)

  (general-def 'normal elfeed-show-mode-map
    "n" 'elfeed-show-next
    "p" 'elfeed-show-prev
    "u" 'vlad/elfeed-show-unread-current-entry
    "q" 'elfeed-search-quit-window)

  :config
  (defconst vlad/elfeed-news-filter "+unread +news")
  (defconst vlad/elfeed-emacs-filter "+unread +emacs")

  (setq elfeed-search-remain-on-entry t
        elfeed-sort-order 'ascending
        elfeed-search-filter vlad/elfeed-news-filter)

  (add-hook 'elfeed-search-mode-hook 'elfeed-update)
  (add-hook 'elfeed-show-mode-hook #'(lambda ()
                                       (setq fill-column 80
                                             shr-max-width 80)))

  (defun vlad/elfeed-search-remove-tag (&optional tag)
    (interactive)
    (let ((entry (elfeed-search-selected :ignore-region)))
      (unless entry
        (error "No entry selected!"))

      (unless tag
        (setq tag (completing-read "Tag: " (elfeed-entry-tags entry))))

      (elfeed-untag entry tag)
      ;; (if (elfeed-tagged-p 'unread entry)
      ;;     (elfeed-untag entry 'unread)
      ;;   (elfeed-tag entry 'unread))
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))))

  (defun vlad/elfeed-search-add-tag (tag)
    (interactive "Tag: ")
    (let ((entry (elfeed-search-selected :ignore-region)))
      (unless entry
        (error "No entry selected!"))
      (elfeed-tag entry tag)
      ;; (if (elfeed-tagged-p 'unread entry)
      ;;     (elfeed-untag entry 'unread)
      ;;   (elfeed-tag entry 'unread))
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))))

  (defun vlad/elfeed-search-toggle-unread ()
    (interactive)
    (let ((entry (elfeed-search-selected :ignore-region)))
      (unless entry
        (error "No entry selected!"))
      (if (elfeed-tagged-p 'unread entry)
          (elfeed-untag entry 'unread)
        (elfeed-tag entry 'unread))
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))))

  (defun vlad/elfeed-show-unread-current-entry ()
    "Quit elfeed-show mode and mark current entry as unread.
Note that this function requires that `elfeed-search-remain-on-entry' is not nil."
    (interactive)
    (elfeed-search-quit-window)
    (vlad/elfeed-search-toggle-unread))
  )

(use-package elfeed-org
  :after (elfeed org)
  :straight t
  :config
  (elfeed-org))

(use-package org-roam
  :straight t

  :custom
  (org-roam-directory vlad/org-directory)
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d>\n"))))

  (org-roam-capture-templates
   `(("s" "science" plain "%?"
      :if-new
      (file+head "science/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: ${title}\n:END:\n#+TITLE: ${title}\n#+CREATED: %U\n#+FILETAGS: :science:\n\n* Источник\n- ")
      :immediate-finish t
      :unnarrowed t)

     ("m" "Main")
     ("mg" "General note" plain "%?"
      :if-new
      (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: ${title}\n:END:\n#+TITLE: ${title}\n#+CREATED: %U\n#+FILETAGS: :main:\n")
      :immediate-finish t
      :unnarrowed t)

     ("mp" "Person" plain "%?"
      :if-new
      (file+head "main/person/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: ${title}\n:END:\n#+TITLE: ${title}\n#+CREATED: %U\n#+FILETAGS: :person:\n")
      :immediate-finish t
      :unnarrowed t)

     ("r" "reference" plain "%?"
      :if-new
      (file+head "references/${citar-citekey}.org"
                 ":PROPERTIES:\n:CATEGORY: [${citar-citekey}] ${note-title}\n:END:\n#+TITLE: [${citar-citekey}] ${note-title}\n#+CREATED: %U\n#+FILETAGS: :reference:\n")
      :immediate-finish t
      :unnarrowed t)

     ("y" "Yandex")

     ("yn" "Note" plain "%?"
      :if-new
      (file+head "yandex/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: Yandex - ${title}\n:END:\n#+TITLE: Yandex - ${title}\n#+CREATED: %U\n#+FILETAGS: :yandex:nda:\n")
      :immediate-finish t
      :unnarrowed t)
     ("yt" "Ticket" plain "%?"
      :if-new
      (file+head "yandex/tickets/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: Yandex - ${title}\n:END:\n#+TITLE: Yandex - ${title}\n#+CREATED: %U\n#+FILETAGS: :yandex:nda:\n\nТрекер: [[https://st.yandex-team.ru/${title}]]")
      :immediate-finish t
      :unnarrowed t)
     ("yp" "person" plain "%?"
      :if-new
      (file+head "yandex/person/%<%Y%m%d%H%M%S>-${slug}.org"
                 ":PROPERTIES:\n:CATEGORY: Yandex - ${title}\n:END:\n#+TITLE: Yandex - ${title}\n#+CREATED: %U\n#+FILETAGS: :yandex:nda:person:\n\n* Контакты\n- Стафф\n- Telegram")
      :immediate-finish t
      :unnarrowed t)
     ))

  :config
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

  :general-config
  (general-def 'normal org-mode-map
   :prefix "SPC"
   "nn" 'org-roam-dailies-goto-next-note
   "np" 'org-roam-dailies-goto-previous-note))

(use-package citar
  :straight t
  :custom
  (org-cite-global-bibliography '("~/data/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/data/references.bib"))
  (citar-notes-paths '("~/data/references"))
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  "))

(use-package citar-org-roam
  :straight t
  :after (citar org-roam)
  :diminish
  :custom
  (citar-org-roam-capture-template-key "r")
  (citar-org-roam-note-title-template "${author} - ${title}")
  :config (citar-org-roam-mode))

;; https://github.com/fxbois/web-mode
(use-package web-mode
  :straight t
  :defer t
  :init
  (defun vlad/web-mode-setup ()
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))
  :hook
  (after-init . vlad/web-mode-setup)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  )

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :straight t
  :defer t
  ;; :hook (web-mode . emmet-mode)
  )

(use-package yasnippet
  :straight t
  :diminish
  :config
  (yas-reload-all)
  (yas-global-mode)
  (diminish 'yas-minor-mode))

(use-package hippie-exp
  :straight t
  :after emmet-mode
  :defer t
  :config
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))

(use-package yaml-mode
  :straight t
  :defer t)

(use-package undo-tree
  :after evil
  :straight t
  :diminish
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)

  (setq undo-tree-history-directory-alist
        `((".*" . ,vlad/emacs-cache-dir)))
  )

(use-package olivetti
  :straight (:type git :host github :repo "rnkn/olivetti")
  :hook (org-agenda-finalize-hook . olivetti-mode)
  )

(provide 'config)
;;; config.el ends here
