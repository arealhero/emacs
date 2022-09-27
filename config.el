;; Personal information
(setq user-full-name "Vladislav Sharshukov"
      user-mail-address "vsharshukov@gmail.com")

(defconst xdg/cache-home (getenv "XDG_CACHE_HOME"))
(defconst xdg/data-dir (getenv "XDG_DATA_DIR")) ;; TODO: this is not XDG's

(defconst vlad/org-directory (expand-file-name "org" xdg/data-dir))

(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-font "Fira Code 11" nil t)
(load-theme 'modus-vivendi)

(use-package writeroom-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package general
  :ensure t
  :config

  (general-evil-setup)

  (general-nmap
   :prefix "SPC"
   "dt" 'org-roam-dailies-goto-today

   "ff" 'vlad/run-fzf
   "fs" 'grep-find

   "gb" 'ivy-switch-buffer

   "hf" 'describe-function
   "hk" 'describe-key
   "hv" 'describe-variable

   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "nt" 'org-roam-buffer-toggle

   "oc" 'vlad/open-config
   "od" 'vlad/open-diary
   "op" 'vlad/open-plan
   "ot" 'vlad/open-todo

   "st" 'vlad/display-current-time

   ;; n stands for NerdTREE - vim legacy :^)
   "ne" 'treemacs-edit-workspaces
   "ns" 'treemacs-switch-workspace
   "nn" 'treemacs

   "up" 'vlad/update-packages

   "zt" 'writeroom-toggle-mode-line
   "zz" 'writeroom-mode)

  (general-def 'normal
    "C-S-j" 'vlad/font-dec
    "C-О" 'vlad/font-dec
    "C-S-k" 'vlad/font-inc
    "C-Л" 'vlad/font-inc)

  (general-def 'normal org-mode-map
    "TAB" 'org-cycle)

  (general-def 'insert web-mode-map
    "TAB" 'emmet-expand-line
    "C-j" 'emmet-next-edit-point
    "C-о" 'emmet-next-edit-point
    "C-k" 'emmet-prev-edit-point
    "C-л" 'emmet-prev-edit-point)

  (general-nmap org-mode-map
    :prefix "SPC"

    "aa" 'org-attach-attach
    "il" 'org-insert-link
    "oo" 'org-open-at-point
    "t" 'org-todo))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

;; --- treemacs ---
(use-package treemacs
  :ensure t)

(use-package projectile
  :ensure t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; --- git ---
(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; --- C++ ---
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style "file"))

(with-eval-after-load 'cc-mode
  (fset 'c-indent-region 'clang-format-region))

;; --- Misc ---
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; See https://libredd.it/r/emacs/comments/l42oep/comment/gkmnh3y/
(setq comp-async-report-warnings-errors nil)
(setq native-comp-async-report-warnings-errors nil)

(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000)

(setq confirm-kill-emacs nil)

;; (global-display-line-numbers-mode 1)
;; See https://libreddit.tiekoetter.com/r/emacs/comments/8pfdlb/weird_shifting_problem_with_new_emacs_line_numbers/
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; By default Emacs creates backup files (e.g. test.c~) and autosave
;; files (e.g. #test.c#) in the same directory the file belongs to.
;; The next code block moves those files to the separate directory.
(defconst emacs-cache-dir (expand-file-name "emacs" xdg/cache-home))
(make-directory emacs-cache-dir t)

(setq backup-directory-alist
      `((".*" . ,emacs-cache-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-cache-dir t)))
(setq auto-save-list-file-prefix
      emacs-cache-dir)

(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-cache-dir)))

(setq org-directory vlad/org-directory
      org-agenda-files (directory-files-recursively vlad/org-directory "\\.org$")
      org-attach-use-inheritance t
      org-hide-emphasis-markers t
      org-startup-indented t
      org-confirm-babel-evaluate nil)

;; Just messing around with KaTeX. AFAIK it is significantly
;; faster than MathJax, but lacks some functionality.
(setq org-html-mathjax-template
      (with-temp-buffer
	(insert-file-contents (expand-file-name "katex.html" user-emacs-directory))
	(buffer-string)))

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

(find-file "keybindings.el")

(use-package evil-commentary
  :ensure t
  :diminish
  :hook
  (after-init . evil-commentary-mode))

(use-package org-roam
  :ensure t

  :custom
  (org-roam-directory (expand-file-name "roam" vlad/org-directory))

  :config
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      `(("d" "default" entry
	 "* %?"
	 :target (file+head "%<%Y-%m-%d>.org"
			    "#+title: %<%Y-%m-%d>\n"))))

;; https://github.com/fxbois/web-mode
(use-package web-mode
  :ensure t)

(defun vlad/web-mode-hook ()
  "Hooks for web-mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'vlad/web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode))

(use-package meson-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" "/usr/share/java/plantuml"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package undo-tree
  :after evil
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package fzf
  :ensure t)

(use-package ivy
  :ensure t
  :diminish
  :hook
  (after-init . ivy-mode)
  :config
  (define-key ivy-minibuffer-map
	      (kbd "C-j") 'ivy-immediate-done))

(use-package grep
  :ensure t
  :config
  (grep-apply-setting
   'grep-find-command
   '("rgp ''" . 6)))

