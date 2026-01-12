;; vlad/emacs/config.el --- My Emacs configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Personal information
(setq user-full-name "Vladislav Sharshukov"
      user-mail-address "vsharshukov@gmail.com")

(when window-system
  (menu-bar-mode -1)

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(when (vlad/macos-system-p)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

(blink-cursor-mode -1)

;; NOTE: pixelwise resizing of windows and frames.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(defun display-startup-echo-area-message ()
  "Hide advertisement from minibuffer."
  (message ""))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; @ref: https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow/603#603
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq sentence-end-double-space nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setq delete-by-moving-to-trash nil)
(setq ring-bell-function 'ignore)

(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10)

(setq confirm-kill-emacs nil)

;; FIXME(vlad): move to vlad-emacs.el
(setq backup-directory-alist
      `((".*" . ,vlad/emacs-backup-dir)))

(setq auto-save-file-name-transforms `((".*" ,vlad/emacs-backup-dir t))
      auto-save-list-file-prefix vlad/emacs-backup-dir
      lock-file-name-transforms `((".*" ,vlad/emacs-lock-files-dir t)))

(setq bookmark-default-file (vlad/get-cache-file "bookmarks"))

;; Indentation settings
(setq standard-indent 4)

(setq-default indent-tabs-mode nil)

(require 'savehist)
;; FIXME(vlad): use vlad/get-cache-file
(setq savehist-file (expand-file-name "savehist-history" vlad/package-cache-dir))
(savehist-mode)

(setq display-time-format "%Y-%m-%d %H:%M"
      display-time-default-load-average nil)
(display-time)

;; --- Misc ---
(electric-pair-mode 1)
;; (setq electric-pair-preserve-balance nil)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(global-eldoc-mode -1)
(setq kill-do-not-save-duplicates t)
(setq blink-matching-paren nil)

(setq comment-multi-line t)
(setq comment-empty-lines nil)

;; (setq font-lock-maximum-decoration 1)

(setq-default fill-column 120)

;; NOTE(vlad): disable line wrapping by default.
(setq-default truncate-lines t)
;; (global-visual-line-mode 1)

(defun vlad/toggle-visual-line-mode ()
  (interactive)
  (if (vlad/minor-mode-enabled-p visual-line-mode)
      (progn
        (let ((inhibit-message t))
          (visual-line-mode -1)
          (unless truncate-lines (toggle-truncate-lines)))
        (message "Line truncation enabled")
        )
    (progn
      (let ((inhibit-message t))
        (when truncate-lines (toggle-truncate-lines))
        (visual-line-mode))
        (message "Line truncation disabled")
      )))

(provide 'vlad/emacs/config)
;;; vlad/emacs/config.el ends here
