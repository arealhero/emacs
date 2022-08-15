(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(blink-cursor-mode -1)

;; Set up package
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Bootstrap use-package
;; Install use-package if it's not already installed.
(unless (or (package-installed-p 'use-package)
	    (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(org-babel-load-file (concat user-emacs-directory "config.org"))
