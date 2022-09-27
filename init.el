(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message ()
  (message ""))

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

(defun vlad/load-config-file (filename)
  (load-file (expand-file-name filename user-emacs-directory)))

(vlad/load-config-file "utils.el")
(vlad/load-config-file "config.el")
(vlad/load-config-file "custom.el")
