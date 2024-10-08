(setq custom-file (locate-user-emacs-file "custom.el"))

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message "")

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; --- straight.el ---

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default 't
              straight-vc-git-default-clone-depth 1)

;; --- garbage collection ---

;; max memory available for gc on startup
(defvar vlad/gc-cons-threshold (* 200 1024 1024))
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold vlad/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; do garbage collection when Emacs is out of focus
(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda () (unless (frame-focus-state)
                               (garbage-collect))))
  (add-hook 'after-focus-change-function
            'garbage-collect))

(defun vlad/defer-garbage-collection-h ()
  "Set max memory available for gc when opening minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun vlad/restore-garbage-collection-h ()
  "Restore max memory available for gc."
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold vlad/gc-cons-threshold))))
(add-hook 'minibuffer-setup-hook #'vlad/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'vlad/restore-garbage-collection-h)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 4 1024 1024)))

(straight-use-package 'use-package)

;; NOTE: I know what I'm doing.
(put 'narrow-to-page 'disabled nil)

(use-package diminish
  :straight t)

(use-package gcmh
  :straight t
  :after diminish
  :config
  ;; General performance tuning
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))


(setq jit-lock-defer-time 0)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format
             "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
            gcs-done)))

(defun display-startup-echo-area-message ()
  "Hide advertisement from minibuffer."
  (message ""))

(blink-cursor-mode -1)

(use-package bind-key
  :straight t)

(defun vlad/load-config-file (filename)
  "Load the config file by FILENAME."
  (load-file (expand-file-name filename user-emacs-directory)))

(load-file (locate-user-emacs-file "utils.el"))
(load-file (locate-user-emacs-file "config.el"))
(load-file (locate-user-emacs-file "custom.el"))

(provide 'init)

;;; init.el ends here
