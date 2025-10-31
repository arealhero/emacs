(setq process-adaptive-read-buffering nil)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 1 1024 1024)))

;; --- straight.el ---

(defvar bootstrap-version)
(setq straight-base-dir vlad/emacs-cache-dir)
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

(straight-use-package 'use-package)

(setq gc-cons-threshold (* 200 1024 1024))
(setq jit-lock-defer-time 0)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract
                                          after-init-time
                                          before-init-time)))
                     gcs-done)))

;;; --- Loading configs ---

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(load-file (locate-user-emacs-file "config.el"))

(require 'vlad-fixme)
(require 'vlad-org)

(provide 'init)

;;; init.el ends here
