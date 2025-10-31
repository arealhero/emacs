(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(defun vlad/get-env-var (variable default-value)
  "Get env variable `VARIABLE' or return `DEFAULT-VALUE' if `VARIABLE' is not set."
  (if-let (value (getenv variable))
      value
    default-value))

(defconst vlad/cache-dir (file-name-as-directory (expand-file-name (vlad/get-env-var "XDG_CACHE_HOME" "~/.cache"))))
(defconst vlad/data-dir (file-name-as-directory (expand-file-name (vlad/get-env-var "XDG_DATA_DIR" "~/data")))) ;; TODO: this is not XDG's
(defconst vlad/org-files-dir (file-name-as-directory (expand-file-name "org" vlad/data-dir)))

(defconst vlad/emacs-cache-dir (expand-file-name "emacs" vlad/cache-dir))
(defconst vlad/emacs-eln-cache-dir (expand-file-name "eln-cache" vlad/emacs-cache-dir))

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache vlad/emacs-eln-cache-dir)

  ;; See https://libredd.it/r/emacs/comments/l42oep/comment/gkmnh3y/
  (setq-default comp-async-report-warnings-errors nil)
  (setq-default native-comp-async-report-warnings-errors nil))

(provide 'early-init)
