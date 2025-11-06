;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'vlad/emacs/early-init-utils)

;; FIXME(vlad): set package-user-dir

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache vlad/emacs-eln-cache-dir)

  ;; See https://libredd.it/r/emacs/comments/l42oep/comment/gkmnh3y/
  (setq-default comp-async-report-warnings-errors nil)
  (setq-default native-comp-async-report-warnings-errors nil))

(provide 'early-init)
