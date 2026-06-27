;;; vlad/lang/eon.el --- My eon settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package simpc-mode
  :straight (simpc-mode
             :type git
             :host github
             :repo "rexim/simpc-mode"))

(defun simpc-types ()
  '("s8" "s16" "s32" "s64"
    "u8" "u16" "u32" "u64"
    "f32" "f64"
    "_"))

(defun simpc-keywords ()
  '("mutable" "if" "else" "return" "while"))

(define-derived-mode eon-mode simpc-mode "eon"
  "Major mode for eon programming language.")

(add-to-list 'auto-mode-alist '("\\.eon\\'" . eon-mode))

(provide 'vlad/lang/eon)
;;; vlad/lang/eon.el ends here
