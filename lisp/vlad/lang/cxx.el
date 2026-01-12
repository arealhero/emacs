;;; vlad/lang/cxx.el --- My C/C++ settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq c-basic-offset 4)

;; NOTE(vlad): based on Casey Muratori's Big Fun C++ Style (c).
(defconst vlad/cxx-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    ;; TODO(vlad): Disable this?
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 .  -)
                                    (access-label          .  -)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  +)
                                    ;; (cpp-macro             .  nil)
                                    ;; (statement-block-intro .  c-lineup-for)
                                    (case-label            .  +)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     .  -)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  +)))
    ;; (c-echo-syntactic-information-p . t)
    )
    "My C/C++ Style")

(c-add-style "vlad" vlad/cxx-style)

(defun vlad/header-format ()
  "Header file placeholder."
  (insert "#pragma once\n\n"))

(defun vlad/cxx-hook ()
  (c-set-style "vlad")
  (c-toggle-comment-style -1) ;; NOTE(vlad): Use line comments.

  (setq tab-width 8
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]h" buffer-file-name) (vlad/header-format))))

(add-hook 'c-mode-hook 'vlad/cxx-hook)
(add-hook 'c++-mode-hook 'vlad/cxx-hook)

(use-package clang-format
  :straight t
  :defer t
  :config
  (setq clang-format-style "file"))

(defun vlad/c++--find-corresponding-header-file (source-filename)
  (cond ((file-exists-p (file-name-with-extension source-filename "h")) (file-name-with-extension source-filename "h"))
        ((file-exists-p (file-name-with-extension source-filename "hh")) (file-name-with-extension source-filename "hh"))
        ((file-exists-p (file-name-with-extension source-filename "hpp")) (file-name-with-extension source-filename "hpp"))))

(defun vlad/cxx--find-corresponding-source-file (header-filename)
  (cond ((file-exists-p (file-name-with-extension header-filename "c")) (file-name-with-extension header-filename "c"))
        ((file-exists-p (file-name-with-extension header-filename "cc")) (file-name-with-extension header-filename "cc"))
        ((file-exists-p (file-name-with-extension header-filename "cpp")) (file-name-with-extension header-filename "cpp"))))

(defun vlad/cxx-switch-between-header-and-source-files ()
  "Switch between header and source C/C++ files."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (file-extension (file-name-extension current-file))
         (other-file (cond ((string-equal file-extension "c") (file-name-with-extension current-file "h"))
                           ((string-equal file-extension "cpp") (vlad/c++--find-corresponding-header-file current-file))
                           ((string-equal file-extension "cc") (vlad/c++--find-corresponding-header-file current-file))
                           ((string-equal file-extension "h") (vlad/cxx--find-corresponding-source-file current-file)))))
    (unless (stringp other-file)
      (error "Unable to find a corresponding file"))
    (if (file-exists-p other-file)
        (find-file other-file)
      (error "File '%s' does not exist" other-file))))

(defun vlad/cxx-open-unit-test-file ()
  "Open C/C++ unit test file. It must end with `_ut'"
  (interactive)
  (let ((test-filename
         (concat (file-name-sans-extension (buffer-file-name))
                 "_ut")))
    (find-file (cond ((file-exists-p (file-name-with-extension test-filename "c")) (file-name-with-extension test-filename "c"))
                     ((file-exists-p (file-name-with-extension test-filename "cpp")) (file-name-with-extension test-filename "cpp"))
                     ((file-exists-p (file-name-with-extension test-filename "cc")) (file-name-with-extension test-filename "cc"))
                     (t (error "Unable to find a unit test file"))))
    ))

(provide 'vlad/lang/cxx)
;;; vlad/lang/cxx.el ends here
