;; vlad/emacs/theme.el --- My theme configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'display-fill-column-indicator)
(require 'whitespace) ;; NOTE(vlad): this package is used to highlight tabs and trailing spaces.

;; @ref: https://libreddit.tiekoetter.com/r/emacs/comments/8pfdlb/weird_shifting_problem_with_new_emacs_line_numbers/
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width-start t)

(setq whitespace-style '(face
                         tabs
                         trailing
                         space-before-tab
                         indentation
                         space-after-tab
                         tab-mark
                         missing-newline-at-eof))

;; (set-frame-font "Fira Code 18" nil t)
(set-frame-font "Hack 12" nil t)
(set-face-italic 'italic nil) ;; Looks bad IMO

(defun vlad/font-adjust (increment)
  (let ((inhibit-message t))
    (global-text-scale-adjust increment)))

(defun vlad/font-inc ()
  (interactive)
  (vlad/font-adjust +2))

(defun vlad/font-dec ()
  (interactive)
  (vlad/font-adjust -2))

;; (load-theme 'modus-vivendi-tinted t)
;; (load-theme 'modus-vivendi t)
(load-theme 'modus-operandi-tinted t)
;; (load-theme 'modus-operandi t)

(column-number-mode 1)

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-whitespace-mode 1)
;; (global-display-line-numbers-mode 1)

(provide 'vlad/emacs/theme)
;;; vlad/emacs/theme.el ends here
