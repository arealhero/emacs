;;; vlad-fixme.el --- Fontify keywords in comments.  -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO(vlad):
;;   - support `tree-sitter'
;;   - support case-insensitivity?

;;; Code:

(defgroup vlad/fixme nil
  "Fontify keywords in comments."
  :group 'files)

(defface vlad/todo
  '((t :foreground "red" :bold t))
  "Face for TODO-like keywords.")

(defface vlad/note
  '((t :foreground "dodger blue" :bold t))
  "Face for NOTE-like keywords.")

(defface vlad/green
  '((t :foreground "wheat" :bold nil))
  "Face for the good things.")

(defcustom vlad/fixme-modes
  '(c-mode
    c-ts-mode
    c++-mode
    c++-ts-mode
    cmake-ts-mode
    emacs-lisp-mode
    sql-mode)
  "List of major modes with enabled keywords highlighting."
  :type 'list)

(defconst vlad/todo-keywords-group (rx (group (or (seq word-start
                                                       (or "TODO" "FIXME" "CRIT" "XXX"))
                                                  (seq "@" word-start
                                                       (or (seq (any "Tt") "odo")
                                                           (seq (any "Ff") "ixme")
                                                           (seq (any "Cc") "rit" (optional "ical")))))
                                              word-end
                                              (optional ":"))))

(defconst vlad/note-keywords-group (rx (group (or (seq word-start "NOTE")
                                                  (seq "@" word-start
                                                       (or (seq (any "Nn") "ote")
                                                           (seq (any "Oo") "ptimization")
                                                           (seq (any "Rr") "ef" (optional "erence")))))
                                              word-end
                                              (optional ":"))))

(defun vlad/in-comment-p ()
  "Check if point is inside a comment."
  (let ((ppss (syntax-ppss)))
    (or (nth 4 ppss)     ;; Line comment.
        (nth 7 ppss))))  ;; Block comment.

(defun vlad/fixme-fontify-mode (&optional mode)
  (interactive)
  (unless mode (setq mode major-mode))

  (font-lock-add-keywords mode
                          `(
                            ;; Highlight all TODO keywords in comments
                            (,(rx (regexp vlad/todo-keywords-group))
                             (1 (when (vlad/in-comment-p)
                                  'vlad/todo)
                                prepend))

                            ;; Highlight all NOTE keywords in comments
                            (,(rx (regexp vlad/note-keywords-group))
                             (1 (when (vlad/in-comment-p)
                                  'vlad/note)
                                prepend))

                            ;; Highlight all usernames after the TODO keywords in comments
                            (,(rx (or (regexp vlad/todo-keywords-group)
                                      (regexp vlad/note-keywords-group))
                                  "(" word-start
                                  (group (one-or-more (not ")")))
                                  word-end ")")
                             (3 (when (vlad/in-comment-p)
                                  'vlad/green)
                                prepend))
                          )))

;; (mapc 'vlad/fixme-fontify-mode vlad/fixme-modes)

(add-hook 'prog-mode-hook
          (lambda ()
            (vlad/fixme-fontify-mode major-mode)))

(provide 'vlad-fixme)
;;; vlad-fixme.el ends here
