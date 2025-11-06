;;; vlad/fixme.el --- Fontify keywords in comments.  -*- lexical-binding: t; -*-

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

;; ;; FIXME(vlad): support light themes.
;; (defface vlad/username
;;   '((t :foreground "wheat" :bold nil))
;;   "Face for usernames in comments.")

(defface vlad/username
  '((t :inherit font-lock-type-face :slant normal :bold nil))
  "Face for usernames in comments.")

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

(defconst vlad/fixme-keywords
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
          'vlad/username)
        prepend))
    ))

(define-minor-mode vlad/fixme-mode
  "Highlight TODO, FIXME, etc in comments."
  :lighter ""
  :group 'vlad/fixme
  (if vlad/fixme-mode
      (font-lock-add-keywords nil vlad/fixme-keywords t)
    (font-lock-remove-keywords nil vlad/fixme-keywords))
  (font-lock-flush))

(define-globalized-minor-mode vlad/global-fixme-mode
  vlad/fixme-mode vlad/fixme--turn-on-mode-if-needed)

(defun vlad/fixme--turn-on-mode-if-needed ()
  (when (and (apply #'derived-mode-p '(prog-mode))
             (not (string-prefix-p " *temp*" (buffer-name))))
    (vlad/fixme-mode)))

(vlad/global-fixme-mode 1)

(provide 'vlad/fixme)
;;; vlad/fixme.el ends here
