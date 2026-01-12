;;; vlad/fixme.el --- Fontify keywords in comments.  -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO(vlad):
;;   - support light themes.

;;; Code:

(defgroup vlad/fixme nil
  "Fontify keywords in comments."
  :group 'files)

(defface vlad/todo
  '((t :foreground "red" :slant normal :bold t))
  "Face for TODO-like keywords.")

(defface vlad/note
  '((t :foreground "dodger blue" :slant normal :bold t))
  "Face for NOTE-like keywords.")

(defface vlad/study
  '((t :inherit isearch :slant normal :bold t))
  "Face for usernames in comments.")

;; (defface vlad/study
;;   '((t :foreground "yellow" :slant normal :bold t))
;;   "Face for STUDY-like keywords.")

(defface vlad/username
  '((t :inherit font-lock-type-face :slant normal :bold nil))
  "Face for usernames in comments.")

(defconst vlad/username-group
  (rx (group (one-or-more (or alnum "-" "_")))))

(defconst vlad/todo-keywords-group
  (rx (group (or (seq word-start
                      (or "TODO" "FIXME" "CRIT" "XXX"))
                 (seq "@" word-start
                      (or (seq (any "Tt") "odo")
                          (seq (any "Ff") "ixme")
                          (seq (any "Cc") "rit" (optional "ical")))))
             word-end
             (optional (seq "("
                            (regexp vlad/username-group)
                            ")"))
             (optional ":"))))

(defconst vlad/note-keywords-group
  (rx (group (or (seq word-start "NOTE")
                 (seq "@" word-start
                      (or (seq (any "Nn") "ote")
                          (seq (any "Oo") "ptimization")
                          (seq (any "Rr") "ef" (optional "erence"))
                          (seq "tag"))))
             word-end
             (optional (seq "("
                            (regexp vlad/username-group)
                            ")"))
             (optional ":"))))

(defconst vlad/study-keywords-group
  (rx (group (seq word-start
                  (or "STUDY" (seq "PERF" (optional "ORMANCE"))))
             word-end
             (optional (seq "("
                            (regexp vlad/username-group)
                            ")"))
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
     (1 (when (vlad/in-comment-p) 'vlad/todo) prepend)
     (2 (when (vlad/in-comment-p) 'vlad/username) prepend t))

    ;; Highlight all NOTE keywords in comments
    (,(rx (regexp vlad/note-keywords-group))
     (1 (when (vlad/in-comment-p) 'vlad/note) prepend)
     (2 (when (vlad/in-comment-p) 'vlad/username) prepend t))

    ;; Highlight all STUDY keywords in comments
    (,(rx (regexp vlad/study-keywords-group))
     (1 (when (vlad/in-comment-p) 'vlad/study) prepend)
     (2 (when (vlad/in-comment-p) 'vlad/username) prepend t))
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

(defconst vlad/fixme-include-modes '(prog-mode))

(defun vlad/fixme--turn-on-mode-if-needed ()
  (when (and (apply #'derived-mode-p vlad/fixme-include-modes)
             (not (string-prefix-p " *temp*" (buffer-name))))
    (vlad/fixme-mode)))

(vlad/global-fixme-mode 1)

(provide 'vlad/fixme)
;;; vlad/fixme.el ends here
