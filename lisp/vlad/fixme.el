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
  '((t :foreground "red" :slant normal :bold t))
  "Face for TODO-like keywords.")

(defface vlad/note
  '((t :foreground "dodger blue" :slant normal :bold t))
  "Face for NOTE-like keywords.")

;; ;; FIXME(vlad): support light themes.
;; (defface vlad/username
;;   '((t :foreground "wheat" :bold nil))
;;   "Face for usernames in comments.")

(defface vlad/username
  '((t :inherit font-lock-type-face :slant normal :bold nil))
  "Face for usernames in comments.")

(defconst vlad/todo-keywords-group
  (rx (group (or (seq word-start
                      (or "TODO" "FIXME" "CRIT" "XXX"))
                 (seq "@" word-start
                      (or (seq (any "Tt") "odo")
                          (seq (any "Ff") "ixme")
                          (seq (any "Cc") "rit" (optional "ical")))))
             word-end
             (optional (seq "("
                            (group (one-or-more alnum))
                            ")"))
             (optional ":"))))

(defconst vlad/note-keywords-group
  (rx (group (or (seq word-start "NOTE")
                 (seq "@" word-start
                      (or (seq (any "Nn") "ote")
                          (seq (any "Oo") "ptimization")
                          (seq (any "Rr") "ef" (optional "erence")))))
             word-end
             (optional (seq "("
                            (group (one-or-more alnum))
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
     (2 (when (vlad/in-comment-p) 'vlad/username) prepend))

    ;; Highlight all NOTE keywords in comments
    (,(rx (regexp vlad/note-keywords-group))
     (1 (when (vlad/in-comment-p) 'vlad/note) prepend)
     (2 (when (vlad/in-comment-p) 'vlad/username) prepend))
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

;; FIXME(vlad): enable `treesit'-powered keyword highlighting.
;; (defun my-highlight-keywords-in-comment (node override start end &rest _)
;;   "Highlight specific keywords within comment nodes."
;;   (let ((text (treesit-node-text node)))
;;     (message "treesit text: %s" text)
;;     (save-match-data
;;       (when (string-match "\\<\\(TODO\\|FIXME\\)\\>" text)
;;         (message "treesit matched!")
;;         (treesit-fontify-with-override
;;          (+ (treesit-node-start node) (match-beginning 1))
;;          (+ (treesit-node-start node) (match-end 1))
;;          'font-lock-warning-face override start end)))))

;; (add-hook 'cmake-ts-mode-hook
;;           (lambda ()
;;             (setq-local treesit-font-lock-settings
;;                         (append treesit-font-lock-settings
;;                                 (treesit-font-lock-rules
;;                                  :language 'cmake
;;                                  :override t
;;                                  :feature 'comment
;;                                  '((comment) @my-highlight-keywords-in-comment))))
;;             (treesit-major-mode-setup)
;;             ))

(vlad/global-fixme-mode 1)

(provide 'vlad/fixme)
;;; vlad/fixme.el ends here
