;; vlad/emacs/dired.el --- My Dired configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dired)

(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-dwim-target t)

(defun vlad/dired-highlight-executables ()
  (save-excursion
    (remove-overlays (point-min) (point-max) 'vlad-to-be-removed t)

    (goto-char (point-min))
    (forward-line) ;; NOTE(vlad): Skipping dired header.
    (while (not (eobp))
      (let ((mask (buffer-substring (+ (line-beginning-position) 2)
                                    (+ (line-beginning-position) 12))))
        ;; NOTE(vlad): mask = '-rwxrwxrwx' ('drwxrwxrwx' for directories).
        (when (and (eq (aref mask 0) ?-) ;; NOTE(vlad): Just a regular file.
                   (or (eq (aref mask 3) ?x)
                       (eq (aref mask 6) ?x)
                       (eq (aref mask 9) ?x)))
          (dired-move-to-filename)
          (let* ((start (point))
                 (end (line-end-position))
                 (overlay (make-overlay start end)))
            (overlay-put overlay 'face 'warning)
            (overlay-put overlay 'vlad-to-be-removed t))))
      (forward-line))))

(add-hook 'dired-after-readin-hook #'vlad/dired-highlight-executables)

(defun vlad/open-current-directory-in-dired ()
  (interactive)
  (dired "."))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'vlad/emacs/dired)
;;; vlad/emacs/dired.el ends here
