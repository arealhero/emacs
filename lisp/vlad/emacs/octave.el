;; vlad/emacs/octave.el --- My Octave configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'octave
  ;; FIXME(vlad): wrap `octave-source-file' so it could open inferior octave buffer in a split.
  ;;              And also it should probably `erase-buffer' before sourcing file
  ;;              also clearing and clearvars-ing existing octave session.
  (setq inferior-octave-startup-args '("-i" "-q" "--no-line-editing")))

(provide 'vlad/emacs/octave)
;;; vlad/emacs/octave.el ends here
