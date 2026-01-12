;;; vlad/debug.el --- My debug setup.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq gdb-debuginfod-enable-setting t)

(defun vlad/gdb-print ()
  (interactive)
  (let ((saved-pos (point)))
    (gud-print)
    (goto-char saved-pos)))

(defun vlad/toggle-breakpoint ()
  (interactive)
  (let ((saved-pos (point)))
    ;; TODO(vlad): Check if breakpoint is already set on this line
    ;;             and remove this if it is.
    (gud-break)
    (goto-char saved-pos)))

(defun vlad/start-debugger ()
  (interactive)
  (let ((this-window (selected-window))
        (gdb-buffer (gdb-get-buffer 'gdbmi)))
    (setq other-window-in-split (next-window))
    (if (eq other-window-in-split this-window)
        (setq other-window-in-split (split-window-right)))
    (select-window other-window-in-split)
    (if gdb-buffer
        (progn
          (switch-to-buffer gdb-buffer)
          (message "GDB is already started."))
      (call-interactively 'gdb))
    (select-window this-window)))

(defun vlad/stop-debugger ()
  (interactive)
  (let ((gdb-buffer (gdb-get-buffer 'gdbmi)))
    (if gdb-buffer
        (progn
          (setq gdb-window (get-buffer-window gdb-buffer))
          (if gdb-window
              (progn
                (select-window gdb-window)
                (kill-buffer-and-window))
            (kill-buffer gdb-buffer)))
      (message "GDB is already stopped."))))

(provide 'vlad/debug)
;;; vlad/debug.el ends here
