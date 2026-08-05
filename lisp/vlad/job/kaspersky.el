;;; vlad/job/kaspersky.el --- Kaspersky-specific settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package bazel
  :straight t)

(defun KL/in-monorepo-p ()
  (interactive)
  (let ((current-directory (downcase (subst-char-in-string ?\\ ?/ (vlad/normalize-directory default-directory)))))
    (or (string-prefix-p "w:/" current-directory)
        (string-prefix-p "c:/m/src" current-directory))))

(defun KL/get-current-line-number ()
  (interactive)
  (unless (KL/in-monorepo-p) (error "Current file is not in Monorepo root folder"))
  (let ((start (point-min))
        (n (line-number-at-pos)))
    (if (= start 1)
        (+ n 0)
      (save-excursion
        (save-restriction
          (widen)
          (+ n (line-number-at-pos start)))))))

;; (defun ya/copy-file-name (&rest args)
;;   (interactive)
;;   (let ((filename (buffer-file-name))
;;         (as-c++-include (plist-get args :as-c++-include))
;;         (as-python-import (plist-get args :as-python-import))
;;         (as-python-import-from (plist-get args :as-python-import-from)))
;;     (if (string-match-p ya/arcadia-root filename)
;;         (progn
;;           (setq relative-path (nth 1 (split-string filename ya/arcadia-root)))
;;           (if as-c++-include
;;               (setq relative-path (concat "#include <" relative-path ">")))
;;           (if as-python-import
;;               (setq relative-path (concat
;;                                    "import "
;;                                    (replace-regexp-in-string "/" "." (string-remove-suffix ".py" relative-path)))))
;;           (if as-python-import-from
;;               (setq relative-path (concat
;;                                    "from "
;;                                    (replace-regexp-in-string "/" "." (string-remove-suffix ".py" relative-path))
;;                                    " import ")))
;;           (kill-new relative-path)
;;           (message (concat "Copied: " relative-path)))
;;       (message "Error: current file is not in arcadia root folder"))))

(defun KL/open-in-monorepo ()
  (interactive)
  (unless (KL/in-monorepo-p) (error "Current file is not in Monorepo root folder"))
  (let* ((line-number (KL/get-current-line-number))
         (filename (downcase (subst-char-in-string ?\\ ?/ (buffer-file-name))))
         (relative-monorepo-path (if (string-match-p "w:/" filename)
                                     (nth 1 (split-string filename "w:/"))
                                   (nth 1 (split-string filename "c:/m/src/")))))
    (let ((url (concat KL/monorepo-url
                       "?path=%2F"
                       (replace-regexp-in-string "/" "%2F" relative-monorepo-path t)
                       "&line="
                       (number-to-string line-number)
                       "&lineEnd="
                       (number-to-string (1+ line-number))
                       "&lineStartColumn=1&lineEndColumn=1&lineStyle=plain"
                       )))
      (kill-new url)
      (browse-url url))))

(provide 'vlad/job/kaspersky)
;;; vlad/job/kaspersky.el ends here
