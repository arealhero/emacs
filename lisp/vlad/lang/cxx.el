;;; vlad/lang/cxx.el --- My C/C++ settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --- C++ ---
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
