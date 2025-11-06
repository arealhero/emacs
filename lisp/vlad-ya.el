;;; vlad-ya.el --- Yandex-specific Emacs configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bug-reference)

(defconst ya/arcadia-root (file-name-as-directory (expand-file-name "~/arcadia")))
(defconst ya/arcadia-url "https://a.yandex-team.ru/arcadia/")

(defconst ya/tracker-ticket-regexp (rx (group bow
                                              (group (one-or-more alpha) "-" (one-or-more digit))
                                              eow)))

(defun ya/in-arcadia-p ()
  (interactive)
  (let ((current-directory (vlad/normalize-directory default-directory)))
    (string-prefix-p ya/arcadia-root current-directory)))

;; FIXME(vlad): use eglot instead
;; NOTE(vlad): the codebase is extremely shitty so there's nothing to do without lsp.
(use-package lsp-mode
  :straight t
  :config
  (setq lsp-semantic-tokens-enable t
        lsp-inlay-hint-enable nil
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-apply-edits-after-file-operations nil ;; See https://old.reddit.com/r/emacs/comments/1b0ppls/anyone_using_lspmode_with_tsls_having_trouble/
        lsp-enable-file-watchers nil) ;; TODO(arealhero): disable watchers for pyright only

  (setq lsp-clangd-binary-path
        "/opt/homebrew/opt/llvm/bin/clangd")

  (setq lsp-clients-clangd-args
        '("-j=8"
          "--background-index"
          ;; "--log=verbose"
          "--clang-tidy"
          "--completion-style=detailed"
          ;; "--all-scopes-completion",
          "--experimental-modules-support"
          "--header-insertion=never"))

  (setq lsp-pylsp-plugins-flake8-max-line-length nil)

  :general-config
  (general-def 'normal '(c-ts-mode-map c++-ts-mode-map)
    "K" 'lsp-describe-thing-at-point)

  (general-def 'normal '(c-ts-mode-map c++-ts-mode-map)
    :prefix "SPC"

    "ca" 'lsp-execute-code-action
    "cr" 'lsp-rename
    "lh" 'lsp-inlay-hints-mode
    "rf" 'lsp-find-references
    "rw" 'lsp-restart-workspace)
  )

;; NOTE(vlad): for terraform
(use-package hcl-mode
  :straight t
  :mode (("\\.tf\\'" . hcl-mode)))

(defun ya/configure-c++ ()
  (when (ya/in-arcadia-p)
    (setq bug-reference-bug-regexp ya/tracker-ticket-regexp)
    (setq bug-reference-url-format "https://st.yandex-team.ru/%s")

    (bug-reference-prog-mode 1)
    (lsp-mode)))

(add-hook 'c-ts-mode-hook 'ya/configure-c++)
(add-hook 'c++-ts-mode-hook 'ya/configure-c++)

(defun ya/get-current-line-number ()
  (interactive)
  (unless (ya/in-arcadia-p) (error "Current file is not in Arcadia root folder"))
  (let ((start (point-min))
        (n (line-number-at-pos)))
    (if (= start 1)
        (+ n 0)
      (save-excursion
        (save-restriction
          (widen)
          (+ n (line-number-at-pos start)))))))

(defun ya/copy-file-name (&rest args)
  (interactive)
  (let ((filename (buffer-file-name))
        (as-c++-include (plist-get args :as-c++-include))
        (as-python-import (plist-get args :as-python-import))
        (as-python-import-from (plist-get args :as-python-import-from)))
    (if (string-match-p ya/arcadia-root filename)
        (progn
          (setq relative-path (nth 1 (split-string filename ya/arcadia-root)))
          (if as-c++-include
              (setq relative-path (concat "#include <" relative-path ">")))
          (if as-python-import
              (setq relative-path (concat
                                   "import "
                                   (replace-regexp-in-string "/" "." (string-remove-suffix ".py" relative-path)))))
          (if as-python-import-from
              (setq relative-path (concat
                                   "from "
                                   (replace-regexp-in-string "/" "." (string-remove-suffix ".py" relative-path))
                                   " import ")))
          (kill-new relative-path)
          (message (concat "Copied: " relative-path)))
      (message "Error: current file is not in arcadia root folder"))))

(defun ya/open-in-arcadia ()
  (interactive)
  (let ((line-number (ya/get-current-line-number))
        (filename (buffer-file-name)))
    (if (string-match-p ya/arcadia-root filename)
        (let ((url (concat ya/arcadia-url
                           (nth 1 (split-string filename ya/arcadia-root))
                           "#L"
                           (number-to-string line-number))))
          (kill-new url)
          (browse-url url))
      (error "Current file is not in arcadia root folder"))))

(defconst ya/unit-test-regex (rx (not "/") "Y_UNIT_TEST" (or "" "_F") "("))
(defun ya/for-each-test (action)
  (interactive)
  (let ((saved-pos (point)))
    (goto-char (point-min))
    (while (re-search-forward ya/unit-test-regex nil t)
      (end-of-line)
      (cond ((eq action 'toggle) (hs-toggle-hiding))
            ((eq action 'open) (hs-show-block))
            ((eq action 'close) (hs-hide-block))
            (t ())))
    (goto-char saved-pos)))

(provide 'vlad-ya)
;;; vlad-ya.el ends here
