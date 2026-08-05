;; vlad/emacs/server.el --- My Emacs server configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'server)

;; NOTE(vlad): `emacsclient' is used as a default editor, so the server needs to be started here.
(unless (server-running-p)
  (server-start))

(provide 'vlad/emacs/server)
;;; vlad/emacs/server.el ends here
