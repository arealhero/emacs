;; vlad/packages/org.el --- My org-mode settings.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; TODO(vlad): install notdeft to be able to quickly search in notes without exiting Emacs?
;;;             @ref: https://github.com/hasu/notdeft

;;; Code:

(defun vlad/org-log-current-time ()
  "Add current time to the nearest header."
  (interactive)
  (org-set-property "TIME" (vlad/get-current-time-string)))

(defun vlad/sync-org-roam-db ()
  "Syncronize org-roam db."
  (interactive)
  ;; (setq org-agenda-files (directory-files-recursively vlad/org-files-dir org-agenda-file-regexp))
  (org-roam-db-sync))

(defconst vlad/org-roam-cache-dir (vlad/get-cache-dir "org-roam"))

;; ------------------------------
;; |            org             |
;; ------------------------------

(use-package org
  ;; NOTE(vlad): building Karthik Chikmagalur's fork to enable painless latex previews.
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pin nil)

  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN_ASSESSMENT(a)" "IN_PROGRESS(p)" "HABIT(h)" "|" "DONE(d)" "WONTFIX(w)" "DROPPED(D)" "BACKLOG(b)")))

  (setq org-directory vlad/org-files-dir
        org-id-locations-file (expand-file-name "org-id-locations" vlad/org-roam-cache-dir)
        org-attach-use-inheritance t
        org-hide-emphasis-markers nil
        org-startup-indented t
        org-startup-folded 'nofold
        org-confirm-babel-evaluate nil
        org-deadline-warning-days 0)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-mode-display-live nil)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-mode-update-delay 0.25)

  (setq org-latex-packages-alist '())
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t))

  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.9)
  (plist-put org-latex-preview-appearance-options
             :scale 1.5)
  (plist-put org-latex-preview-appearance-options
             :zoom 1.5)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)
  (setq org-latex-preview-cache vlad/org-latex-preview-cache-dir)

  (add-hook 'org-mode-hook (lambda ()
                             (setq fill-column 90)))

  ;; NOTE(vlad): open documents in zathura and videos in mpv.
  ;;             @ref: https://stackoverflow.com/a/8836108/789593
  (add-hook 'org-mode-hook
            #'(lambda ()
                (delete '("\\.pdf\\'" . default) org-file-apps)
                (delete '("\\.djvu\\'" . default) org-file-apps)
                (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
                (add-to-list 'org-file-apps '("\\.djvu\\'" . "zathura %s"))

                (delete '("\\.webm\\'" . default) org-file-apps)
                (delete '("\\.mkv\\'" . default) org-file-apps)
                (add-to-list 'org-file-apps '("\\.webm\\'" . "mpv --speed=1.75 --fs %s"))
                (add-to-list 'org-file-apps '("\\.mkv\\'" . "mpv --speed=1.75 --fs %s"))))

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook (lambda ()
                             (org-latex-preview 'buffer)
                             (org-latex-preview-mode)))

  ;; FIXME(vlad): change to `evil-next-visual-line' and `evil-previous-visual-line'.
  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-ignored-commands 'previous-line))

;; ------------------------------
;; |          org-roam          |
;; ------------------------------

(use-package org-roam
  :straight t

  :config
  (defconst vlad/daily-template
    ":PROPERTIES:
:CATEGORY: Daily [%<%Y-%m-%d>]
:END:
#+TITLE: Daily [%<%Y-%m-%d>]
#+FILETAGS: :daily:
")

  (setq org-roam-directory vlad/org-files-dir
        org-roam-dailies-directory "daily/"
        org-roam-db-location (expand-file-name "org-roam.db" vlad/org-roam-cache-dir))

  ;; NOTE(vlad): temporarily increase gc threshold to speed up memory-intensive operations.
  ;;             @ref: https://www.orgroam.com/manual.html#Garbage-Collection-1
  (setq org-roam-db-gc-threshold most-positive-fixnum)

  (setq org-roam-dailies-capture-templates
        `(("d" "default" plain "%?"
           :if-new (file+head "%<%Y-%m-%d>.org" ,vlad/daily-template)
           :immediate-finish t
           :unnarrowed t)
          ))

  ;; NOTE(vlad): defining custom org-roam-node properties for convenience.

  ;; Youtube channel name
  (cl-defmethod org-roam-node-channel-name ((node org-roam-node))
    (let ((channel-name (cdr (assoc-string "CHANNEL_NAME" (org-roam-node-properties node)))))
      (if channel-name channel-name
        (when org-roam-capture--node ; non-nil only during org-roam-capture
          (setq channel-name (read-string "Channel name: "))
          (push (cons "CHANNEL_NAME" channel-name) (org-roam-node-properties node)))
        channel-name)))

  ;; Youtube channel link
  (cl-defmethod org-roam-node-channel-link ((node org-roam-node))
    (let ((channel-link (cdr (assoc-string "CHANNEL_LINK" (org-roam-node-properties node)))))
      (if channel-link channel-link
        (when org-roam-capture--node ; non-nil only during org-roam-capture
          (setq channel-link (read-string "Channel link: "))
          (push (cons "CHANNEL_LINK" channel-link) (org-roam-node-properties node)))
        channel-link)))

  ;; Youtube video link
  (cl-defmethod org-roam-node-video-link ((node org-roam-node))
    (let ((video-link (cdr (assoc-string "VIDEO_LINK" (org-roam-node-properties node)))))
      (if video-link video-link
        (when org-roam-capture--node ; non-nil only during org-roam-capture
          (setq video-link (read-string "Video link: "))
          (push (cons "VIDEO_LINK" video-link) (org-roam-node-properties node)))
        video-link)))

  (defconst vlad/yt-channel-template
    ":PROPERTIES:
:CATEGORY: ${channel-name}
:CHANNEL_NAME: ${channel-name}
:CHANNEL_LINK: ${channel-link}
:END:
#+TITLE: ${channel-name}
#+CREATED: %U
#+FILETAGS: :channel:youtube_channel:
")

  (defconst vlad/yt-video-entry
    "

* ${channel-name} - \"${title}\" :yt:youtube_video:video:
:PROPERTIES:
:CATEGORY: ${channel-name} - \"${title}\"
:VIDEO_LINK: ${video-link}
:END:
#+CREATED: %U
- Youtube link: [[${video-link}][${channel-name} - \"${title}\"]]
%?
")

  (setq org-roam-capture-templates
        `(("m" "Main")
          ("mg" "General note" plain "%?"
           :if-new
           (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                      ":PROPERTIES:\n:CATEGORY: ${title}\n:END:\n#+TITLE: ${title}\n#+CREATED: %U\n#+FILETAGS: :main:\n")
           :immediate-finish t
           :unnarrowed t)

          ("mp" "Person" plain "%?"
           :if-new
           (file+head "main/person/%<%Y%m%d%H%M%S>-${slug}.org"
                      ":PROPERTIES:\n:CATEGORY: ${title}\n:END:\n#+TITLE: ${title}\n#+CREATED: %U\n#+FILETAGS: :person:\n")
           :immediate-finish t
           :unnarrowed t)

          ("y" "Youtube video"
           entry
           ,vlad/yt-video-entry
           :if-new
           (file+head "main/videos/yt-channels/${channel-name}.org" ,vlad/yt-channel-template)
           :empty-lines-before 1
           :immediate-finish t
           :unnarrowed t ;; FIXME(vlad): remove
           )
          ))

  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags}" 'face 'org-tag)))

  ;; FIXME(vlad): move to org-roam
  (add-hook 'after-init-hook 'vlad/sync-org-roam-db)
  (add-hook 'org-roam-mode-hook 'vlad/sync-org-roam-db)

  ;; NOTE(vlad): comment out the next line if you get an error `(wrong-type-argument sqlitep nil)' during startup.
  ;;             @ref: https://github.com/doomemacs/doomemacs/issues/8066
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

(provide 'vlad/packages/org)
;;; vlad/packages/org.el ends here
