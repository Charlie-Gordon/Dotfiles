;;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package volume
  :straight '(volume.el :type git
                        :host github
                        :repo "dbrock/volume.el"
                        :fork "spwhitton/volume.el"
                        :branch "series/define-obsolete-variable-alias")
  :bind ("s-c" . volume))

(use-package password-store
  :straight t)

(use-package transmission
  :bind (:map transmission-mode-map
              ("r" . transmission-move)
              ("R" . transmission-remove))
  :straight t)

(use-package trashed
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package vterm
  :straight t)

(use-package emms
  :straight t
  :bind
  ("s-m e" . emms)
  ("s-m b" . emms-smart-browse)
  (:map emms-browser-mode-map
        ("a" . emms-add-directory-tree))
  :custom
  (emms-source-file-default-directory "/storage/music/")
  (emms-history-file (expand-file-name "emms-history" emms-source-file-default-directory))
  (emms-player-mpv-parameters '("--volume=100" "--quiet"
                                "--really-quiet" "--no-audio-display"
                                "--no-video"))
  (emms-history-start-playing t)
  :config
  (use-package emms-setup :ensure nil)
  (emms-all)
  (emms-default-players)
  (use-package emms-history :ensure nil))

(use-package ebib
  :straight t)

(provide 'init-local)
;;; init-local.el ends here
