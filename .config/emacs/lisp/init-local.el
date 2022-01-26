;;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package volume
  :straight '(volume.el :type git
                        :host github
                        :repo "spwhitton/volume.el"
                        :branch "series/define-obsolete-variable-alias")
  :bind ("s-c" . volume))

(use-package password-store
  :straight t)

(use-package transmission
  :straight '(transmission :type git
                           :host github
                           :repo "xFA25E/transmission"
                           :branch "feature/rename-path"))

(use-package trashed
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package vterm
  :straight t)

(use-package elfeed
  :straight t)

(use-package org-el-cache
  :straight '(org-zk :type git
                     :host github
                     :repo "l3kn/org-el-cache")
  :disabled)

(use-package org-zettelkasten
  :disabled
  :straight '(org-zk :type git
                     :host github
                     :repo "l3kn/org-zettelkasten"))

(use-package emms
  :straight t
  :bind
  ("s-m e" . emms)
  ("s-m b" . emms-smart-browse)
  :custom
  (emms-source-file-default-directory "/storage/music/")
  (emms-history-file (expand-file-name "emms-history" emms-source-file-default-directory))
  (emms-history-start-playing t)
  :config
  (use-package emms-setup :ensure nil)
  (emms-all)
  (emms-default-players)
  (use-package emms-history :ensure nil))

(provide 'init-local)
;;; init-local.el ends here
