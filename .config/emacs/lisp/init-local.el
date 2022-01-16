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





(provide 'init-local)
;;; init-local.el ends here
