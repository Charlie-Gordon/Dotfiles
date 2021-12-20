;;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package password-store
  :straight t)

(use-package transmission
  :straight t)

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


(use-package peertube
  :straight t)


(use-package org-zettelkasten
  :disabled
  :straight '(org-zk :type git
                     :host github
                     :repo "l3kn/org-zettelkasten"))





(provide 'init-local)
;;; init-local.el ends here
