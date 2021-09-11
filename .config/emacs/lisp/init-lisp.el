;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Eldoc
(use-package eldoc
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  :ensure nil)

(use-package highlight-parentheses
  :straight t
  :hook
  (prog-mode . highlight-parentheses-mode)
  :diminish)

;;;; Scheme

(use-package quack
  :straight t)

(use-package geiser
  :straight t
  :config
  (use-package geiser-mit :straight t))

;;;; Common-lisp

(use-package cl-generic
  :straight t)

(use-package slime
  :defer t
  :custom
  (inferior-lisp-program "/usr/local/bin/sbcl")
  :config
  (use-package slime-autoloads :ensure nil)
  (slime-setup '(slime-fancy)))


(provide 'init-lisp)
;;; init-lisp.el ends here