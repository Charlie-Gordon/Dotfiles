;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Dependencies and Setup

(use-package paredit
  :straight t
  :bind (:map paredit-mode-map
              ("C-<backspace>" . paredit-backward-kill-word))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (ielm-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
