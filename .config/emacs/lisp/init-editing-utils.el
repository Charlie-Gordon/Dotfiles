;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;;; Some basic preferences
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name "bookmarks/bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files t
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 sentence-end-double-space nil
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 bidi-paragraph-direction 'left-to-right)



(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)

(show-paren-mode 1)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(save-place-mode t)

(global-so-long-mode 1)

(use-package autorevert
  :ensure nil
  :config
  :diminish auto-revert-mode)

(use-package beacon
  :straight t
  :custom
  (beacon-lighter "")
  (beacon-size 20)
  :hook
  (after-init . beacon-mode))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(use-package rainbow-delimiters
  :straight t
  :hook
  (progn-mode . rainbow-delimiters-mode))


(use-package avy
  :straight t
  :bind*
  ("M-j" . avy-goto-char-timer)
  :custom
  (avy-timeout-seconds "0.4"))

(use-package ace-link
  :straight t
  :bind (:map org-mode-map
              ("M-o" . ace-link))
  :config
  (ace-link-setup-default))


(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode)
  :diminish)

;; persistent-scratch
(use-package persistent-scratch
  :straight t
  :custom
  (persistent-scratch-backup-directory (cdar backup-directory-alist))
  :config
  (persistent-scratch-setup-default)
  :diminish persistent-scratch-mode)

;;; Pretty fonts

(use-package mixed-pitch
  :straight t)

;;; Quick navigation

(use-package affe
  :straight t
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package navigation
  :ensure nil
  :after consult affe
  :bind-keymap* ("s-n" . navigation-map))

;;; Writing
(use-package typo
  :straight '(typoel :type git :host github :repo "jorgenschaefer/typoel"))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
