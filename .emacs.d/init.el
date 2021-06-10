;;; init.el --- Configuration for emacs             -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My personal configuration file for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From Jordon Biondo at
;; https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up
(let ((file-name-handler-alist nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;; Essential External Programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(load "~/.emacs.d/external/stuffs.el" t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Managing packages
;;;;;; straight.el (with-use-package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-package)
;;;;;; Local packages (on site-lisp/)
(use-package init-site-lisp :ensure nil)
;;;;; Utilities functions
(use-package init-utils :ensure nil)
;;;;; Interface tweaks
(use-package init-face
  :ensure nil
  :config
  (exwm-input-set-key (kbd "s-.") #'reload-emacs-configuration)
  (exwm-input-set-key (kbd "s-s") #'magit-status-dotfiles)
  (exwm-input-set-key (kbd "s-d") #'modus-themes-toggle)
  (exwm-input-set-key (kbd "s-<return>") #'eshell)
  (exwm-input-set-key (kbd "<print>") #'screenshot-svg))
;;;;; TeX
(use-package init-tex :ensure nil)
;;;;; Org-mode configuration
(use-package init-org :ensure nil)

;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Builtin packages
;;;;;; Dired
(use-package init-dired :ensure nil)
;;;;;; Outline
(use-package init-outline :ensure nil)
;;;;;; EWW & SHR
(use-package init-eww-shr :ensure nil)
;;;;; Git
(use-package init-git :ensure nil)
;;;;; Completions & Navigation
;;;;;; Completions
;;;;;;;  Selectrum
(use-package selectrum
  :straight t
  :custom
  (selectrum-fix-vertical-window-height selectrum-max-window-height "Show as many candidates as possible")
  :hook (after-init . selectrum-mode))
;;;;;;;  Consult
(use-package init-consult :ensure nil)
;;;;;;;  Embark
(use-package embark
  :straight t
  :bind
  ("C-." . embark-act)
  (:map embark-url-map
	("s" . #'browse-url-xdg-open)
	("m" . #'mpv-play-url))
  (:map embark-symbol-map
	("h" . #'helpful-at-point)))
;;;;;;;  Orderless
(use-package orderless
  :straight t
  :after selectrum
  :custom
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (completion-styles '(orderless partial-completion)))
;;;;;;;  Marginalia
(use-package marginalia
  :straight t
  :after selectrum
  :config (marginalia-mode 1))
;;;;;; Navigation
;;;;;;;  avy
(use-package avy
  :straight t)
;;;;; Application & utilities
;;;;;; Password-store
(use-package pass
  :straight t)
;;;;;; Torrenting
(use-package transmission
  :straight t
  :custom
  (transmission-timer 30))
;;;;; Multimedia
;;;;;; EMMS
(use-package emms
  :straight t
  :config
  (use-package emms-setup
    :ensure nil
    :custom
    (emms-source-file-default-directory "/storage/journals/resources/")
    :config
    (emms-all)
    (emms-default-players))
  (use-package emms-player-simple-mpv :disabled :straight t))
;;;;;; MPV
(use-package mpv
  :straight t)
;;;;;; LBRY

(straight-use-package
 '(lbry-mode.el :type git :host gitlab :repo "Charlie-Gordon/lbry-mode-el"))

;;;;;; parallel-mode.el

(straight-use-package
 '(parallel-mode.el :type git :host gitlab :repo "Charlie-Gordon/parallel-mode.el"))

;;;;;; Magit
(use-package magit
  :straight t
  :requires with-editor tramp)
;;;;; Note
;;;;;; Nov.el
(use-package nov
  :straight t
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :mode (("\\.epub\\'" . nov-mode)))
;;;;;; PDFs
(use-package init-pdftools :ensure nil)
;;;;; Language settings for prose and code
;;;;;; Yasnippet
(use-package yasnippet
  :straight t
  :config
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-global-mode))
;;;;;; Markdown

(use-package markdown-mode
  :straight t
  :disabled
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;;;;; Lisp
(use-package init-paredit :ensure nil)
;;;;;;; Scheme

(use-package quack
  :straight t)

(use-package geiser
  :straight t
  :config
  (use-package geiser-mit :straight t))

;;;;;;; Common-lisp

(use-package cl-generic
  :straight t)

(use-package slime
  :defer t
  :custom
  (inferior-lisp-program "/usr/local/bin/sbcl")
  :config
  (use-package slime-autoloads :ensure nil)
  (slime-setup '(slime-fancy)))
)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; no-byte-compile: t
;; End:
;;;;; Miscellaneous
(use-package init-misc :ensure nil)
;;; init.el ends here
