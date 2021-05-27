(let ((file-name-handler-alist nil))
;;; Essential External Programs
(load "~/.emacs.d/external/stuffs.el" t)
;;; Emacs initialization
;;;; Use-package
;;;;; Initialize
(package-initialize)
;;;;; Add package sources
(unless (assoc-default "melpa" package-archives) 
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
;;;;; Bootstrapping Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-verbose t)
;;;; Setting load-path
(let* ((path (expand-file-name "site-lisp" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))
;;;; Utilities functions
(use-package init-00-utils.el
  :ensure nil)
;;;; Interface tweaks
(use-package init-10-face.el
  :ensure nil)
;;;; TeX
(use-package init-30-tex.el
  :ensure nil)
;;;; Buit-in package configuration
(use-package init-01-builtin.el
  :ensure nil)
;;;; Org-mode configuration
(use-package init-31-org.el
  :ensure nil)
;;;; General Improvement
;;;;; Sentences end with a single space
(setq sentence-end-double-space nil)
;;;;; Allow access to content from clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;;;;; Customize file
(setq custom-file (expand-file-name "site-lisp/custom/custom.el" user-emacs-directory))
;;;;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;;;;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;;;; Keyboard layout
(add-hook 'after-init-hook #'modremap)
;;;;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;; Packages
;;;; General interface
;;;;; Helpful extras
;;;;;; Helpful
(use-package helpful
  :bind
  ("C-h v" . helpful-variable)
  ("C-h M" . helpful-macro)
  ("C-h o" . helpful-symbol)
  ("C-h c" . helpful-command)
  ("C-h C-k" . helpful-kill-buffers)
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-function))
(put 'dired-find-alternate-file 'disabled nil)
;;;;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))
;;;; Completions & Navigation
;;;;; Completions
;;;;;; Selectrum
(use-package selectrum
  :hook (after-init . selectrum-mode)
  :bind (:map ctl-x-map
	      ("C-f" . find-file))
  :custom
  ;; Show as many candidates as possible
  (selectrum-fix-vertical-window-height selectrum-max-window-height)
  :ensure t)
;;;;;; Consult
(use-package consult
  :load 00-consult
  :init (define-prefix-command '00/consult-navigate-map)
  :bind-keymap ("s-a" . 00/consult-navigate-map)
  :bind
  (:map 00/consult-navigate-map
	("e" . consult-find-emacs-dir)
	("s" . consult-find-site-lisp)
	("g" . consult-find-git-dir)
	("p" . consult-grep-package))
  (:map ctl-x-map
	("b" . consult-buffer)))
;;;;;; Embark
(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  (:map embark-url-map
	("s" . #'browse-url-xdg-open)
	("m" . #'mpv-play-url))
  (:map embark-symbol-map
	("h" . #'helpful-at-point)))
;;;;;; Orderless
(use-package orderless
  :after selectrum
  :ensure t
  :custom
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (completion-styles '(orderless partial-completion)))
;;;;;; Marginalia
(use-package marginalia
  :config
  (marginalia-mode 1)
  :ensure t)
;;;;; Navigation
;;;;;; avy
(use-package avy
  :ensure t)
;;;; Application & utilities
;;;;; Password-store
(use-package pass
  :ensure t)
;;;;; Torrenting
(use-package transmission
  :config
  (customize-set-variable 'transmission-timer 30)
  :ensure t)
;;;;; Multimedia
;;;;;; MPV
(use-package mpv
  :ensure t) 
;;;;;;  LBRY
(use-package lbry-mode.el
  :load-path "site-lisp/lbry-mode/"
  :ensure nil)
;;;;;;  parallel-mode.el
(use-package parallel-mode.el
  :ensure nil
  :load-path "site-lisp/parallel/")
;;;;;;  Magit
(use-package magit
  :requires with-editor tramp 
  :ensure t
  :bind ("M-g ." . magit))
;;;;; Notes
;;;;;; Nov.el
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :ensure t)
;;;;;; PDFs
(use-package pdf-tools
  :load 00-pdf-avy-highlight
  :init (pdf-loader-install)
  :bind (:map pdf-view-mode-map
	      ("a k" . pdf-keyboard-highlight))
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.1)
  :ensure avy)
(use-package pdf-view-restore
  :ensure t)
;;;; Language settings for prose and code
;;;;; Yasnippet
(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  (yas-after-exit-snippet . save-buffer)
  :config
  (yas-load-directory (concat user-emacs-directory "snippets"))
  :ensure t)
;;;;; Markdown
(use-package markdown-mode
  :disabled
  :init (setq markdown-command "multimarkdown")
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure t)
;;;;; Scheme
(use-package quack
  :ensure t)
(use-package geiser
  :ensure t)
(use-package geiser-mit
  :after geiser
  :ensure t)
(use-package paredit
  :ensure t)
;;;;; Lisp (SLIME)
(use-package cl-generic
  :ensure t)
(use-package slime
  :load slime-autoloads
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy))
  :defer t)
(use-package emms
  :ensure t)
)
