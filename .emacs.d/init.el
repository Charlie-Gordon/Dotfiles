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
;;;;; Buit-in package configuration
(use-package init-builtin :ensure nil)
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
;;;;; General Improvement
;;;;;; Sentences end with a single space
(setq sentence-end-double-space nil)
;;;;;; Allow access to content from clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;;;;;; Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;;;;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;;;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;;;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;;;;;; Keyboard layout
(add-hook 'after-init-hook #'modremap)
;;;;;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; General interface
;;;;;; Helpful extras
;;;;;;  Helpful
(use-package helpful
  :straight t
  :bind
  ("C-h v" . helpful-variable)
  ("C-h M" . helpful-macro)
  ("C-h o" . helpful-symbol)
  ("C-h c" . helpful-command)
  ("C-h C-k" . helpful-kill-buffers)
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-function))
;;;;;;  Which-key
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode)
  :diminish which-key-mode)
;;;;; Completions & Navigation
;;;;;; Completions
;;;;;;;  Selectrum
(use-package selectrum
  :straight t
  :custom
  (selectrum-fix-vertical-window-height selectrum-max-window-height "Show as many candidates as possible")
  :hook (after-init . selectrum-mode))
;;;;;;;  Consult
(use-package consult
  :straight t
;;  Replacing functions with their consult counterparts
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

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
;;;;;;; affe
(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches))
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
(use-package lbry-mode.el
  :disabled
  :ensure nil
  :load-path "lisp/lbry-mode/")
;;;;;; parallel-mode.el
(use-package parallel-mode.el
  :disabled
  :ensure nil
  :load-path "lisp/parallel/")
;;;;;; Magit
(use-package magit
  :straight t
  :requires with-editor tramp)
;;;;;; Note
;;;;;; Nov.el
(use-package nov
  :straight t
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :mode (("\\.epub\\'" . nov-mode)))
;;;;;; PDFs
(use-package pdf-tools
  :straight avy
  :bind (:map pdf-view-mode-map
	      ("a k" . pdf-keyboard-highlight))
  :init
  (pdf-loader-install)
  (use-package pdf-view-restore
    :straight t
    :custom
    (pdf-view-restore-filename "/storage/journals/library/.pdf-view-restore"))
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.1))

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
;;;;;;; Scheme

(use-package quack
  :straight t)
(use-package geiser
  :straight t
  :config
  (use-package geiser-mit :straight t))
(use-package paredit
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (ielm-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode)
  :straight t)
;;;;;;; Common-lisp

(use-package cl-generic
  :straight t)
(use-package slime
  :custom
  (inferior-lisp-program "/usr/local/bin/sbcl")
  :defer t
  :config
  (use-package slime-autoloads :ensure nil)
  (slime-setup '(slime-fancy)))

)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
