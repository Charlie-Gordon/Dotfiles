;;; * Essential External Programs
(load "~/.emacs.d/external/stuffs.el" t)
;;; * Emacs initialization
;;;; ** Use-package
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
;;;; ** Setting load-path
(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))
;;;; ** Utilities functions
(use-package init-00-utils.el
  :after magit
  :ensure nil)
;;;; ** Interface tweaks
(use-package init-10-face.el
  :ensure nil)
;;;; ** LaTeX
(use-package init-30-tex.el
  :ensure nil)
;;;; ** Org-mode configuration
(use-package init-31-org.el
  :ensure nil)
;;;; ** General Improvement
;;;;; Sentences end with a single space
(setq sentence-end-double-space nil)
;;;;; Allow access to content from clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;;;;; Customize file
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
;;;;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;;;;; Keyboard layout
(add-hook 'after-init-hook 'modremap)
;;; * Packages
;;;; Completions
;;;;; Selectrum
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1)
  :bind (:map ctl-x-map
	      ("C-f" . find-file)))
;;;;; Consult
(use-package consult
  :bind (:map ctl-x-map
	      ("b" . consult-buffer)))
;;;;; Embark
(use-package embark
  :ensure t
  :bind
  ("C-x ." . embark-act))
;;;;; Orderless
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless partial-completion)))
;;;;; Marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))
;;;;; Embark-consult
(use-package embark-consult
  :ensure t
  :after (embark consult))
;;;; LSP
(use-package lsp-mode
  :disabled
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (emacs-lisp-mode-hook . lsp)
  :commands lsp)
;;;; EMMS
(use-package emms
  :ensure t
  (emms-all)
  (emms-default-players)
  :custom
  (emms-source-file-default-directory "~/Music/"))
;;;; Yasnippet
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (yas-global-mode 1))
;;;; shrface
(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)
  :custom (shrface-href-versatile t))
;;;; EWW
(use-package eww
  :defer t
  :init
;;  (add-hook 'eww-after-render-hook #'shrface-mode)
  (add-hook 'eww-mode-hook
            (lambda()
	      (add-hook 'text-scale-mode-hook
			'text-scale-mode-hook
			nil :local))))
;;;; Nov.el
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link)))
;;;; pdf-tools & restore functions
(use-package pdf-tools
  :init
  (pdf-tools-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
	      ("g"  . pdf-view-first-page)
	      ("G"  . pdf-view-last-page)
	      ("l"  . image-forward-hscroll)
	      ("h"  . image-backward-hscroll)
	      ("j"  . pdf-view-next-page)
	      ("k"  . pdf-view-previous-page)
	      ("e"  . pdf-view-goto-page)
	      ("y"  . pdf-view-kill-ring-save)
	      ("i"  . pdf-misc-display-metadata)
       	      ("o"  . pdf-occur)
	      ("b"  . pdf-view-set-slice-from-bounding-box)
	      ("r"  . pdf-view-reset-slice)
	      ("R"  . pdf-view-revert-buffer)
	      ("C-s" . isearch-forward)
	      ("C-c C-c" . image-toggle-display))
  :custom
  (yas-minor-mode nil)
  (pdf-cache-image-limit 32)
  (pdf-view-max-image-width 2048)
  (pdf-view-resize-factor 1.8)
  (pdf-isearch-batch-mode t)
  (pdf-annot-activate-created-annotations t))
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))
;;;; with-editor
(use-package with-editor)
;;;; tramp
(use-package tramp
  :config
  ;; Thanks u/baltakatei on r/emacs subreddit for getting magit to work with yadm(my dotfiles manager)
;; https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
  (add-to-list 'tramp-methods
	       '("yadm"
		 (tramp-login-program "yadm")
		 (tramp-login-args (("enter")))
		 (tramp-login-env (("SHELL") ("/bin/sh")))
		 (tramp-remote-shell "/bin/sh")
		 (tramp-remote-shell-args ("-c"))))
  :ensure nil)
;;;; Magit
(use-package magit
  :requires with-editor tramp 
  :ensure t)
;;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))
;;;; Helpful
(use-package helpful
  :bind
  ("C-h v" . helpful-variable)
  ("C-h o" . helpful-symbol)
  ("C-h c" . helpful-command)
  ("C-h C-k" . helpful-kill-buffers)
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-function))
(put 'dired-find-alternate-file 'disabled nil)

;;;; Highlighting
(use-package highlight
  :disabled
  :bind (:map ctl-x-map
	      ("y" . hlt-highlight)
	      ("<mouse-2>" . hlt-highlighter)
	      ("S-<mouse-2>" . hlt-eraser)
	      ("S-C-p" . hlt-previous-highlight)
	      ("S-C-n" . hlt-next-highlight)))

;;;; EXWM
(use-package exwm
  :config
  (use-package exwm-config
    :ensure nil
    :custom
;; Prefix keys to ignore
    (exwm-input-prefix-keys `(?\C-x
				?\C-u
				?\C-h
			        ?\C-g
			        ?\M-x
				?\M-`
				?\M-&
				?\M-:))
;; Global keys for EXWM
    (exwm-input-global-keys `(([?\s-.] . reload-emacs-configuration)
				([?\s-w] . exwm-workspace-switch)
				([?\s-&] . (lambda (command)
					     (interactive (list (read-shell-command "$ ")))
					     (start-process-shell-command command nil command)))
				([?\s-d] . modus-themes-toggle)
				([?\s-s] . magit-status-dotfiles)
				(,(kbd "s-<return>") . eshell)
				,@(mapcar (lambda (i)
					    `(,(kbd (format "s-%d" i)) .
					      (lambda ()
						(interactive)
						(exwm-workspace-switch-create ,i))))
					  (number-sequence 0 9))))
;; Line-editing keybindings for X windows
    (exwm-input-simulation-keys '(([?\C-b] . [left])
				  ([?\C-f] . [right])
				  ([?\C-p] . [up])
				  ([?\C-n] . [down])
				  ([?\C-a] . [home])
				  ([?\C-e] . [end])
				  ([?\M-v] . [prior])
				  ([?\C-v] . [next])
				  ([?\C-d] . [delete])
				  ([?\C-k] . [S-end C-x])
				  ([?\C-y] . [C-v])))
;; Set the initial workspace number
    :config    
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 4))
;; Fix Ido
    (exwm-config-ido)
    (ido-mode 0)
;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
	      (lambda ()
      	(exwm-workspace-rename-buffer exwm-class-name)))
;; Firefox keybindings
    (add-hook 'exwm-manage-finish-hook  (lambda ()
					  (when (and exwm-class-name
						     (string= exwm-class-name "Firefox"))
					    (exwm-input-set-local-simulation-keys
					     `( ,@exwm-input-simulation-keys
						([?\C-\M-j] . ,(kbd "C-<tab>"))
						([?\C-\M-k] . ,(kbd "C-S-<tab>"))
						([?\C-\M-h] . ,(kbd "M-<left>"))
						([?\C-\M-l] . ,(kbd "M-<right>"))
						([?\C-l] . [f6])
						([?\C-q] . ?\C-w)
						([?\C-w] . ?\C-c)
						([?\C-y] . ?\C-v)
						([?\C-s] . ?\C-f)
      						([?\C-\S-s] . ?\C-g)
						([?\C-m] . ?\')
						([?\M-a] . [C-home])
						([?\M-e] . [C-end])
						([?\M-f] . ,(kbd "C-<left>"))
       						([?\M-b] . ,(kbd "C-<right>"))
						(,(kbd "C-/") . [?\C-z]))))))    
;; Prefered minibuffer    
    (selectrum-mode +1)
;; Enable EXWM    
    (exwm-enable)))
