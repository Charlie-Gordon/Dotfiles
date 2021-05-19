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
(let* ((path (expand-file-name "lisp" user-emacs-directory))
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
;;;;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;;;; Keyboard layout
(add-hook 'after-init-hook #'modremap)
;;; Packages
;;;; Completions & Navigation
;;;;; Completions
;;;;;; Selectrum
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1)
  :bind (:map ctl-x-map
	      ("C-f" . find-file)))
;;;;;; Consult
(use-package consult
  :bind (:map ctl-x-map
	      ("b" . consult-buffer)))
;;;;;; Embark
(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  (:map embark-url-map
	("s" . browse-url-xdg-open)
	("m" . mpv-play-url)))
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
  :ensure t
  :config
  (marginalia-mode 1))
;;;;;; Embark-consult
(use-package embark-consult
  :after (embark consult)
  :ensure t)

;;;;; Navigation
;;;;;; avy
(use-package avy
  :ensure t)
;;;; Application & utilities
;;;;; Multimedia
;;;;;; ERC
(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("irc.highway.net" "#ebooks")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (use-package erc-dcc ;; DCC support
    :ensure nil)
  (use-package erc-image ;; Image module
    :ensure t)
  (use-package erc-hl-nicks
    :ensure t)
  :ensure nil)
;;;;;; LBRY
(use-package lbry-mode.el
  :load-path "lisp/lbry-mode/"
  :ensure nil)
;;;;;; parallel-mode.el
(use-package parallel-mode.el
  :ensure nil
  :load-path "lisp/parallel/")
;;;;;; Magit
(use-package magit
  :requires with-editor tramp 
  :ensure t
  :bind ("M-g ." . magit))
;;;;; Document
;;;;;; Nov.el
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :ensure t)
;;;;;; PDFs
(use-package pdf-tools
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
  :ensure t)
(use-package pdf-view-restore
  :ensure t)
(use-package pdf-avy-highlight
  :requires avy
  :ensure nil)
;;;; Language settings for prose and code
;;;;; Bookmarks
(use-package bm
  :bind-keymap ("C-c m" . bm-show-mode-map)
  :bind (:map bm-show-mode-map
	      ("m" . bm-toggle)
	      ("n" . bm-next)
	      ("p" . bm-previous)
	      ("L" . bm-show-all)
	      ("l" . bm-show)
	      ("s" . bm-save)
	      ("r" . bm-load-and-restore))
  ("<right-fringe> <mouse-5>" . bm-next-mouse)
  ("<right-fringe> <mouse-4>" . bm-previous-mouse)
  ("<right-fringe> <mouse-1>" . bm-toggle-mouse)
  :custom
  (bm-marker 'bm-marker-right)
;;  (bm-repository-file (concat emacs-persistence-directory "bm-repository"))
  (bm-recenter t)
  (bm-highlight-style 'bm-highlight-only-line)
  :ensure t)
;;;;; Yasnippet
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (yas-global-mode 1))
;;;;; Markdown
(use-package markdown-mode
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
  :config
  (use-package slime-autoloads :ensure nil)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy))
   :defer t)
;;;; General interface
;;;;; Highlight
(use-package highlight
  :ensure t)
;;;;; EXWM
(use-package exwm
  :after init-00-utils.el
  :config
  (use-package exwm-config
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
    (exwm-input-simulation-keys '(;; Backward-char
				  ([?\C-b] . [left])
				  ;; Forward-char
				  ([?\C-f] . [right])
				  ;; Previous-line
				  ([?\C-p] . [up])
				  ;; Next-line
				  ([?\C-n] . [down])
				  ;; Beginning of the line
				  ([?\C-a] . [home])
				  ;; End of the line
				  ([?\C-e] . [end])
				  ;; Forward-word
				  ([?\M-f] . [C-right])
				  ;; Backward-word
				  ([?\M-b] . [C-left])
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
    ;; Proper modeline
    (add-hook 'exwm-input-input-mode-change-hook 'force-mode-line-update)
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
	      (lambda ()
      		(exwm-workspace-rename-buffer exwm-class-name)))
    ;; Prefered minibuffer
    (selectrum-mode +1)
    ;; Enable EXWM
    (exwm-enable)
    :ensure nil)
  :ensure t)
;;;;; Helpful extras
;;;;;; Helpful
(use-package helpful
  :bind
  ("C-h v" . helpful-variable)
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
