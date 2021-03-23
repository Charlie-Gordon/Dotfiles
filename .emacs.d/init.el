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
(require 'init-00-utils.el)
;;;; ** Interface tweaks
(require 'init-10-face.el)
;;;; ** LaTeX
(require 'init-30-tex.el)
;;;; ** Org-mode
(require 'init-31-org.el)
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
;;;;; Orderless
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless partial-completion)))
;;;;; Marginalia
(use-package marginalia
  :ensure t
  :config (marginalia-mode 1))
;;;; EXWM
(use-package exwm
  :config
  (use-package exwm-config
    :ensure nil
    :config
    (exwm-config-example)
;;;;; Set the initial workspace number
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 4))
;;;;; Make class name the buffer name
      (add-hook 'exwm-update-class-hook
		(lambda ()
		  (exwm-workspace-rename-buffer exwm-class-name)))
;;;;; Prefix keys to ignore
    (setq exwm-input-prefix-keys
	  `(?\C-x
	    ?\C-u
	    ?\C-h
	    ?\C-g
	    ?\M-x
	    ?\M-`
	    ?\M-&
	    ?\M-:))
;;;;; Global keys for EXWM
    (setq exwm-input-global-keys
          `(([?\s-.] . reload-emacs-configuration)
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
	    ([?\s-d] . modus-themes-toggle)
	    (,(kbd "s-<return>") . eshell)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
			    (interactive)
                           (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))
;;;;; Simulate Emacs keybindings for X windows    
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
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
;;;;;;; Special keybindings for Firefox
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
;;;;; Prefered minibuffer    
    (selectrum-mode +1)
;;;;; Enable EXWM    
    (exwm-enable)))
;;;; Yasnippet
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (yas-global-mode 1))
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
;;;; Magit
(use-package magit
  :requires with-editor
  :ensure t)
;;;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
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

;;;; Bugs
(use-package bug-hunter)
;;;; Highlighting
(use-package highlight
  :disabled
  :bind (:map ctl-x-map
	      ("y" . hlt-highlight)
	      ("<mouse-2>" . hlt-highlighter)
	      ("S-<mouse-2>" . hlt-eraser)
	      ("S-C-p" . hlt-previous-highlight)
	      ("S-C-n" . hlt-next-highlight)))

