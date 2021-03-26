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
;;;; ** Org-mode
(require 'init-31-org.el)
;;;; ** General Improvement
;;;;; Sentences end with a single space
(setq sentence-end-double-space nil)
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
;;; * Packages
;;;; Completions
;;;;; Selectrum
(use-package selectrum
  :ensure t
  :config  (selectrum-mode +1))
;;;;; Embark
(use-package embark
  :ensure t)
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
;;;; Yasnippet
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (yas-global-mode 1))
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

;;;; Highlighting
(use-package highlight
  :disabled
  :bind (:map ctl-x-map
	      ("y" . hlt-highlight)
	      ("<mouse-2>" . hlt-highlighter)
	      ("S-<mouse-2>" . hlt-eraser)
	      ("S-C-p" . hlt-previous-highlight)
	      ("S-C-n" . hlt-next-highlight)))

