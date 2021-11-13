;;; Use-package
;;;; Initialize
(package-initialize)
;;;; Add package sources
(unless (assoc-default "melpa" package-archives) 
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
;;;; Bootstrapping Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-verbose t)
;;;; Org-mode
(use-package org
  :ensure nil
  :config
  (setq org-directory "~/storage/shared/Org/"))

(use-package org-agenda
  :ensure nil
  :config
  ;; Daniel Patru's answer at
  ;; https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take
  (defun org-get-agenda-files-recursively (dir)
    "Get org agenda files from root DIR."
    (directory-files-recursively dir "\.org$"))
  (defun org-set-agenda-files-recursively (dir)
    "Set org-agenda files from root DIR."
    (setq org-agenda-files 
	  (org-get-agenda-files-recursively dir)))
  (defun org-add-agenda-files-recursively (dir)
    "Add org-agenda files from root DIR."
    (nconc org-agenda-files 
	   (org-get-agenda-files-recursively dir)))
  
  (add-hook 'after-init-hook (lambda nil (org-set-agenda-files-recursively org-directory))))


(use-package org-drill
  :ensure t)

;;;; General Improvement
;;; Interface tweaks
;;;; Remove lame startup screen
(setq inhibit-startup-message t)
;;;; UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;; Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
