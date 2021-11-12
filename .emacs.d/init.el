;;; Use-package
;;;; Initialize
(package-initialize)
;;;; Add package sources
(unless (assoc-default "melpa" package-archives) 
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
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
  :ensure nil)

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
