;;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;; Sentences end with a single space
(setq sentence-end-double-space nil)
;;;;; Allow access to content from clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;;;;;  Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;;;;  Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;;;  Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;;;  Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;;;;;  Keyboard layout
(add-hook 'after-init-hook #'modremap)
;;;;;  Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-local)
;;; init-local.el ends here
