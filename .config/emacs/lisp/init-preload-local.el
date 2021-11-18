;;; init-preload-local.el --- Configuration that loads earlier than startup process -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow access to content from clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;; Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Backup files
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; From Mastering Emacs
;; https://masteringemacs.org/article/working-coding-systems-unicode-emacs
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Suppress warning when opening large files
(setq large-file-warning-threshold nil)

;; Compact modeline
(setq mode-line-compact 'long)
;; Enable disabled command
(setq disabled-command-function nil)

(provide 'init-preload-local)
;;; init-preload-local.el ends here
