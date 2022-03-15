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

;;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(straight-use-package '(org-fc :type git :host github :repo "c1-g/org-fc" :branch "org-roam"))
(setq org-directory "~/storage/shared/Org/")

(setq org-fc-directories (list org-directory))
