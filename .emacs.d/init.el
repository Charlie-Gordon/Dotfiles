;;; init.el --- Configuration for emacs             -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My personal configuration file for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defconst *termux* (string-match "Android" (shell-command-to-string "uname -a")))
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
(require 'init-straight)
(use-package init-use-package :ensure nil)
;;;;;; Local packages (on site-lisp/)
(use-package init-site-lisp :ensure nil)
;;;;; Utilities functions
(use-package init-utils :ensure nil)
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

;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Builtin packages
;;;;;; Dired
(use-package init-dired :ensure nil)
;;;;;; Outline
(use-package init-outline :ensure nil)
;;;;;; EWW & SHR
(use-package init-eww-shr :ensure nil)
;;;;;; ERC
(use-package init-erc :ensure nil)
;;;;;; Repeat mode
(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))
;;;;; Completions
(use-package init-completion :ensure nil)
;;;;; Git
(use-package init-git :ensure nil)
;;;;; Lisp
(use-package init-paredit :ensure nil)
(use-package init-lisp :ensure nil)
;;;;; Miscellaneous
(use-package init-notetake :ensure nil)
(use-package init-local :ensure nil)
(use-package init-editing-utils :ensure nil)
(use-package init-misc :ensure nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; no-byte-compile: t
;; End:
(provide 'init)
;;; init.el ends here
