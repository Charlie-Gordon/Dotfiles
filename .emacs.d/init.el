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

(defconst *journals-dir* (if *termux*
			     "~/storage/shared/journals/"
			   "/storage/journals/"))

;; From Jordon Biondo at
;; https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up
(let ((file-name-handler-alist nil))

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
;;;;; Early local configuration
(use-package init-preload-local :ensure nil)
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
(use-package init-org :ensure nil :termux)

;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Individual configuration
;;;;;; Repeat mode
(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))
;;;;;; Eldoc
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))
;;;;;; Emacsclient
(add-hook 'after-init-hook
          '(lambda ()
             (require 'server)
             (unless (server-running-p)
               (server-start))))
;;;;;; Load customize'd variables
(when (file-exists-p custom-file)
  (load custom-file))
;;;;; Builtin packages
;;;;;; Dired
(use-package init-dired :ensure nil)
;;;;;; Outline
(use-package init-outline :ensure nil)
;;;;;; EWW & SHR
(use-package init-eww-shr :ensure nil)
;;;;;; ERC
(use-package init-erc :ensure nil)
;;;;; Completions
(use-package init-completion :ensure nil :termux)
;;;;; Git
(use-package init-git :ensure nil)
;;;;; Lisp
(use-package init-paredit :ensure nil)
(use-package init-lisp :ensure nil)
;;;;; Miscellaneous
(use-package init-misc :ensure nil)
(use-package init-notetake :ensure nil)
(use-package init-editing-utils :ensure nil :termux)
(use-package init-local :ensure nil)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; no-byte-compile: t
;; End:
(provide 'init)
;;; init.el ends here
