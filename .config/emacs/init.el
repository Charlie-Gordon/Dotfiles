;;; init.el --- Configuration for emacs             -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My personal configuration file for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;; Constant
(use-package init-const :ensure nil)
;;;;; Early local configuration
(use-package init-preload-local :ensure nil)
;;;;; Interface tweaks
(use-package init-face :ensure nil)
(use-package init-exwm :ensure nil)
;;;;; TeX
(use-package init-tex :ensure nil)
;;;;; Org-mode configuration
(use-package init-org :ensure nil :termux)
;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Individual configuration
;;;;;; Hyperbole
(use-package hyperbole
  :straight t)
;;;;;; Repeat mode
(use-package repeat
  :when (>= (string-to-number emacs-version) 28)
  :config
  (repeat-mode 1))
;;;;;; Eldoc
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook #'global-eldoc-mode))
;;;;;; Emacsclient
(add-hook 'after-init-hook
          #'(lambda ()
             (require 'server)
             (unless (server-running-p)
               (server-start))))
;;;;;; Load customized variables
(when (file-exists-p custom-file)
  (load custom-file))
;;;;; Builtin packages
;;;;;; Dired
(use-package init-dired :ensure nil)
;;;;;; Outline
(use-package init-outline :ensure nil)
;;;;;; Browser
(use-package init-browser :ensure nil)
;;;;;; ERC
(use-package init-erc :ensure nil)
;;;;; Completions
(use-package init-completion :ensure nil :termux)
;;;;; Git
(use-package init-git :ensure nil)
;;;;; Lisp
(use-package init-lisp :ensure nil)
;;;;; Miscellaneous
(use-package init-uniquify :ensure nil)
(use-package init-markdown :ensure nil)
(use-package init-misc :ensure nil)
(use-package init-notetake :ensure nil)
(use-package init-window :ensure nil)
(use-package init-eaf :ensure nil)
(use-package init-ibuffer :ensure nil)
(use-package init-editing-utils :ensure nil :termux)
(use-package init-local :ensure nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; no-byte-compile: t
;; End:
(provide 'init)
;;; init.el ends here

