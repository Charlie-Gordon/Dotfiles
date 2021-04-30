;;; init-01-buitin.el --- Configuration for Emacs built-in packages  -*- lexical-binding: t; -*-
;;; dired
(use-package dired
  :custom
  (dired-use-ls-dired nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  :ensure nil)
;;; dired-x
(use-package dired-x
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-x-hands-off-my-keys t)
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  :config
  (unless (assoc-default "mp4" dired-guess-shell-alist-default)
    (add-to-list 'dired-guess-shell-alist-default '("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'" "mpv")))
  :ensure nil)
;;; dired-subtree
(use-package dired-subtree
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle))
  :ensure t)
;;; tramp
(use-package tramp
  :config
;; Thanks u/baltakatei on r/emacs subreddit for getting magit to work with yadm(my dotfiles manager)
;; https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
  (add-to-list 'tramp-methods
	       '("yadm"
		 (tramp-login-program "yadm")
		 (tramp-login-args (("enter")))
		 (tramp-login-env (("SHELL") ("/bin/sh")))
		 (tramp-remote-shell "/bin/sh")
		 (tramp-remote-shell-args ("-c"))))
  :ensure nil)
;;; ls-lisp
(use-package ls-lisp
  :custom
  (ls-lisp-use-insert-directory-program t)
  (insert-directory-program "gnuls")
  :ensure nil)
;;; eww
(use-package eww
  :defer t
  :init
  (add-hook 'eww-mode-hook
            (lambda()
	      (add-hook 'text-scale-mode-hook
			'text-scale-mode-hook
			nil :local)))
  :config
  (add-hook 'eww-after-render-hook 'eww--rename-buffer)
  (advice-add 'eww-back-url :after 'eww--rename-buffer)
  (advice-add 'eww-forward-url :after 'eww--rename-buffer)
  :bind (:map eww-mode-map
	      ("W" . mpv-play-url))
  :custom
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)")
  (eww-download-directory (expand-file-name "~/Downloads/eww/"))
  (eww-header-line-format "%t <%u>")
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t))
;;; shr
(use-package shr
  :ensure t
  :custom
  (shr-discard-aria-hidded t)
  (shr-image-animate nil)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-cookie-policy nil))
;;; browse-url
(use-package browse-url
  :ensure t
  :custom
  (browse-url-browser-function
   (quote
    (("youtu\\.?be" . mpv-play-url)
     ("." . eww-browse-url))))
  (browse-url-secondary-browser-function 'browse-url-default-browser))

(provide 'init-01-builtin.el)
;;; lisp-mode
(use-package paren-face
  :ensure t)
