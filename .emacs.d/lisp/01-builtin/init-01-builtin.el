;;; init-01-buitin.el --- Configuration for Emacs built-in packages  -*- lexical-binding: t; -*-
;;;; Directory
;;;;; Dired
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
;;;;;; dired-x
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
;;;;;; dired-subtree
(use-package dired-subtree
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle))
  :ensure t)
;;;;;; ls-lisp 
(use-package ls-lisp
  :custom
  (ls-lisp-use-insert-directory-program t)
  (insert-directory-program "gnuls")
  :ensure nil)

;;;; Outline
(use-package outline
  :init
  (defvar outline-minor-mode-prefix "\M-o")
  :config
  ;; Customize the distracting folding markers.
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot standard-display-table 'selective-display
			  (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
			    (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))
  ;; Functions
  (use-package 01-outline
    :config
    ;; Python mode-specific Outline folding.
    (add-hook 'python-mode-hook 'outline-python)
    :ensure nil)
  :ensure nil)
;;;; Application and Utilities
;;;;; TRAMP
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

;;;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww)
;;;;;; browse-url
(use-package browse-url
  :ensure t
  :custom
  (browse-url-browser-function '(("youtu\\.?be" . mpv-play-url)
				 ("." . eww-browse-url)))
  (browse-url-secondary-browser-function 'browse-url-default-browser))
;;;;;; shr
(use-package shr
  :custom
  (shr-max-image-proportion 0.6)
  (shr-discard-aria-hidded t)
  (shr-image-animate nil)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-width nil)
  (shr-cookie-policy nil)
  :ensure nil)
;;;;;; eww 
(use-package eww
  :bind
  (:map eww-mode-map
	("<return>" . eww-follow-link)
	("W" . mpv-play-url)
	("L" . eww-list-bookmarks))
  (:map eww-link-keymap
	("v" . nil)) ;; stop overriding `eww-view-source'
  (:map eww-buffers-mode-map
	("d" . eww-bookmark-kill))
  (:map eww-bookmark-mode-map
	("d" . eww-bookmark-kill))
  :custom
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)")
  (eww-download-directory (expand-file-name "~/Downloads/eww/"))
  (eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (eww-header-line-format nil)
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  :config
  (define-prefix-command 'prot/eww-map)
  ;; Prot's functions
  (use-package 01-eww
    :bind-keymap ("s-e" . prot/eww-map)
    :bind
    (:map prot/eww-map
	  ("b" . prot/eww-visit-bookmark)
	  ("e" . prot/eww-browse-dwim)
	  ("a" . prot/eww-search-arch-wiki)
	  ("A" . prot/eww-search-arch-aur)
	  ("d" . prot/eww-search-debbugs)
	  ("w" . prot/eww-search-wikipedia)
	  ("s" . prot/eww-search-engine))
    (:map eww-mode-map
	  ("B" . prot/eww-bookmark-page)
	  ("D" . prot/eww-download-html)
	  ("F" . prot/eww-find-feed)
	  ("b" . prot/eww-visit-bookmark)
	  ("e" . prot/eww-browse-dwim)
	  ("o" . prot/eww-open-in-other-window)
	  ("E" . prot/eww-visit-url-on-page)
	  ("J" . prot/eww-jump-to-url-on-page)
	  ("R" . prot/eww-readable))
    :ensure nil)
  :defer t)

;;;; Language setting for prose and coding
;;;;; Parentheses (show-paren-mode) (paren-face)
(use-package paren-face
  :config
  (global-paren-face-mode)
  :ensure t)
(add-hook 'after-init-hook #'show-paren-mode)
(add-hook 'after-init-hook #'electric-pair-mode)



(provide 'init-01-builtin.el)
;;; init-01-builtin.el ends here
