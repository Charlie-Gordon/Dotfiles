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
  :bind (:map dired-mode-map
	      ("E" . #'eww-open-file))
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
  :load 01-outline
  :init (defvar outline-minor-mode-prefix "\M-o")
  :config
  ;; Customize the distracting folding markers.
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot standard-display-table 'selective-display
			  (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
			    (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))
  ;; Python mode-specific Outline folding.
  (add-hook 'python-mode-hook 'outline-python)
  :ensure nil)
;;;; Application and Utilities
;;;;; ERC
(use-package erc
  :custom
  (erc-paranoia t)
  (erc-autojoin-channels-alist '(("irc.highway.net" "#ebooks")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (use-package erc-dcc ;; DCC support
    :ensure nil)
  (use-package erc-image ;; Image module
    :ensure t)
  (use-package erc-hl-nicks
    :ensure t)
  :ensure nil)
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
  (shr-width 70)
  (shr-cookie-policy nil)
  :ensure nil)
;;;;;; eww 
(use-package eww
  :load 01-eww
  :init (define-prefix-command 'prot/eww-map) ;; Keymapping for Protesilaos's extensions
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
	("<return>" . eww-follow-link)
	("W" . mpv-play-url)
	("L" . eww-list-bookmarks)
	("t" . eww-readable)
	("n" . shr-next-link)
	("p" . shr-next-link)
	("u" .  eww-back-url)
	("B" . prot/eww-bookmark-page)
	("D" . prot/eww-download-html)
	("F" . prot/eww-find-feed)
	("b" . prot/eww-visit-bookmark)
	("e" . prot/eww-browse-dwim)
	("o" . prot/eww-open-in-other-window)
	("E" . prot/eww-visit-url-on-page)
	("J" . prot/eww-jump-to-url-on-page)
	("R" . prot/eww-readable))
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
  :ensure nil)

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
