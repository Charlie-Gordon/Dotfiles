;;; init-buitin.el --- Configuration for Emacs built-in packages  -*- lexical-binding: t; -*-
;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))
;;;; Directory, buffer, window management 
;;;;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	      ("E" . eww-open-file))
  :custom
  (dired-use-ls-dired nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode))
;;;;;; dired-x
(use-package dired-x
  :disabled
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
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle))
  :straight t)
;;;;;; ls-lisp 
(use-package ls-lisp
  :custom
  (ls-lisp-use-insert-directory-program t)
  (insert-directory-program "gnuls")
  :ensure nil)
;;;;; Buffers
;;;;;; Ibuffer
;; Use `ibuffer' instead of `list-buffers'
(use-package ibuffer
  :bind (:map ctl-x-map
	      ("C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil "Do not open on the other window; use the current one.")
  (ibuffer-show-empty-filter-groups nil "Do not show empty filter groups.")
  (ibuffer-movement-cycle nil "Do not go to the top when moving downward at the last item on the list.")
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  :ensure nil)
;;;;; Window management
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(;; Top side window
     ("\\*Messages.*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 1))
     ("\\*Warnings.*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 0))
     ;; Right side window
     ("\\*[Hh]elp\*.*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0))
     ("\\*Faces\\*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0))
     ("\\qutebrowser.*"
      (display-buffer-in-side-window)
      (window-width . 0.45)
      (side . right)
      (slot . 0))
     ("\\mpv.*"
      (display-buffer-in-side-window)
      (window-height . 0.45)
      (side . right)
      (slot . 2))
     ;; Bottom buffer
     ("\\*.*\\(e?shell\\|v?term\\).*"
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.2))))
  (window-combination-resize t)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop))
;;;; Outline
(use-package outline
  :init (setq outline-minor-mode-prefix "\M-o")
  :config
  ;; Customize the distracting folding markers.
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot standard-display-table 'selective-display
			  (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
			    (vconcat (mapcar (lambda (c) (+ face-offset c)) "-"))))
  ;; Python mode-specific Outline folding.
  (add-hook 'python-mode-hook 'outline-python)
  (use-package init-outline :ensure nil)
  :ensure nil)
;;;; Application and Utilities
;;;;; ERC
(use-package erc
  :ensure nil
  :custom
  (erc-paranoia t)
  (erc-autojoin-channels-alist '(("irc.irchighway.net" "#ebooks")))
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
  (use-package erc-dcc
    :ensure nil
    :custom
    (erc-dcc-get-default-directory "/tmp/"))
  (use-package erc-hl-nicks :straight t))
;;;;; TRAMP
(use-package tramp
  :ensure nil
  :config
;; Thanks u/baltakatei on r/emacs subreddit for getting magit to work with yadm(my dotfiles manager)
;; https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
  (add-to-list 'tramp-methods
	       '("yadm"
		 (tramp-login-program "yadm")
		 (tramp-login-args (("enter")))
		 (tramp-login-env (("SHELL") ("/bin/sh")))
		 (tramp-remote-shell "/bin/sh")
		 (tramp-remote-shell-args ("-c")))))

;;;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww)
;;;;;; browse-url
(use-package browse-url
  :straight t
  :custom
  (browse-url-handlers '(("youtu\\.?be" . mpv-play-url)
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
  :ensure nil
  :bind
  (:map 01-prot-eww-map
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
  :bind-keymap ("C-' e" . 01-prot-eww-map)
  :init (define-prefix-command '01-prot-eww-map) ;; Keymapping for Protesilaos's extensions
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
  (use-package init-eww :ensure nil))

;;;; Language setting for prose and coding
;;;;; Eldoc
(use-package eldoc
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  :ensure nil)
;;;;; Parentheses (show-paren-mode) (paren-face)
(use-package paren-face
  :straight t
  :hook
  (after-init . show-paren-mode)
  (after-init . electric-pair-mode)
  :config
  (global-paren-face-mode))

(provide 'init-builtin)
;;; init-builtin.el ends here
