;;; init-buitin.el --- Configuration for Emacs built-in packages  -*- lexical-binding: t; -*-
;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))
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

;;;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww)
;;;;;; browse-url
(use-package browse-url
  :straight t
  :custom
  (browse-url-handlers '(("youtu\\.?be" . mpv-play-url)
				 ("." . eww-browse-url)))
  (browse-url-secondary-browser-function 'browse-url-default-browser))
;;;;;; shr

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
