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
  :config
  (unless (assoc-default "mp4" dired-guess-shell-alist-default)
    (add-to-list 'dired-guess-shell-alist-default '("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'" "mpv")))
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
