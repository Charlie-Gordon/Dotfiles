;;; init-dired.el --- Dired customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Freebsd's "ls" doesn't have --group-directories-first option like the coreutils version
(let ((gnuls (executable-find "gnuls")))
  (when gnuls (setq insert-directory-program gnuls)))

(defun dired-eww-open-file ()
  (interactive)
  (eww-open-file (dired-get-file-for-visit)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	      ("E" . dired-eww-open-file))
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

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle)))

(use-package diredfl
  :straight t
  :requires dired
  :config
  (diredfl-global-mode))

(use-package dired-x
  :ensure nil
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-x-hands-off-my-keys t)
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  :config
  (unless (assoc-default "mp4" dired-guess-shell-alist-default)
    (add-to-list 'dired-guess-shell-alist-default '("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'" "mpv"))))


(provide 'init-dired)
;;; init-dired.el ends here
