;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight t
  :bind
  (("<f12>" . magit-status)
   :map ctl-x-map
   ("g g" . magit-status)
   ("g l" . magit-list-repositories)
   :map magit-status-mode-map
   ("C-M-<up>" . magit-section-up))
  :custom
  (magit-diff-refine-hunk t)
  (magit-repository-directories `(("~/Git/" . 2)
                                  (,(expand-file-name "straight/" straight-base-dir) . 2)))
  :init
  (use-package tramp :ensure nil)
  :config
  ;; Thanks u/baltakatei on r/emacs subreddit for getting magit to work with yadm
  ;; https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
  (unless (assoc-default "yadm" tramp-methods)
    (add-to-list 'tramp-methods
		 '("yadm"
		   (tramp-login-program "yadm")
		   (tramp-login-args (("enter")))
		   (tramp-login-env (("SHELL") ("/bin/sh")))
		   (tramp-remote-shell "/bin/sh")
		   (tramp-remote-shell-args ("-c"))))))

(use-package git-commit
  :straight t
  :hook
  (git-commit-mode . goto-address-mode))

(provide 'init-git)
;;; init-git.el ends here
