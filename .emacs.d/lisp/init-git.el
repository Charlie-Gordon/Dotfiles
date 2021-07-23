;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  :ensure nil)

(use-package magit
  :straight t
  :if (executable-find "git")
  :bind
  (("C-x g g" . magit-status)
   ("C-x g l" . magit-list-repositories)
   :map magit-status-mode-map
   ("C-M-<up>" . magit-section-up)
   ("M-<return>" . magit-diff-visit-file-other-window))
  :custom
  (magit-diff-refine-hunk t)
  (magit-repository-directories `(("~/git/" . 2)
                                  (,(expand-file-name "straight/" straight-base-dir) . 2)
                                  (,(expand-file-name "site-lisp/" user-emacs-directory) . 2)))
  (use-package vc
    :straight t)
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
