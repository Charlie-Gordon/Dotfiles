;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  :straight t
  :custom
  (tramp-terminal-type "tramp")
  :config
  (setenv "SHELL" (executable-find "bash"))
  (unless (assoc-default "yadm" tramp-methods)
    (add-to-list 'tramp-methods
		 '("yadm"
		   (tramp-login-program "yadm")
		   (tramp-login-args (("enter")))
		   (tramp-login-env (("SHELL") ("/bin/sh")))
		   (tramp-remote-shell "/bin/sh")
		   (tramp-remote-shell-args ("-c"))))))


(use-package magit
  :straight t vc straight
  :when (executable-find "git")
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
                                  (,(expand-file-name "site-lisp/" user-emacs-directory) . 2))))

(use-package git-commit
  :straight t
  :hook
  (git-commit-mode . goto-address-mode))

(provide 'init-git)
;;; init-git.el ends here
