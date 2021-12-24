;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  :straight t
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

(global-set-key (kbd "C-x .") #'yadm-status)

(defun yadm-stage ()
  (interactive)
  (let ((file
         (let ((default-directory "~/"))
           (read-file-name "Stage file: "))))
    (if (equal (expand-file-name file)
               (expand-file-name "~/.yadm/"))
        (user-error "Can't stage yadm dir itself.")
      (magit-with-toplevel
        (magit-stage-1 nil (list file))))))


(use-package magit
  :straight t 
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

(use-package magit-todos
  :straight t
  :hook
  (magit-mode . magit-todos-mode))


(use-package git-commit
  :straight t
  :hook
  (git-commit-mode . goto-address-mode))

(defun yadm--files ()
  (let ((default-directory "~/"))
    (cl-delete-if-not
     #'file-exists-p
     (process-lines "yadm" "ls-tree" "--full-tree" "-r" "--name-only" "HEAD"))))

(defun yadm-find-file ()
  (interactive)
  (let ((default-directory  "~/"))
    (find-file
     (completing-read "Yadm file: " (yadm--files)))))

(defun yadm-dired ()
  (interactive)
  (let ((default-directory "~/"))
    (with-current-buffer (dired `("*yadm*" ,@(yadm--files)))
      (setq-local revert-buffer-function
                  (lambda (&rest args)
                    (setq dired-directory
                          (cons (car dired-directory)
                                (yadm--files)))
                    (apply #'dired-revert args))))))

(defun yadm-dired-jump (&optional other-window)
  (interactive "P")
  (let ((default-directory "~/"))
    (dired-jump other-window
                (when-let ((file (magit-file-at-point)))
                  (expand-file-name (if (file-directory-p file)
                                        (file-name-as-directory file)
                                      file))))))

(define-minor-mode yadm-minor-mode
  "A minor mode for magit yadm buffers."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x d") 'yadm-dired)
            (define-key map (kbd "C-x C-f") 'yadm-find-file)
            (define-key map (kbd "C-x C-j") 'yadm-dired-jump)
            (define-key map "s" 'yadm-stage)
            map))

(defun yadm-status ()
  (interactive)
  (with-current-buffer (magit-status "/yadm::")
    (yadm-minor-mode 1)))


(provide 'init-git)
;;; init-git.el ends here
