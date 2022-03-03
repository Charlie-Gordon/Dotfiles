;;; init-site-lisp.el --- Support elisp manually installed in the site-lisp dir -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; From https://www.emacswiki.org/emacs/LoadPath
;;
;; Adds subdirectories containing ".el" files to load-path inside the
;; directory user-emacs-directory/site-lisp. If user-emacs-directory/site-lisp does
;; not exist, create it. If already exists, then add the subdirs to
;; load-path.

(let* ((path (expand-file-name "site-lisp" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (lambda (pkg)
              (push pkg load-path))
            local-pkgs)
    (make-directory path :parents)))

(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
