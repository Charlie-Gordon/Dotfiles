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
	      ("E" . dired-eww-open-file)
              ("=" . dired-ediff))
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

(defun dired-ediff ()
   "Run ediff-files on a pair of files marked in dired buffer"
   (interactive)
   (let* ((marked-files (dired-get-marked-files nil nil))
          (other-win (get-window-with-predicate
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (and (not (eq window (selected-window)))
                               (eq major-mode 'dired-mode))))))
          (other-marked-files (and other-win
                                   (with-current-buffer (window-buffer other-win)
                                     (dired-get-marked-files nil)))))
     (cond ((= (length marked-files) 2)
            (ediff-files (nth 0 marked-files)
                         (nth 1 marked-files)))
           ((and (= (length marked-files) 1)
                 (= (length other-marked-files) 1))
            (ediff-files (nth 0 marked-files)
                         (nth 0 other-marked-files)))
           ((= (length marked-files) 1)
            (let ((single-file (nth 0 marked-files))) 
              (ediff-files single-file
                           (read-file-name
                            (format "Diff %s with: " single-file)
                            nil (m (if (string= single-file (dired-get-filename))
                                       nil
                                     (dired-get-filename))) t))))
           (t (error "mark no more than 2 files")))))

(use-package dired-hist
  :straight '(dired-hist :type git
                         :host github
                         :repo "karthink/dired-hist")
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward))
  :hook (dired-mode . dired-hist-mode))

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
