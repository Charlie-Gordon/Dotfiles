;;; init-org.el --- Code for initializing org-mode -*- lexical-binding: t; -*-
;;;; Org
;;----------------------------------------------------------------------------
;; Updating time-based metadata
;;----------------------------------------------------------------------------
(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

(use-package org
  :termux
  :straight t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 :map org-mode-map
	 ("C-'" . nil))
  :custom
  (org-directory *org-dir*)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands nil)
  (org-refile-target '((org-agenda-files . (:maxlevel . 6))))
  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "QUIZ(q)" "|" "FOR_ZETTEL(d)" "ZETTEL'ED")
     (sequence "REVIEW(r)" "|" "ANSWERED(d)" "ANKIFIED(a)")))
  (org-todo-keyword-faces
   '(("QUIZ" . "Skyblue1")
     ("FOR_ZETTEL" . "SkyBlue3")
     ("ZETTEL'ED" . "SkyBlue4")
     ("ANSWERED" . "PaleGreen2")
     ("ANKIFIED" . "PaleGreen4")))
  (org-image-actual-width nil)
  (org-format-latex-options
   '(:foreground default
                 :background default :scale 1.6
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (add-hook 'org-mode-hook #'(lambda nil
                               (add-hook 'before-save-hook #'zp/org-set-last-modified nil t)))
  :hook
  (org-mode . visual-line-mode))



(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)))

(use-package worf
  :straight t)


(use-package org-edna
  :straight t
  :hook
  (org-mode . org-edna-mode)
  :config
  (defun org-edna-action/org-anki-this! (_last-entry)
    "Action to call `org-anki-sync-entry' on this entry
Edna Syntax: org-anki-this!"
    (org-anki-sync-entry))
  :diminish)

(use-package org-make-toc
  :straight t)

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-agenda
  :ensure nil
  :config
  ;; Daniel Patru's answer at
  ;; https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take
  (defun org-get-agenda-files-recursively (dir)
    "Get org agenda files from root DIR."
    (directory-files-recursively dir "\.org$"))
  (defun org-set-agenda-files-recursively (dir)
    "Set org-agenda files from root DIR."
    (setq org-agenda-files 
	  (org-get-agenda-files-recursively dir)))
  (defun org-add-agenda-files-recursively (dir)
    "Add org-agenda files from root DIR."
    (nconc org-agenda-files 
	   (org-get-agenda-files-recursively dir)))
  (add-hook 'after-init-hook (lambda nil (org-set-agenda-files-recursively *org-dir*)))
  
  (setq org-agenda-custom-commands
        '(("R" "List of all headline with REVIEW keyword." search "REVIEW"
           ((org-show-context-detail 'minimal)
            (org-agenda-prefix-format ""))))))

(use-package org-super-agenda
  :straight t
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package edraw-org
  :ensure nil
  :mode
  ("\\.edraw\\.svg$" . edraw-mode))


;;;; Recur

(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :straight t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))


(use-package org-ql
  :straight t)

;;;; Outshine
(use-package outshine
  :straight t
  :after outline
  :config
  ;; Required for outshine
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook 'outshine-mode)
  :diminish)

(provide 'init-org)
;;; init-org.el ends here
