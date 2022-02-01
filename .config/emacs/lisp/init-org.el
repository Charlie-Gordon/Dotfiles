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
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-'" . nil))
  :custom
  (org-id-link-to-org-use-id t)
  (org-ctrl-k-protect-subtree t)
  (org-directory *org-dir*)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands t)
  (org-refile-targets '(("/storage/org/gtd/inbox.org" :maxlevel . 2)
                        ("/storage/org/gtd/org-gtd-tasks.org" :maxlevel . 2)))
  (org-image-actual-width nil)
  (org-todo-keywords
   '((sequence "NEXT(n)" "|" "DONE(d)" "CNCL(c)")))
  (org-format-latex-options
   '(:foreground auto
                 :background auto :scale 1.6
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-speed-commands '("w" . widen))
  (add-to-list 'org-speed-commands '("N" . org-narrow-to-element))
  (add-hook 'org-mode-hook #'(lambda nil
                               (add-hook 'before-save-hook #'zp/org-set-last-modified nil t)))
  (org-load-modules-maybe t)
  :hook
  (org-mode . visual-line-mode))

(defun org-babel-tangle-append (&optional arg target-file lang-re)
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle arg target-file lang-re)))

(use-package oc
  :ensure nil
  :custom
  (org-cite-global-bibliography `(,(expand-file-name "references.bib" *bibliography-dir*))))

(use-package org-contrib
  :straight t)

(use-package ol-bookmark
  :straight t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)))

(use-package request-deferred
  :straight t)

(use-package deferred
  :straight t)

(use-package org-make-toc
  :straight t)

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(straight-use-package '(asoc :type git :host github :repo "troyp/asoc.el"))
(straight-use-package 'doct)

(use-package org-capture-ref
  :after org
  :straight (org-capture-ref
             :type git
             :host github
             :repo "yantar92/org-capture-ref")
  :custom
  (org-capture-ref-headline-tags nil)
  (org-capture-ref-capture-target `(:file ,(expand-file-name "inbox.org" *gtd-dir*)))
  (org-capture-ref-capture-template-set-p t)
  (org-capture-ref-capture-template
   `(:group "org-capture-ref template"
            :type entry
            ,@org-capture-ref-capture-target
            :clock-in nil
            :jump-to-captured t
            :prepare-finalize (lambda () (org-babel-tangle-append nil (expand-file-name "references.bib" *bibliography-dir*) "bibtex"))
            :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
            :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
            :org-entry (lambda () (org-capture-ref-get-org-entry))
            :bibtex (lambda ()
                      (string-join `(":BIBTEX:"
                                     "#+begin_src bibtex"
                                     ,(org-capture-ref-get-bibtex-field :bibtex-string)
                                     "#+end_src"
                                     ":END:")
                                   "\n"))
            :template
            ("%{fetch-bibtex}* %?%{space}%{org-entry}%{bibtex}%{content}")
            :children (("Interactive + Content"
                        :keys "s"
                        :space " "
                        :content
                        (lambda () (bir-import (org-capture-ref-get-buffer-from-html-file-in-query))))
                       ("Interactive org-capture-ref template"
                        :keys ,(car org-capture-ref-capture-keys)
                        :space " ")
                       ("Silent org-capture-ref template"
                        :keys ,(cadr org-capture-ref-capture-keys)
                        :space ""
                        :immediate-finish t))))
  :config
  (let ((templates (doct org-capture-ref-capture-template)))
    (dolist (template templates)
      (asoc-put! org-capture-templates
                 (car template)
                 (cdr template)
                 'replace))))

(use-package org-super-agenda
  :straight t
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package edraw-org
  :ensure nil
  :mode
  ("\\.edraw\\.svg$" . edraw-mode))

(use-package org-special-block-extras
  :straight t
  :config
  (add-hook 'org-mode-hook #'org-special-block-extras-mode)
  (o-deflink extract
             ""
             [:face 'org-dispatcher-highlight
                    :follow (org-id-goto o-label)]))

(use-package org-edna
  :straight t)

(use-package org-gtd
  :straight t
  :after org org-edna
  :bind (:map org-gtd-map
              ("c" . org-gtd-choose)
              ("e" . org-gtd-engage)
              ("p" . org-gtd-process-inbox)
              ("C" . org-gtd-capture))
  :bind-keymap ("C-c g" . org-gtd-map)
  :init
  (define-prefix-command 'org-gtd-map)
  :custom
  (org-gtd-agenda-custom-commands
   '(("g" "Scheduled today and all NEXT items"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-start-day nil)))
       (todo "NEXT"
             ((org-agenda-overriding-header "All NEXT items")
              (org-agenda-prefix-format "%b %i")
              (org-agenda-compact-blocks t)
              (org-agenda-breadcrumbs-separator "⋅")))
       (todo "WAIT"
             ((org-agenda-todo-ignore-with-date t)
              (org-agenda-overriding-header "Blocked items"))))
      "")))
  (org-gtd-directory "/storage/org/gtd/")
  (org-gtd-process-item-hooks nil)
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode))

(defun c1/mark-as-project ()
  (interactive)
  (org-set-property "COOKIE_DATA" "todo recursive"))

(add-hook 'org-gtd-process-item-hooks #'c1/mark-as-project)

(defun c1/org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)

  (if (org-gtd--poorly-formatted-project-p)
      (org-gtd--show-error-and-return-to-editing)

    (org-gtd--decorate-item)
    (org-gtd-projects--nextify)
    (goto-char (point-min))
    (let ((org-special-ctrl-a t))
      (org-beginning-of-line))
    (insert "[/] ")
    (org-update-statistics-cookies t)
    (org-gtd--refile org-gtd-projects)
    (org-gtd-process-inbox)))

(advice-add 'org-gtd--project :override #'c1/org-gtd--project)


;;;; Recur

(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :straight t
  :disabled
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))


(use-package org-ql
  :straight t)

(use-package org-yt
  :straight '(org-yt :type git
                     :host github
                     :repo "TobiasZawada/org-yt"
                     :fork t))

(org-link-set-parameters
 "http"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "https"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "data"
 :image-data-fun #'org-inline-data-image)

(defun org-inline-data-image (_protocol link el &optional _description)
  "Interpret LINK as base64-encoded image data."
  (let ((image-data (base64-decode-string (replace-regexp-in-string "image/.*;base64," "" link))))
    (deferred:next
      (lambda () (save-excursion (org-image-update-overlay image-data el t t))))))

(defun org-image-link (protocol link el &optional description)
  "Interpret LINK as base64-encoded image data."
  (when (string-match-p (image-file-name-regexp) link)
    (deferred:$
      (deferred:url-retrieve (concat protocol ":" link))
      (deferred:nextc it
        (lambda (buf)
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n")
            (buffer-substring-no-properties (point) (point-max)))))
      (deferred:nextc it
        (lambda (image-data)
          (save-excursion (org-image-update-overlay image-data el t t))))
      (deferred:nextc it
        (lambda (overlay)
          (when (and overlay description)
            (overlay-put overlay 'after-string description)))))))

(provide 'init-org)
;;; init-org.el ends here
