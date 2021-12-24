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
         ("C-'" . nil)
         ("C-c l" . org-store-link))
  :custom
  (org-id-link-to-org-use-id t)
  (org-ctrl-k-protect-subtree t)
  (org-directory *org-dir*)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands t)
  (org-refile-target '((org-agenda-files . (:maxlevel . 6))))
  (org-image-actual-width nil)
  (org-format-latex-options
   '(:foreground default
                 :background "Transparent" :scale 1.6
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (add-hook 'org-mode-hook #'(lambda nil
                               (add-hook 'before-save-hook #'zp/org-set-last-modified nil t)))
  (org-load-modules-maybe t)
  :hook
  (org-mode . visual-line-mode))

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
  (org-capture-ref-capture-target '(:file "/storage/org/slip-box/lit/inbox.org"))
  (org-capture-ref-capture-template-set-p t)
  (org-capture-ref-capture-template
   `(:group "org-capture-ref template"
            :type entry
            ,@org-capture-ref-capture-target
            :clock-in nil
            :jump-to-captured t
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
            ("%{fetch-bibtex}* %?%{space}%{org-entry}%{bibtex}")
            :children (("Interactive org-capture-ref template"
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
  :hook (org-mode . org-special-block-extras-mode)
  :config
  (o-defblock extract
              (id "ID of the extract" :face 'org-dispatcher-highlight)
              nil
              contents)

  (o-deflink extract
             ""
             [:face 'org-dispatcher-highlight
                    :follow (org-id-goto o-label)]))


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
     (org-image-update-overlay image-data el t t)))

(defun org-image-link (protocol link el &optional description)
  "Interpret LINK as base64-encoded image data."
  (when (string-match-p (image-file-name-regexp) link)
    (save-excursion
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
            (org-image-update-overlay image-data el t t)))
        (deferred:nextc it
          (lambda (overlay)
            (when (and overlay description)
              (overlay-put overlay 'after-string description))))))))

(defun org-element-special-block-parser (limit affiliated)
  "Parse a special block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `special-block' and CDR is a plist
containing `:type', `:begin', `:end', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let* ((case-fold-search t)
         (type (progn (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)\\(?:[	 ]+\\(.+\\)\\)?")
                      (match-string-no-properties 1)))
         (arguments (match-string-no-properties 2)))
    (if (not (save-excursion
               (re-search-forward
                (format "^[ \t]*#\\+END_%s[ \t]*$" (regexp-quote type))
                limit t)))
        ;; Incomplete block: parse it as a paragraph.
        (org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
        (save-excursion
          (let* ((begin (car affiliated))
                 (post-affiliated (point))
                 ;; Empty blocks have no contents.
                 (contents-begin (progn (forward-line)
                                        (and (< (point) block-end-line)
                                             (point))))
                 (contents-end (and contents-begin block-end-line))
                 (pos-before-blank (progn (goto-char block-end-line)
                                          (forward-line)
                                          (point)))
                 (end (progn (skip-chars-forward " \r\t\n" limit)
                             (if (eobp) (point) (line-beginning-position)))))
            (list 'special-block
                  (nconc
                   (list :type type
                         :arguments arguments
                         :begin begin
                         :end end
                         :contents-begin contents-begin
                         :contents-end contents-end
                         :post-blank (count-lines pos-before-blank end)
                         :post-affiliated post-affiliated)
                   (cdr affiliated)))))))))

(defun org-element-special-block-interpreter (special-block contents)
  "Interpret SPECIAL-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (let ((block-type (org-element-property :type special-block))
        (block-arg (org-element-property :arguments special-block)))
    (format "#+begin_%s %s\n%s#+end_%s" block-type block-arg contents block-type)))


(provide 'init-org)
;;; init-org.el ends here
