;;; init-31-org.el --- Code for initializing org-mode

(use-package org
  :ensure t
  :init
  (defvar outline-minor-mode-prefix "\M-t")
  :hook (visual-line-mode . org-mode)) ;; text-wrap

(use-package outshine
  :ensure t
  :after org
  :config
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode))

(use-package org-noter
  :after org
  :ensure t
  :config
  (setq org-noter-default-ntoes-file-naems '("notes.org")
	org-noter-separate-notes-from-heading t)
  (use-package org-noter-pdftools
    :after org-noter))

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/journals/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
 ;; (push '("b" "Brain" plain (function org-brain-goto-end)
   ;       "* %i%?" :empty-lines 1)
    ;     org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

(use-package polymode
  :disabled
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package anki-editor
  :after org-noter
  :config
  (setq anki-editor-create-decks t
	anki-editor-org-tags-as-anki-tags t))

(provide 'init-31-org.el)
