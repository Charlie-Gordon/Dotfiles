;;; 31-init-org.el --- Code for initializing org-mode
;;;; Org
(use-package org
  :ensure t
  :bind ("C-c c" . org-capture)
  :custom
  (org-export-coding-system 'utf-8)
  (org-refile-target '((org-agenda-files . (:maxlevel . 6))))
  (org-todo-keywords
   '((sequence "TODO" "|" "DONE")
     (sequence "QUIZ" "|" "FOR_ZETTEL" "ZETTEL'ED")
     (sequence "REVIEW" "|" "ANSWERED" "ANKIFIED")))
  (org-todo-keyword-faces
   '(("QUIZ" . org-upcoming-deadline)
     ("FOR_ZETTEL" . org-agenda-current-time)
     ("ANSWERED" . org-scheduled-previously)))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-edna-mode))

(use-package org-agenda
  :ensure nil
  :config
;; Daniel Patru's answer at
;;  https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take
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
  (org-set-agenda-files-recursively "/storage/journals/"))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   '(("q"
      "Question bank for this book.")
     ("qb"
      "Questions bank on this book."
      entry
      (file buffer-file-name)
      (file "~/.emacs.d/org-template/QUIZ-todo.txt"))
     ("qq"
      "Question for this book."
      entry
      (file buffer-file-name)
      "** REVIEW Q:%?"))))

(use-package org-anki
  :ensure t
  :custom
  (org-anki-default-deck "one-big-deck"))

(use-package org-edna
  :ensure t
  :config
  (defun org-edna-action/org-anki-this! (_last-entry)
    "Action to call `org-anki-sync-entry' on this entry
Edna Syntax: org-anki-this!"
    (org-anki-sync-entry)))

;;;; Outshine
(use-package outshine
  :ensure t
  :after org
  :config
  ;; Required for outshine
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook 'outshine-mode))
;;;; Org-brain
(use-package org-brain
  :disabled
  :init
  (setq org-brain-path "~/journals/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  :custom
  (org-id-track-globally t)
  (org-id-locations-file "~/.emacs.d/.org-id-locations")
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 12)
  (org-brain-include-file-entries nil)
  (org-brain-file-entries-use-title nil)
  :ensure t)
;;;; Note-taking
;;;;; Org-roam
(use-package org-roam
  :ensure t
  :bind (:map org-roam-mode-map
	      ("C-c n l" . org-roam)
	      ("C-c n f" . org-roam-find-file)
	      ("C-c n g" . org-roam-graph)
	      ("C-c j d" . org-roam-dailies-find-today)
	      :map org-mode-map
	      ("C-c n i" . org-roam-insert)
	      ("C-c n I" . org-roam-insert-immediate))
  :custom
  (org-roam-directory (file-truename "/storage/journals/org/"))
  (org-roam-dailies-directory (file-truename "/storage/journals/org/daily/"))
  (org-roam-db-update-method 'immediate)
  (org-roam-dailies-capture-templates
	'(("d" "default" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "/storage/journals/org/daily/%<%Y-%m-%d>"
	 :head "#+title: %<%Y-%m-%d>\n\n")))
  :hook (after-init . org-roam-mode)
  :config
  ;; Org-roam use sqlite3
  (add-to-list 'exec-path (executable-find "sqlite3")))
;;;;; Org-noter
(use-package org-noter
  :ensure t
  :after org
  :custom
  (org-noter-default-notes-file-naems '("notes.org"))
  (org-noter-separate-notes-from-heading t)
  :bind (:map org-noter-doc-mode-map
	      ("M-h" . org-noter-insert-note-highlight))
  :hook (nov-post-html-render . org-noter-rehighlight-buffer)
  :config
  (use-package org-noter-highlight :ensure nil))
;;;;; Org-transclusion
(use-package org-transclusion
  :ensure nil
  :load-path "~/Git/org-transclusion"
  :after org
  :bind ("s-i" . org-transclusion-add-at-point)
  :custom
  (org-transclusion-add-all-on-activate nil)
;;  :hook (org-mode . org-transclusion-mode)
  :config
  (use-package text-clone :ensure nil))
;;;;; Org-marginalia
(use-package org-marginalia
  :disabled
  :ensure nil
  :load-path "~/Git/org-marginalia/"
  :after org
  :bind (:map org-marginalia-mode-map
	      ("C-c o" . org-marginalia-open)
	      ("C-c m" . org-marginalia-mark)
	      ("C-c n j" . org-marginalia-next)
	      ("C-c n k" . org-marginalia-prev)
	      ("C-c M" . org-marginalia-make-annotation)
	      ("C-c n J" . org-marginalia-browse-forward)
	      ("C-c n K" . org-marginalia-browse-backward))
  :hook ((nov-mode org-mode) . org-marginalia-mode))
;;;;; PDFs
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(use-package org-noter-pdftools
  :after (org-noter org-pdftools)
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
  :ensure t)

(provide '31-init-org.el)
