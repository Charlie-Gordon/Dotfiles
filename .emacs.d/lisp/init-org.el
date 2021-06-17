;;; init-org.el --- Code for initializing org-mode -*- lexical-binding: t; -*-
;;;; Org
(use-package org
  :termux
  :straight t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 :map org-mode-map
	 ("C-'" . nil))
  :custom
  (org-export-coding-system 'utf-8)
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
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-edna-mode))

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-agenda
  :termux
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
  (org-set-agenda-files-recursively *journals-dir*)
  (setq org-agenda-custom-commands
        '(("R" "List of all headline with REVIEW keyword." search "REVIEW"
           ((org-show-context-detail 'minimal)
            (org-agenda-prefix-format ""))))))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   '(("Q"
      "Questions for this book.")
     ("Qb"
      "Questions bank on this book."
      entry
      (file buffer-file-name)
      (file "~/.emacs.d/org-template/QUIZ-todo.txt"))
     ("Qq"
      "A question for this book."
      entry
      (file+function buffer-file-name org-maybe-go-to-quiz)
      "** REVIEW Q:%?")
     ("Q4"
      "Essential four questions for reading, from Adler's How to Read A Book"
      entry
      (file+function buffer-file-name org-maybe-go-to-quiz)
      "** REVIEW Q:What is [[file:%F][%(file-name-sans-extension \"%f\")]] about as a whole?
** REVIEW Q:What [[file:%F][%(file-name-sans-extension \"%f\")]] said in detail, and how?
** REVIEW Q:Is [[file:%F][%(file-name-sans-extension \"%f\")]] true, in whole or part?
** REVIEW Q:What of [[file:%F][%(file-name-sans-extension \"%f\")]]?")))
  :init
  (defun org-maybe-go-to-quiz ()
    "Go to the first todo element with \"QUIZ\" keyword in current file, do nothing if not found."
    (goto-char
     (or (car (org-map-entries
	       (lambda nil (point-marker)) "todo=\"QUIZ\"" 'file))
	 (point-marker))))
  (defun my-org-capture-place-template-dont-delete-windows (oldfun args)
    "Prevent org-capture from modifying window configuration.

Taken this snippet from
legoscia's answer at
https://stackoverflow.com/questions/54192239/open-org-capture-buffer-in-specific-window/54251825#54251825"
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))
  :config
  (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))

(use-package org-anki
  :termux
  :straight '(org-anki :type git :host github :repo "eyeinsky/org-anki"
                       :fork t)
  :bind (:map org-mode-map
              ("C-c C-'" . org-anki-cloze-dwim))
  :custom
  (org-anki-default-deck "one-big-deck"))

(use-package org-edna
  :termux
  :straight t
  :config
  (defun org-edna-action/org-anki-this! (_last-entry)
    "Action to call `org-anki-sync-entry' on this entry
Edna Syntax: org-anki-this!"
    (org-anki-sync-entry)))

(use-package org-ref
  :straight t
  :custom
  (reftex-default-bibliography `(,(expand-file-name "org/bib/library.bib" *journals-dir*)))
  (org-ref-bibliography-notes (expand-file-name "org/ref-notes.org" *journals-dir*))
  (org-ref-pdf-directory (expand-file-name "library/" *journals-dir*))
  (bibtex-completion-bibliography reftex-default-bibliography)
  (bibtex-completion-library-path org-ref-pdf-directory))

;;;; Outshine
(use-package outshine
  :straight t
  :after outline
  :config
  ;; Required for outshine
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook 'outshine-mode))
;;;; Note-taking
;;;;; Org-roam
(use-package org-roam
  :straight t
  :bind (:map org-roam-mode-map
              (("C-c m l" . org-roam)
               ("C-c m F" . org-roam-find-file)
               ("C-c m r" . org-roam-find-ref)
               ("C-c m ." . org-roam-find-directory)
               ("C-c m d" . org-roam-dailies-map)
               ("C-c m j" . org-roam-jump-to-index)
               ("C-c m b" . org-roam-switch-to-buffer)
               ("C-c m g" . org-roam-graph))
              :map org-mode-map
              (("C-c m i" . org-roam-insert)))
  :custom
  (org-roam-db-update-method 'immediate)
  (org-roam-buffer-no-delete-other-windows t)
  :hook (after-init . org-roam-mode)
  :config
  ;; Org-roam uses sqlite3
  (add-to-list 'exec-path (executable-find "sqlite3"))
  (setq org-roam-directory (expand-file-name "org/slip-box/" *journals-dir*)
        org-roam-index-file "index.org"
        org-roam-dailies-directory (expand-file-name "daily/" org-roam-directory))
  (setq org-roam-dailies-capture-templates
        '("d" "default" entry
          #'org-roam-capture--get-point
          "* %?"
          :file-name "/storage/journals/org/daily/%<%Y-%m-%d>"
          :head "#+title: %<%Y-%m-%d>\n\n")))


(use-package org-roam-bibtex
  :straight t
  :requires bibtex-completion
  :bind (:map org-roam-bibtex-mode-map
              (("C-c m f" . orb-find-non-ref-file))
              :map org-mode-map
              (("C-c m t" . orb-insert-non-ref)
               ("C-c m a" . orb-note-actions)))
  :custom
  (orb-autokey-format "%a%y")
  (orb-templates
   `(("r" "ref" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "${citekey}"
      :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
      :unnarrowed t)
     ("n" "ref + noter" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      (concat "#+title: "
                              orb-title-format)
                      "#+roam_key: ${ref}"
                      ""
                      "* %(orb-process-file-field \"${citekey}\")"
                      ":PROPERTIES:"
                      ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                      ":NOTER_PAGE:"
                      ":END:")))))
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :diminish)

;;;;; Org-noter
(use-package org-noter
  :straight '(org-noter :type git :host github :repo "weirdNox/org-noter"
                        :fork t)
  :after org pdf-view
  :custom
  (org-noter-always-create-frame t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list *journals-dir*)))

;;;;; Org-transclusion
(use-package org-transclusion
  :straight '(org-transclusion :type git :host github :repo "nobiot/org-transclusion"
                               :fork t)
  :after org
  :bind ("<f9>" . org-transclusion-add)
  :custom
  (org-transclusion-add-all-on-activate nil)
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
;;;;; pdf-tools integration
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :straight t
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
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(provide 'init-org)
;;; init-org.el ends here
