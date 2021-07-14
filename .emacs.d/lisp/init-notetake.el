;;; init-notetake.el --- My notetaking setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My current note taking system also, see more in the note-taking
;; section in lisp/init-org.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Note-taking with org
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
        org-roam-dailies-directory (expand-file-name "org/daily/" *journals-dir*))
  
  (setq org-roam-capture-templates
        '(("d" "default" plain
           #'org-roam-capture--get-point
           "%?"
           :file-name
           "/storage/journals/org/slip-box/%(number-to-string (float (1+ (my/count-org-file-in-directory org-roam-directory))))"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" plain
            #'org-roam-capture--get-point
            "* %?"
            :file-name "%<%Y-%m-%d>"
            :head "#+TITLE: %<%Y-%m-%d>\n\n"
            :unnarrowed t)))
  :diminish)

(defvar orb-title-format "${author-or-editor-abbrev}.  ${title}."
  "Format of the title to use for `orb-templates'.")

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
  (orb-file-field-extensions '("pdf" "epub"))
  (orb-templates
        `(("r" "ref" plain
           (function org-roam-capture--get-point)
           ""
           :file-name "refs/${citekey}"
           :head ,(string-join
                   (list
                    (concat "#+TITLE: " orb-title-format)
                    "#+ROAM_KEY: ${ref}"
                    "")
                   "\n")
           :unnarrowed t)
          ("n" "ref + noter" plain
           (function org-roam-capture--get-point)
           ""
           :file-name "refs/${citekey}"
           :head ,(string-join
                   (list
                    (concat "#+TITLE: " orb-title-format)
                    "#+ROAM_KEY: ${ref}"
                    ""
                    "* ${title}"
                    ":PROPERTIES:"
                    ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                    ":END:")
                   "\n"))))
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :diminish)

;;;;; Org-transclusion
(use-package org-transclusion
  :straight '(org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :after org
  :bind ("<f9>" . org-transclusion-add)
  :custom
  (org-transclusion-add-all-on-activate nil)
  (org-transclusion-exclude-elements '()))

(use-package interleave
  :straight t)

;;;;; Org-noter
(use-package org-noter
  :straight '(org-noter :type git :host github :repo "weirdNox/org-noter"
                        :fork t)
  :after org pdf-view
  :custom       
  (org-noter-doc-split-fraction '(0.57 0.43))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list *journals-dir*
                                     (expand-file-name "org/refs/" *journals-dir*))))


(use-package text-clone :ensure nil)

(use-package org-noter-synoptic
  :after text-clone org-roam-bibtex org-noter
  :config
  (add-hook 'org-noter-notes-mode-hook 'org-noter-synoptic--find-companion)
  :ensure nil)

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

;;;; Calibre
(use-package calibredb
  :straight t
  :config
  (setq calibredb-program (executable-find "calibredb"))
  (setq calibredb-root-dir *library-dir*)
  (setq calibredb-library-alist '(("/storage/library/")))
  (setq calibredb-ref-default-bibliography (concat calibredb-root-dir "muhbib.bib")))

;;;; Bibtex completion
(use-package bibtex-completion
  :straight t
  :bind
  ("C-' b" . helm-bibtex)
  :custom
  (bibtex-completion-bibliography reftex-default-bibliography)
  (bibtex-completion-library-path org-ref-pdf-directory)
  (bibtex-completion-notes-path (expand-file-name "refs/" org-roam-directory))
  (bibtex-completion-pdf-field "file"))
;;;; EPUB(with nov.el)
(use-package nov
  :straight t
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :mode (("\\.epub\\'" . nov-mode)))

;;;; PDF
(use-package pdf-tools
  :straight avy '(pdf-avy-highlight :type git :host github :repo "dalanicolai/dala-emacs-lisp"
				    :files ("pdf-avy-highlight.el"))
  :bind (:map pdf-view-mode-map
	      ("a k" . pdf-keyboard-highlight))
  :init
  (pdf-loader-install)
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.1)
  :config
  (defun prot/pdf-tools-backdrop ()
    (face-remap-add-relative
     'default
     `(:background ,(modus-themes-color 'bg-alt))))

  (defun prot/pdf-tools-midnight-mode-toggle ()
    (when (derived-mode-p 'pdf-view-mode)
      (if (eq (car custom-enabled-themes) 'modus-vivendi)
          (pdf-view-midnight-minor-mode 1)
        (pdf-view-midnight-minor-mode -1))
      (prot/pdf-tools-backdrop)))
  (add-hook 'pdf-tools-enabled-hook #'prot/pdf-tools-midnight-mode-toggle)
  (add-hook 'modus-themes-after-load-theme-hook #'prot/pdf-tools-midnight-mode-toggle))

(use-package pymupdf-mode
  :straight '(pymupdf-mode.el :type git :host github :repo "dalanicolai/pymupdf-mode.el"))

;;;; Trying out SRS (space-repetition system)
(use-package anki-editor
  :straight t)

(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
