;;; init-notetake.el --- My notetaking setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My current note taking system
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EPUB(with nov.el)
(use-package nov
  :straight '(nov :type git
                  :repo "https://depp.brause.cc/nov.el.git"
                  :fork "https://notabug.org/c1-g/nov.el.git")
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :mode (("\\.epub\\'" . nov-mode))
  :custom
  (nov-text-width fill-column)
  (nov-variable-pitch nil))

;; (use-package ereader
;;   :straight t
;;   :mode (("\\.epub\\'" . ereader-mode)))


;;;; PDF
(use-package pdf-tools
  :straight '(pdf-tools :type git :host github
                        :repo "vedang/pdf-tools"
                        :fork "orgtre/pdf-tools")
  :bind (:map pdf-view-mode-map
               ("j" . pdf-view-next-line-or-next-page)
               ("k" . pdf-view-previous-line-or-previous-page))
  :mode "\\.pdf\\'"
  :custom
  (pdf-keynav-copy-region-blink-delay 2)
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.1)
  (pdf-keynav-transient-mark-mode t)
  :config
  (pdf-loader-install)
  (defun pdf-view-midnight-colors-theme ()
    (cons (frame-parameter nil 'foreground-color)
          (color-darken-name
           (frame-parameter nil 'background-color) 5)))
  
  (add-hook 'pdf-view-midnight-mode-hook 'pdf-view-midnight-colors-theme)
  
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

(use-package toc-mode
  :straight t)

;;;; Djvu

(use-package djvu3
  :straight djvu '(djvu3 :type git :host github
                         :repo "dalanicolai/djvu3")
  :when (executable-find "djvused")
  :custom
  (djvu-continuous t))

;;;; Calibre
(use-package calibredb
  :straight '(calibredb.el :type git :host github
                           :repo "chenyanming/calibredb.el")
  :when (executable-find "calibredb")
  :config
  (setq calibredb-program (executable-find "calibredb"))
  (setq calibredb-root-dir *library-dir*)
  (setq calibredb-library-alist `((,*library-dir*)))
  (setq calibredb-db-dir (concat calibredb-root-dir "metadata.db"))
  (setq calibredb-ref-default-bibliography (concat calibredb-root-dir "muhbib.bib"))
  (setq calibredb-sort-by 'title)
  (setq calibredb-sql-newline "\n")
  (setq calibredb-sql-separator "|")
  (setq calibredb-detailed-view nil))
             
;;;; Note-taking with org
;;;;; Org-roam

;;;###autoload
(defun my/count-org-file-in-directory (directory)
  "A wrapper for `file-expand-wildcards' with \"*.org\" as its pattern.

Return 0 when `file-expand-wildcards' returns nil i.e. no files matched its pattern.
If `file-expand-wildcards' returns non-nil then return the length of the list of files
names that matches its pattern i.e. count them.

Used to determines filename in `org-roam-capture-templates'."
  (let ((org-files (file-expand-wildcards (concat directory "*.org"))))
    (if org-files
        (length org-files)
      0)))

;;;###autoload
(defun org-roam-slip-box-new-file ()
  "Return a new file path when creating a new note."
  (concat org-roam-directory (number-to-string (float (1+
                                                       (my/count-org-file-in-directory org-roam-directory))))
          ".org"))

(use-package org-roam
  :straight t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (defvar org-roam-directory (expand-file-name "slip-box/" org-directory))
  (defvar org-roam-v2-ack t)
  :config
  (org-roam-setup)
  (setq org-roam-index-file "index.org"
        org-roam-dailies-directory (expand-file-name "daily" org-directory)
        org-roam-db-update-method 'immediate
        org-roam-buffer-no-delete-other-windows t)
  (setq org-roam-capture-templates
        `(("d" "default" plain
           "%?"
           :if-new
           (file+head
            "%(org-roam-slip-box-new-file)"
            "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("n" "note" plain
           (file ,(expand-file-name "org-template/ROAM-note.txt" user-emacs-directory))
           :if-new
           (file+head
            "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
            "#+title: ${citekey}.  ${title}.\n#+created: %U\n#+last_modified: %U\n\n")
           :unarrowed t)))
  (setq org-roam-dailies-capture-templates
        `(("d" "default" plain
           "* %?"
           :if-new
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>\n#+created: %u\n\n")
           :unnarrowed t)))
  :diminish)

(use-package org-roam-bibtex
  :straight t
  :bind (:map org-roam-bibtex-mode-map
              (("C-c m f" . orb-find-non-ref-file))
              :map org-mode-map
              (("C-c m t" . orb-insert-non-ref)
               ("C-c m a" . orb-note-actions)))
 
  :custom
  (orb-autokey-format "%a%y")
  (orb-file-field-extensions '("pdf" "epub" "djvu"))
  :config
  (add-to-list 'orb-preformat-keywords "url")
  (org-roam-bibtex-mode)
  :diminish)

(use-package vulpea
  :straight t
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))


;;;;; Org-ref
(use-package org-ref
  :straight t
  :when *bibliography-dir*
  :custom
  (reftex-default-bibliography (directory-files *bibliography-dir* t directory-files-no-dot-files-regexp))
  (org-ref-notes-function #'orb-notes-fn)
  (org-ref-pdf-directory *library-dir*)
  (org-ref-default-bibliography reftex-default-bibliography)
  (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex))

;;;;; Media note
(use-package org-media-note
  :init (setq org-media-note-use-org-ref t)
  :disabled
  :straight '(org-media-note :type git
                             :host github
                             :repo "yuchen-lea/org-media-note")
  :hook (org-mode . org-media-note-mode)
  :bind ("s-v" . org-media-note-hydra/body)
  :config
  (use-package org-media-note-org-ref :ensure nil))

;;;;; Org-transclusion
(use-package org-transclusion
  :straight '(org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :after org
  :bind ("<f9>" . org-transclusion-add)
  :custom
  (org-transclusion-add-all-on-activate nil)
  (org-transclusion-exclude-elements '()))

(use-package interleave
  :straight t
  :disabled
  :custom
  (interleave--pdf-prop "INTERLEAVE_URL")
  (interleave--page-note-prop "INTERLEAVE_PAGE_NOTE"))

;;;;; Org-noter
(use-package org-noter
  :straight '(org-noter :type git
                        :host gitlab
                        :repo "c1-g/org-noter-plus-djvu"
                        :files ("other" "*.el"))
  :custom
  (org-noter-property-doc-file "INTERLEAVE_URL")
  (org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")
  (org-noter-doc-split-fraction '(0.57 0.43))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list (expand-file-name "lit" org-roam-directory)))
  :config
  (use-package org-noter-nov-overlay :load-path "straight/build/org-noter/other"))

(use-package org-noter-pdftools
  :after org-noter
  :straight t
  :hook (org-mode . org-pdftools-setup-link)
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


;;;; Bibtex completion
(use-package bibtex-completion
  :ensure nil
  :custom
  (bibtex-completion-edit-notes-function #'orb-bibtex-completion-edit-note)
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an"))
  (bibtex-completion-cite-default-command "cite")
  (bibtex-autokey-titleword-length 20)
  (bibtex-autokey-titlewords-stretch 0)
  (bibtex-autokey-titlewords 0)
  (bibtex-completion-bibliography (directory-files *bibliography-dir* t directory-files-no-dot-files-regexp))
  ;; (bibtex-completion-library-path org-ref-pdf-directory)
  (bibtex-completion-notes-path (expand-file-name "lit/" org-roam-directory))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-pdf-extension '(".pdf" ".djvu" ".epub"))
  (bibtex-completion-display-formats
   '((Book . "${author:36} ${title:*} ${year:4} ${formats:18} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
     (t . "${author:36} ${title:*} ${year:4} ${=has-note=:1} ${=type=:7}")))
  (bibtex-completion-pdf-symbol "P")
  (bibtex-completion-notes-symbol "N")
  (bibtex-completion-notes-template-multiple-files
   "#+TITLE: ${=key=}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\nLiterature notes for cite:${=key=}.\n\n")
  (bibtex-user-optional-fields '(("file" "Path to file")))
  (bibtex-completion-additional-search-fields '(file formats)))

;;;; Helm-bibtex
(use-package helm-bibtex
  :straight '(helm-bibtex :type git :host github
                          :repo "tmalsburg/helm-bibtex")
  :config
  (global-set-key (kbd "C-' b") #'helm-bibtex)
  
  (defun my/bibtex-completion-open-org-noter (keys)
    (with-temp-buffer
      (bibtex-completion-edit-notes keys)
      (org-noter)))

  (helm-bibtex-helmify-action my/bibtex-completion-open-org-noter helm-bibtex-open-org-noter)

  (helm-add-action-to-source "Open in org-noter" #'helm-bibtex-open-org-noter helm-source-bibtex))




;;;; Trying out SRS (space-repetition system)
(use-package org-anki
  :straight promise request '(org-anki
                              :type git
                              :host github
                              :repo "eyeinsky/org-anki")
  
  :when (executable-find "anki")
  :init
  :custom
  (org-anki-default-deck "one-big-deck"))

(use-package org-drill
  :termux
  :straight t)

(use-package org-fc
  :straight '(org-fc :type git
                     :host github
                     :repo "l3kn/org-fc"
                     :files ("awk" "*.org" "*.sh" "*.el")))

(use-package bir
  :straight '(bir.el :type git
                     :repo "https://notabug.org/c1-g/bir.el.git"
                     :files ("icons" "*.el")))


(use-package emacsql-sqlite :straight t)

;;;###autoload
(defun calibredb-query (sql-query)
    "Query calibre database and return the result.
Argument SQL-QUERY is the sqlite sql query string.

The function works by sending SQL-QUERY to `sql-sqlite-program' for the
database file defined by `calibredb-db-dir', dump the output to a hidden
buffer called *calibredb-query-output*, then if the sqlite program
terminates successfully, it will return the string of the output
buffer. If the program fails, it will switch to the output buffer and
tell user something’s wrong."
    (interactive)
    (let* ((inhibit-message t)
             (out-buf " *calibredb-query-output*")
             (tmp (make-temp-file "*calibredb-query-string*" nil nil sql-query)))
      (when (get-buffer out-buf)
        (kill-buffer out-buf))
      (if (not (file-exists-p calibredb-db-dir))
          (message "calibredb-query: calibredb-db-dir is nil! calibredb-query won't work without it.")
        (if (zerop (call-process-shell-command
                    (format "%s -list -nullvalue '' -noheader %s -init %s"
                            sql-sqlite-program
                            (shell-quote-argument (expand-file-name calibredb-db-dir))
                            tmp)
                    nil (list out-buf t)))
            ;; If this command terminates successfully (return 0)
            ;; Return the output's string
            (with-current-buffer out-buf
              (delete-file tmp)
              (buffer-string))
          ;; If this command fails return 'error
          (delete-file tmp)
          (switch-to-buffer out-buf)
          (error (format "calibredb-query: Can't query \"%s\". switching to its error buffer." (expand-file-name calibredb-db-dir)))))))

;;;; eww-bibtex
(use-package eww-bibtex
  :ensure nil)

(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
