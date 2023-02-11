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
                  :host nil
                  :repo "https://depp.brause.cc/nov.el.git"
                  :fork "https://notabug.org/c1-g/nov.el.git")
  :bind (:map nov-mode-map
	      ("C-S-n" . shr-next-link)
	      ("C-S-p" . shr-previous-link))
  :mode (("\\.epub\\'" . nov-mode))
  :custom
  (nov-text-width fill-column)
  (nov-variable-pitch t))

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
              ("k" . pdf-view-previous-line-or-previous-page)
              ("h" . image-backward-hscroll)
              ("l" . image-forward-hscroll))
  :mode "\\.pdf\\'"
  :custom
  (pdf-keynav-copy-region-blink-delay 2)
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-view-resize-factor 1.1)
  (pdf-keynav-transient-mark-mode t)
  :config
  (pdf-loader-install)
  (defun pdf-view-midnight-colors-theme ()
    (cons (frame-parameter nil 'foreground-color)
          (color-darken-name
           (frame-parameter nil 'background-color) 5)))
  (add-to-list 'pdf-tools-enabled-modes 'pdf-virtual-global-minor-mode)
  (add-to-list 'pdf-tools-enabled-modes 'pdf-keynav-minor-mode)
  (add-hook 'pdf-view-midnight-mode-hook 'pdf-view-midnight-colors-theme)
  
  (defun prot/pdf-tools-backdrop ()
    (face-remap-add-relative
     'default
     `(:background ,(modus-themes-get-color-value 'bg-dim))))
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

;;;; Note-taking with org
;;;;; Org-roam

;;;###autoload
(defun c1/count-org-file-in-directory (directory)
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
(defun org-roam-notecard-new-file ()
  "Return a new file path when creating a new note."
  (number-to-string (1+ (c1/count-org-file-in-directory org-roam-directory))))

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
  (defvar org-roam-directory (expand-file-name "notecard/" org-directory))
  :custom
  (org-roam-node-display-template "${title:100} ${tags:20}")
  (org-roam-dailies-directory (expand-file-name "daily" org-directory))
  (org-roam-extract-new-file-path "other/writing.org")
  (org-roam-db-location (expand-file-name "org-roam.db" "/storage/data/org/"))
  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :target
      (file+head
       "%<%F-%s>-%(org-roam-notecard-new-file).org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     ("w" "Writing inbox" plain
      "%?"
      :target
      (file "/storage/org/notecard/other/writing.org")
      :unnarrowed t)
     ("n" "note" plain
      "%?"
      :target
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${citekey}.  ${title}.\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)

     ("a" "article")

     ("au" "article from url" plain
      "%(bir-import \"%^{url}\")"
      :target
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     
     ("ak" "article from clipboard" entry
      "%(org-web-tools--url-as-readable-org)"
      :target
      (file "%<%F-%s>-%(org-roam-notecard-new-file).org")
      :unnarrowed t)

     ("af" "article from file" plain
      "%(bir-import-file \"%^{file}\")"
      :target
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   `(("d" "default" plain
      "* %?"
      :target
      (file+head "%<%Y-%m-%d>.org"
                 "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %u\n\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

(use-package citar-org-roam
  :straight t
  :after citar org-roam
  :no-require
  :custom
  (citar-org-roam-subdir "lit")
  :config (citar-org-roam-mode)
  :diminish)

(use-package org-roam-bibtex
  :straight t
  :after citar citar-org-roam
  :bind (:map org-roam-bibtex-mode-map
              (("C-c m f" . orb-find-non-ref-file))
              :map org-mode-map
              (("C-c m t" . orb-insert-non-ref)
               ("C-c m a" . orb-note-actions)))

  :custom
  (orb-autokey-format "%a%y")
  (orb-file-field-extensions '("pdf" "epub" "djvu" "htm" "html"))
  (orb-roam-ref-format 'org-cite)
  (citar-notes-source 'orb-citar-source)
  :config
  (add-to-list 'orb-preformat-keywords "url")
  (add-to-list 'orb-preformat-keywords "file")
  (org-roam-bibtex-mode)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))
  :diminish)


(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :diminish)

(use-package vulpea
  :disabled
  :straight t
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))



;;;;; Org-ref
(use-package org-ref
  :straight t
  :when *bibliography-dir*)

(use-package citar
  :straight t
  :bind (:map citar-map
              ("i" . citar-insert-citation)
              ("n" . citar-open-notes)
              ("o" . citar-open)
              ("e" . citar-open-entry)
              ("r" . citar-refresh)
              :map minibuffer-local-map
              ("M-b" . citar-insert-preset)
              :map org-mode-map :package org
              ("C-c B" . #'org-cite-insert))
  :bind-keymap ("C-c t" . citar-map)
  :init
  (define-prefix-command 'citar-map nil "Citar")
  :custom
  (citar-bibliography (append (directory-files *bibliography-dir* t ".bib")
                              (directory-files (expand-file-name "other/" org-roam-directory) t ".bib")))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-at-point-function 'embark-act)
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-library-paths `(,*library-dir*))
  (citar-notes-paths `(,org-roam-directory ,(expand-file-name "lit/" org-roam-directory)))
  (citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${formats:12}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.")
                     (note . "Notes on ${author editor}, ${title}"))))


;;;;; Media note
(use-package org-media-note
  :init (setq org-media-note-use-org-ref t)
  :straight '(org-media-note :type git
                             :host github
                             :repo "yuchen-lea/org-media-note")
  :hook (org-mode . org-media-note-mode)
  :bind* ("s-v" . org-media-note-hydra/body)
  :config
  (use-package org-media-note-org-ref :ensure nil))

;;;;; Org-transclusion
(use-package org-transclusion
  :straight '(org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :after org
  :bind ("<f9>" . c1/org-transclusion-toggle)
  :custom
  (org-transclusion-add-all-on-activate nil)
  (org-transclusion-exclude-elements '(drawer))
  :config
  
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode))

(defun c1/org-transclusion-exclude-element-id (data)
  (org-element-map data '(node-property)
    (lambda (d)
      (when (string= "ID" (org-element-property :key d))
        (org-element-extract-element d))))
  data)

(advice-add 'org-transclusion-content-filter-org-exclude-elements
            :filter-return #'c1/org-transclusion-exclude-element-id)

(defun c1/org-transclusion-toggle ()
  (interactive)
  (save-excursion
    (if (org-transclusion-within-transclusion-p)
        (org-transclusion-deactivate)
      (org-transclusion-add))))

(use-package interleave
  :straight t
  :disabled
  :custom
  (interleave--pdf-prop "INTERLEAVE_URL")
  (interleave--page-note-prop "INTERLEAVE_PAGE_NOTE"))

;;;;; Org-noter
(use-package org-noter
  :straight '(org-noter :type git
                        :host nil
                        :repo "git@github.com:c1-g/org-noter-plus-djvu.git"
                        :branch "link-as-doc"
                        :files ("other/*.el" "*.el" "modules/*.el"))
  :bind (:map org-noter-doc-mode-map
              ("I" . org-noter-insert-dynamic-block)
              ("M-I" . org-noter-insert-precise-dynamic-block))
  :custom
  (org-noter-property-doc-file "SOURCE")
  (org-noter-property-note-location "DOCUMENT_PAGE")
  (org-noter-doc-split-fraction '(0.65 0.4))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame t)
  (org-noter-prefer-root-as-file-level t)
  (org-noter-disable-narrowing t)
  (org-noter-keep-notes-after-kill-session t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-hide-other t)
  (org-noter-notes-search-path (list (expand-file-name "lit" org-roam-directory)))
  :config
  (advice-add 'org-noter-pdf--get-selected-text :before #'c1/pdf-keynav-region-to-active-region)
  (use-package org-noter-nov :ensure nil)
  (use-package org-noter-djvu :ensure nil)
  (use-package org-noter-pdf :ensure nil)
  (use-package org-noter-nov-overlay :ensure nil)
  (use-package org-noter-dynamic-block :ensure nil)
  (use-package org-noter-citar :ensure nil))

(defun c1/pdf-keynav-region-to-active-region (mode)
  (when (eq mode 'pdf-view-mode)
    (require 'pdf-keynav)
    (pdf-keynav-region-to-active-region)))

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after (:and org-noter org-pdftools)
  :straight t
  :custom
  (org-noter-pdftools-markup-pointer-color "#00bfff")
  (org-noter-pdftools-use-unique-org-id nil)
  (org-noter-pdftools-org-id-prefix #'c1/org-noter-pdftools-org-id-prefix)
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
  (advice-add 'org-noter-insert-precise-note :override #'org-noter-pdftools-insert-precise-note))

(defun c1/org-noter-pdftools-org-id-prefix ()
  (org-noter--with-valid-session
   (my-generate-sanitized-alnum-dash-string
    (file-name-base (org-noter--session-property-text session)))))

(use-package org-noter-media
  :straight '(org-noter-media :type git
                              :host github
                              :repo "auroranil/org-noter-media"
                              :fork t)
  :custom
  (mpv-default-options '("--gapless-audio=no" "--ytdl-format=bestvideo[height<=?480]+bestaudio/best")))

(use-package bookmark+
  :straight t
  :config
  (use-package bookmark+-lit :ensure nil))


;;;;; Org-download
(use-package org-download
  :straight t
  :bind (:map org-mode-map
              ("<f10>" . org-download-screenshot))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s"))

;;;; Bibtex completion
(use-package bibtex
  :straight t
  :custom
  (bibtex-files `(,*bibliography-dir*)))


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
  (bibtex-completion-bibliography (append (directory-files *bibliography-dir* t ".bib")
                                          (directory-files (expand-file-name "other/" org-roam-directory) t ".bib")))
  (bibtex-completion-notes-path (expand-file-name "lit/" org-roam-directory))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-pdf-extension '(".pdf" ".djvu" ".epub"))
  (bibtex-completion-display-formats
   '((Misc . "${author:36} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
     (t . "${author:36} ${title:*} ${year:4} ${formats:18} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  (bibtex-completion-pdf-symbol "P")
  (bibtex-completion-notes-symbol "N")
  (bibtex-completion-notes-template-multiple-files
   "#+TITLE: ${=key=}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\nLiterature notes for cite:${=key=}.\n\n")
  (bibtex-user-optional-fields '(("file" "Path to file")))
  (bibtex-completion-additional-search-fields '(file formats)))

;;;; Trying out SRS (space-repetition system)
(use-package org-anki
  :straight promise request '(org-anki
                              :type git
                              :host github
                              :repo "eyeinsky/org-anki")
  :disabled
  :when (executable-find "anki")
  :init
  :custom
  (org-anki-default-deck "one-big-deck"))

(use-package org-drill
  :termux
  :straight t)

(use-package org-fc
  :straight '(org-fc :type git
                     :host nil
                     :repo "https://github.com/l3kn/org-fc.git"
                     :fork "git@github.com:c1-g/org-fc.git"
                     :branch "org-roam"
                     :files ("awk" "*.org" "*.sh" "*.el" "tests" "icons"))
  :bind (:map org-fc-review-flip-mode-map
              ("C-;" . #'c1/open-org-noter)
              ("C-," . #'c1/read-aloud-org)
              ("n" . #'org-fc-review-skip-card)
              ("r" . #'c1/org-fc-edit-on-saved-place)
              :map org-fc-review-edit-mode-map
              ("n" . #'org-fc-review-skip-card)
              ("C-;" . #'c1/open-org-noter)
              ("C-," . #'c1/read-aloud-org))
  :init (use-package tablist-filter :ensure nil)
  :custom
  (org-fc-directories `(,org-roam-directory ,(expand-file-name "lit/" org-roam-directory)))
  ;; (org-fc-browser-list-entries-function #'org-fc-browser-list-db)
  (org-fc-algorithm 'roam-sm2)
  (org-fc-review-show-remaining-cards t)
  (org-fc-roam-postpone-skip-following-number-of-cards 100)
  (org-fc-topic-proportion 80)
  (org-fc-review-history-file "/storage/data/org/review.tsv")
  (org-fc-browser-headers
   '(("No." org-fc-browser-num>?)
     ("Title" nil)
     ("Priority" org-fc-browser-priority>?)
     ("Intrv" org-fc-browser-intrv>?)
     ("Due" t :read-only)
     ("Type" nil)))
  :config
  (add-to-list 'org-tags-exclude-from-inheritance org-fc-suspended-tag)
  (add-to-list 'org-tags-exclude-from-inheritance org-fc-flashcard-tag)
  (advice-add 'org-fc-review-resume :before #'c1/org-fc-save-place)
  (add-to-list 'org-fc-custom-contexts (cons 'writing `(:paths (,org-roam-directory "/storage/org/notecard/other/writing.org")
                                                               :indexer org-fc-awk-index
                                                               :filterer org-fc-index-filter-due
                                                               :non-recursive t)))
  (advice-add 'org-fc-review-next-card :before #'c1/maybe-close-org-noter)
  (advice-add 'org-fc-review-next-card :after #'org-fc-write-setup)
  (add-hook 'after-init-hook #'org-fc-review-daily 80))

(use-package writeroom-mode
  :straight t
  :hook
  (org-fc-after-setup . writeroom-mode))

(defun c1/org-fc-edit-on-saved-place ()
  (interactive)
  (org-fc-review-edit)
  (if (org-entry-get nil "FC_READ_POINT" t t)
      (goto-char (string-to-number (org-entry-get nil "FC_READ_POINT" t t)))
    (save-place-find-file-hook)))

(defun c1/org-fc-save-place ()
  (org-entry-put nil "FC_READ_POINT" (number-to-string (point)))
  (save-buffer)
  (save-place-to-alist))


(defun org-fc-write-setup (&optional resuming)
  (when (and (not resuming) (member "writing" (org-get-tags)))
    (save-excursion
      (c1/org-fc-edit-on-saved-place))
    (unless (org-before-first-heading-p)
      (org-fc-narrow)
      (org-fc-hide-drawers))
    (org-show-all)))

(defun org-fc-review-daily ()
  (interactive)
  (if org-fc-review--session
      (when (yes-or-no-p "Flashcards are already being reviewed. Resume? ")
        (org-fc-review-resume))
    (let* ((index (org-fc-index org-fc-context-all))
           (cards (funcall org-fc-index-filter-function index)))
      (setq cards (delete-dups
                   (append (org-fc-index-shuffled-positions
                            (org-fc-index-filter-due
                             (org-fc-awk-index
                              (list org-roam-directory "/storage/org/notecard/other/") nil t)))
                           (funcall org-fc-index-sort-function cards))))
      (if (null cards)
          (message "No cards due right now")
        (setq org-fc-review--session (org-fc-make-review-session cards))
        (run-hooks 'org-fc-before-review-hook)
        (org-fc-review-next-card)))))

(use-package org-fc-roam
  :ensure nil
  :after org-fc
  :diminish
  :config
  (org-fc-roam-db-autosync-enable)
  (org-fc-roam-mode +1))

(defun c1/maybe-close-org-noter (&rest _args)
  (interactive)
  (org-noter--with-valid-session
   (let ((org-noter-use-indirect-buffer nil))
     (org-noter-kill-session session))))

(defun c1/open-org-noter ()
  (interactive)
  (let ((org-noter-disable-narrowing t)
        (org-noter-use-indirect-buffer nil))
    (org-noter 0)))

(use-package read-aloud
  :straight t
  :custom
  (read-aloud-engine "espeak")
  :config
  (lax-plist-put read-aloud-engines
                 "festival"
                 '(cmd "festival" args ("--tts" "--pipe")))
  (lax-plist-put read-aloud-engines
                 "espeak"
                 '(cmd "espeak" args ("-s 350" "--stdin"))))

(org-export-define-derived-backend 'ascii-simple 'ascii
  :translate-alist
  '((link . c1/ascii-simple-link)
    (bold . c1/ascii-simple-bold)))

(defun c1/ascii-simple-link (link desc info)
  "Transcode a LINK object from Org to ASCII.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (if (org-string-nw-p desc)
      desc
    "link"))

(defun c1/ascii-simple-bold (_bold contents _info)
  contents)

(defun c1/read-aloud-org ()
  (interactive)
  (let ((org-export-with-toc nil)
        (org-export-with-author nil)
        (org-export-with-emphasize nil)
        (org-export-with-tables nil)
        (org-ascii-links-to-notes nil)
        (buf))
    (if (org-before-first-heading-p)
        (setq buf (org-export-to-buffer 'ascii-simple "*Org Ascii export*"
                    nil nil t t nil (lambda () (text-mode))))
      (setq buf (org-export-to-buffer 'ascii-simple "*Org Ascii export*"
                  nil t t t nil (lambda () (text-mode)))))
    (with-current-buffer buf
      (read-aloud-buf))))

(use-package bir
  :straight '(bir :type git
                  :host gitlab
                  :repo "c1-g/bir")
  :custom
  (bir-directory (expand-file-name "lit/" org-roam-directory))
  :config
  (advice-add 'bir-extract-region-prepare-finalize :after #'org-roam-db-update-file))


(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
