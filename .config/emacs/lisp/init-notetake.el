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
  (nov-variable-pitch nil)
  :config
  (add-hook 'nov-post-html-render-hook 'c1/org-fc-hard-to-read-font))

(defun c1/org-fc-hard-to-read-font ()
  "Set font to a hard to read font in org-fc to enhance concentration."
  (interactive)
  (setq buffer-face-mode-face '(:family "Sans Forgetica"))
  (buffer-face-mode))



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

(defun calibredb-query (sql-query)
  "Query calibre databse and return the result.
Argument SQL-QUERY is the sqlite sql query string."
  (interactive)
  (if (file-exists-p calibredb-db-dir)
    (shell-command-to-string
       (format "%s -list -noheader %s -init %s"
               sql-sqlite-program
               (shell-quote-argument (expand-file-name calibredb-db-dir))
               (make-temp-file "calibredb-query-string" nil nil sql-query))) nil))
             
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
(defun org-roam-slip-box-new-file ()
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
  (defvar org-roam-directory (expand-file-name "slip-box/" org-directory))
  :custom
  (org-roam-node-display-template "${refs}. ${my-title:100} ${tags:20}")
  (org-roam-dailies-directory (expand-file-name "daily" org-directory))
  (org-roam-extract-new-file-path "%<%F-%s>-%(org-roam-slip-box-new-file).org")
  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :if-new
      (file+head
       "%<%F-%s>-%(org-roam-slip-box-new-file).org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     ("r" "References" plain
      "${tree}"
      :if-new
      (file+olp ,(expand-file-name "lit/refs/bibliography.org" org-roam-directory)
                ("Unsorted"))
      :unnarrowed t)
     ("n" "note" plain
      "%?"
      :if-new
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${citekey}.  ${title}.\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)

     ("a" "article")

     ("au" "article from url" plain
      "%(bir-import \"%^{url}\")"
      :if-new
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     
     ("ak" "article from clipboard" entry
      "%(org-web-tools--url-as-readable-org)"
      :if-new
      (file "%<%F-%s>-%(org-roam-slip-box-new-file).org")
      :unnarrowed t)

     ("af" "article from file" plain
      "%(bir-import-file \"%^{file}\")"
      :if-new
      (file+head
       "%(expand-file-name \"lit\" org-roam-directory)/${citekey}.org"
       "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   `(("d" "default" plain
      "* %?"
      :if-new
      (file+head "%<%Y-%m-%d>.org"
                 "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %u\n\n")
      :unnarrowed t)
     ("t" "tree" plain
      "${tree}"
      :if-new
      (file+head "%<%Y-%m-%d>.org"
                 "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %u\n\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode)
  ;; (cl-defmethod org-roam-node-article ((node org-roam-node))
  ;; (let* ((props (org-roam-node-properties node))
  ;;        (article (cdr (assoc bir-ref-article-property props #'string=)))
  ;;        desc)
  ;;   (unless article
  ;;     (setq article
  ;;           (with-temp-buffer
  ;;             (insert-file-contents (org-roam-node-file node))
  ;;             (plist-get (org-element--get-global-node-properties)
  ;;                        (intern (concat ":" (upcase bir-ref-article-property)))))))
  ;;   (when article
  ;;     (string-match org-link-bracket-re article)
  ;;     (setq desc (match-string 2 article)))
  ;;   (if desc
  ;;       (concat desc ". ")
  ;;     "")))
  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (if (string-match-p "^[[:digit:]]+" (org-roam-node-title node))
        (with-temp-buffer
          (insert-file-contents (org-roam-node-file node))
          (org-roam-end-of-meta-data 'full)
          (replace-regexp-in-region (concat "\\(?:"
                                            "^%+" ; line beg with %
                                            "\\|" org-property-re
                                            "\\|" "\n"
                                            "\\|:\\S-+:"
                                            "\\|" org-table-line-regexp
                                            "\\|" org-heading-regexp
                                            "\\|" ":[[:alnum:]_@#%]+:"
                                            "\\|" org-keyword-regexp
                                            "\\|" "#\\+.*"
                                            "\\|" org-element--timestamp-regexp
                                            "\\|-\\*-[[:alpha:]]+-\\*-" ; -*- .. -*- lines
                                            "$\\)")
                                    "")
          (buffer-substring-no-properties
           (point)
           (or (re-search-forward (sentence-end) nil t) (point-max))))
      (org-roam-node-title node))))

(use-package org-roam-bibtex
  :straight t
  :bind (:map org-roam-bibtex-mode-map
              (("C-c m f" . orb-find-non-ref-file))
              :map org-mode-map
              (("C-c m t" . orb-insert-non-ref)
               ("C-c m a" . orb-note-actions)))
  
  :custom
  (orb-autokey-format "%a%y")
  (orb-file-field-extensions '("pdf" "epub" "djvu" "htm" "html"))
  :config
  (add-to-list 'orb-preformat-keywords "url")
  (add-to-list 'orb-preformat-keywords "file")
  (org-roam-bibtex-mode)
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
                              (directory-files (expand-file-name "lit/refs/" org-roam-directory) t ".bib")))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-at-point-function 'embark-act)
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-library-paths `(,*library-dir*))
  (citar-notes-paths `(,*org-dir*))
  (citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${formats:12}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.")
                     (note . "Notes on ${author editor}, ${title}")))
  :config
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))


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
  (org-transclusion-exclude-elements '(drawer)))

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
                        :host github
                        :repo "c1-g/org-noter-plus-djvu"
                        :branch "link-as-doc"
                        :files ("other/*.el" "*.el" "modules/*.el"))
  :bind (:map org-noter-doc-mode-map
              ("I" . org-noter-insert-dynamic-block)
              ("M-I" . org-noter-insert-precise-dynamic-block))
  :custom
  (org-noter-property-doc-file "ROAM_REFS")
  (org-noter-property-note-location "DOCUMENT_PAGE")
  (org-noter-doc-split-fraction '(0.6 0.4))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame t)
  (org-noter-disable-narrowing t)
  (org-noter-keep-notes-after-kill-session t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list (expand-file-name "lit" org-roam-directory)))
  :config
  (add-to-list 'org-speed-commands '("." . org-noter-sync-current-note))
  (use-package org-noter-nov-overlay :ensure nil)
  (use-package org-noter-dynamic-block :ensure nil)
  (use-package org-noter-citar :ensure nil))

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
    :after (:and org-noter org-pdftools)
    :straight t
    :config
    ;; Add a function to ensure precise note is inserted
    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                     (not org-noter-insert-note-no-questions)
                                                   org-noter-insert-note-no-questions))
             (org-pdftools-use-isearch-link t)
             (org-pdftools-use-freepointer-annot t))
         (org-noter-insert-note (org-noter--get-precise-info)))))

    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-noter-media
  :straight '(org-noter-media :type git
                              :host github
                              :repo "auroranil/org-noter-media"))




;;;;; Org-download
(use-package org-download
  :straight t
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
  (bibtex-completion-bibliography (directory-files *bibliography-dir* t ".bib"))
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
                     :host github
                     :repo "l3kn/org-fc"
                     :fork t
                     :files ("awk" "*.org" "*.sh" "*.el" "tests"  "icons"))
  :init (use-package tablist-filter :ensure nil)
  :custom
  (org-fc-directories `(,org-roam-directory ,(expand-file-name "lit/" org-roam-directory)))
  ;; (org-fc-browser-list-entries-function #'org-fc-browser-list-db)
  (org-fc-index-function #'org-fc-roam-index)
  (org-fc-index-filter-function #'identity)
  (org-fc-algorithm 'roam-sm2)
  (org-fc-custom-contexts '((writing :paths "/storage/org/gtd/writing.org")))
  (org-fc-topic-proportion 80)
  (org-fc-browser-headers
   '(("No." org-fc-browser-num>?)
     ("Title" nil)
     ("Priority" org-fc-browser-priority>?)
     ("Intrv" org-fc-browser-intrv>?)
     ("Due" t :read-only)
     ("Type" nil)))
  :config
  (use-package org-fc-roam
    :ensure nil
    :config
    (org-fc-roam-db-autosync-enable))
  (add-hook 'org-fc-before-setup-hook #'c1/org-fc-hard-to-read-font)
  ;; (org-fc-cache-mode)
  (add-hook 'org-fc-after-setup-hook #'c1/maybe-open-org-noter))

(defun c1/maybe-close-org-noter (&rest _args)
  (org-noter--with-valid-session
   (let ((org-noter-use-indirect-buffer nil))
     (org-noter-kill-session session))))

(advice-add 'org-fc-review-next-card :before #'c1/maybe-close-org-noter)

(defun c1/maybe-open-org-noter ()
  (interactive)
  (require 'org-noter)
  (when (or (org-property-values org-noter-property-note-location)
            (org-property-values org-noter-property-doc-file))
    (let ((org-noter-disable-narrowing t)
          (org-noter-use-indirect-buffer nil))
      (org-noter 0))))

(defun org-fc-algo-sm2-cloze-review-interval (position)
  (when (and (org-fc-entry-cloze-p) (org-entry-get nil bir-ref-parent-property))
    (let ((interval (+ 12 (cl-random 30.0))))
      (list position (org-fc-algo-sm2-ease-initial) 0
            interval
            (org-fc-timestamp-in interval)))))

(defun org-fc-browser-index-pending ()
  (org-roam-db-query "SELECT * FROM
(SELECT id,
 '[',
'\"' || rowid || '\"',
title,
'\"' || prior || '\"',
'\"' || ivl || '\"',
'\"' || strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ', due, 'unixepoch') || '\"',
'\"' || type || '\"', pos,
'(' || group_concat(tags, ' ') || ')' as tags,
']'
FROM
(SELECT
cards.rowid as rowid,
cards.node_id as id,
cards.title as title,
cards.pos as pos,
cards.prior as prior,
cards.ease as ease,
cards.box as box,
cards.ivl as ivl,
cards.due as due,
cards.postp as postp,
cards.type as type,
cards.queue as queue,
tags.tag as tags
FROM cards
LEFT JOIN tags ON tags.node_id = cards.node_id
GROUP BY id, cards.pos, tags)
WHERE queue = 0
GROUP BY id, pos)"))

(defun org-fc-browser-list-db ()
  "docstring"
  (let ((outstanding (plist-get org-fc-browser-context :outstanding))
        (pending (plist-get org-fc-browser-context :pending))
        cards)
    (cond (outstanding
           (if (and org-fc-review--session (oref org-fc-review--session cards))
               (setq cards (oref org-fc-review--session cards))
             (setq cards (org-fc-index org-fc-browser-context))
             (setq cards (funcall org-fc-index-sort-function cards))
             (setq org-fc-review--session
                (org-fc-make-review-session cards org-fc-browser-context))
             (setf (oref org-fc-review--session cards) cards)))
          (pending
           (org-fc-browser-index-pending))
          (t
           (setq cards (org-fc-index org-fc-browser-context))
           (setq cards (org-fc-index-positions cards))
           (if cards
               (cl-loop for card in cards
                        append `((,(plist-get card :id)
                                  [,(number-to-string (plist-get card :num))
                                   ,(if (string-empty-p (plist-get card :title))
                                        (plist-get card :filetitle)
                                      (plist-get card :title))
                                   ,(if (plist-get card :prior)
                                        (number-to-string (plist-get card :prior))
                                      "-")
                                   ,(format "%.2f" (plist-get card :interval))
                                   ,(if (stringp (plist-get card :due))
                                        (plist-get card :due)
                                      (format-time-string "%FT%TZ" (plist-get card :due) "UTC0"))
                                   ,(symbol-name (plist-get card :type))
                                   ,(plist-get card :position)
                                   ,(plist-get card :tags)])))
             (message "No cards due right now")
             nil)))))

(use-package bir
  :straight '(bir :type git
                  :host gitlab
                  :repo "c1-g/bir")
  :custom
  (bir-directory (expand-file-name "lit/" org-roam-directory)))


(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
