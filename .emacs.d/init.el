;;;; General Improvement
;;; Interface tweaks
;;;; Remove lame startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message t)
;;;; UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;; Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;;; Autosave files
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;;; Lazy yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(save-place-mode 1)

(define-derived-mode external-mode fundamental-mode "External"
  (call-process "xdg-open" nil 0 nil (buffer-file-name)))

(add-to-list 'auto-mode-alist '("\\.\\(?:html\\|pdf\\|djvu\\|xps\\|cbz\\|fb2\\|pdf\\|txt\\|rft\\|chm\\|epub\\|doc\\|mobi\\)\\'" . external-mode))

;;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-host-usernames
      '((github . "c1-g")
        (gitlab . "c1-g")))

(straight-use-package 'use-package)

(use-package org
  :straight (:type built-in)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-'" . nil)
         ("C-M-a" . org-backward-paragraph)
         ("C-M-e" . org-forward-paragraph))
  :custom
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline t)
  (org-pretty-entities nil)
  (org-ellipsis "…")
  (org-id-link-to-org-use-id t)
  (org-ctrl-k-protect-subtree t)
  (org-directory "/sdcard/Download/")
  (org-clock-persist t)
  (org-special-ctrl-a/e t)
  (org-clock-auto-clock-resolution 'always)
  (org-attach-id-dir "/sdcard/Download/data/")
  (org-id-link-to-org-use-id t)
  (org-habit-graph-column 60)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands t)
  (org-refile-targets '(("/sdcard/Download/gtd/inbox.org" :maxlevel . 2)
                        ("/sdcard/Download/gtd/org-gtd-tasks.org" :maxlevel . 2)
                        ("/sdcard/Download/notecard/other/writing.org" :maxlevel . 2)))
  (org-image-actual-width nil)
  (org-todo-keywords
   '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CNCL(c)")))
  (org-format-latex-options
   '(:foreground auto
                 :background auto :scale 2
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (org-load-modules-maybe t)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (add-hook 'before-save-hook #'zp/org-set-last-modified nil t))))

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
  :after org
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
  (org-roam-db-location (expand-file-name "org-roam.db" "/sdcard/Download/"))
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

(use-package tablist
  :straight t)

(use-package f
  :straight t)

(use-package org-fc
  :straight '(org-fc :type git
                     :host github
                     :repo "l3kn/org-fc"
                     :fork t
                     :branch "org-roam"
                     :files ("awk" "*.org" "*.sh" "*.el" "tests" "icons"))
  :bind (:map org-fc-review-flip-mode-map
              ("r" . #'c1/org-fc-edit-on-saved-place)
              ("n" . #'org-fc-review-skip-card)
              :map org-fc-review-edit-mode-map
              ("n" . #'org-fc-review-skip-card))
  :init (use-package tablist-filter :ensure nil)
  :custom
  (org-fc-directories `(,org-roam-directory ,(expand-file-name "lit/" org-roam-directory)))
  (org-fc-algorithm 'roam-sm2)
  (org-fc-review-show-remaining-cards t)
  (org-fc-roam-postpone-skip-following-number-of-cards 100)
  (org-fc-topic-proportion 80)
  (org-fc-review-history-file "/sdcard/Download/review.tsv")
  :config
  (add-to-list 'org-tags-exclude-from-inheritance org-fc-suspended-tag)
  (add-to-list 'org-tags-exclude-from-inheritance org-fc-flashcard-tag)
  (advice-add 'org-fc-review-resume :before #'c1/org-fc-save-place)
  (add-to-list 'org-fc-custom-contexts (cons 'writing `(:paths (,org-roam-directory "/sdcard/Download/notecard/other/writing.org")
                                                               :indexer org-fc-awk-index
                                                               :filterer org-fc-index-filter-due
                                                               :non-recursive t)))
  (add-hook 'after-init-hook 'org-fc-review-all))

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(use-package writeroom-mode
  :straight t
  :hook
  (org-fc-after-setup . writeroom-mode))

(defun c1/org-fc-save-place ()
  (org-entry-put nil "FC_READ_POINT" (number-to-string (point)))
  (save-buffer)
  (save-place-to-alist))

(defun c1/org-fc-edit-on-saved-place ()
  (interactive)
  (org-fc-review-edit)
  (if (org-entry-get nil "FC_READ_POINT" t t)
      (goto-char (string-to-number (org-entry-get nil "FC_READ_POINT" t t)))
    (save-place-find-file-hook)))

(use-package org-fc-roam
  :ensure nil
  :after org-fc
  :diminish
  :config
  (org-fc-roam-db-autosync-enable)
  (org-fc-roam-mode 1))

(use-package bir
  :after org-roam
  :straight '(bir :type git
                  :host gitlab
                  :repo "c1-g/bir")
  :custom
  (bir-directory (expand-file-name "lit/" org-roam-directory))
  :config
  (advice-add 'bir-extract-region-prepare-finalize :after #'org-roam-db-update-file))
