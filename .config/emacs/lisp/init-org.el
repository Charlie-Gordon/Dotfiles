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
  (org-clock-persist t)
  (org-clock-auto-clock-resolution 'always)
  (org-clock-sound "/storage/music/metronome2.wav")
  (org-attach-id-dir "/storage/org/data/")
  (org-id-link-to-org-use-id t)
  (org-habit-graph-column 60)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands t)
  (org-refile-targets '(("/storage/org/gtd/inbox.org" :maxlevel . 2)
                        ("/storage/org/gtd/org-gtd-tasks.org" :maxlevel . 2)
                        ("/storage/org/slip-box/lit/other/writing.org" :maxlevel . 2)))
  (org-image-actual-width nil)
  (org-todo-keywords
   '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CNCL(c)")))
  (org-format-latex-options
   '(:foreground auto
                 :background auto :scale 1.6
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'habits)
  (add-to-list 'org-speed-commands '("w" . widen))
  (add-to-list 'org-speed-commands '("N" . org-narrow-to-element))
  (add-hook 'org-mode-hook #'(lambda nil
                               (add-hook 'before-save-hook #'zp/org-set-last-modified nil t)))
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  (org-load-modules-maybe t)
  (org-resolve-clocks)
  :hook
  (org-mode . visual-line-mode)
  (org-clock-in . c1/org-set-todo-progress)
  (org-clock-in . org-clock-save)
  (org-clock-out . c1/org-set-todo-waiting))

(use-package org-capture
  :ensure nil
  :config
  (add-to-list 'org-capture-templates
               '("w" "Writing inbox" plain
                 (file "/storage/org/slip-box/lit/other/writing.org")
                 "* %?"
                 :unnarrowed t)))

(use-package org-clock-convenience
  :straight t
  :bind (:map org-agenda-mode-map
              ("<s-up>" . org-clock-convenience-timestamp-up)
              ("<s-down>" . org-clock-convenience-timestamp-down)
              ("+" . org-clock-convenience-fill-gap)
              ("=" . org-clock-convenience-fill-gap-both)))

(defun c1/org-set-todo-waiting ()
  (unless (member (org-entry-get nil "TODO") org-done-keywords)
    (org-entry-put nil "TODO" "WAIT")))

(defun c1/org-set-todo-progress ()
  (org-entry-put nil "TODO" "PROG"))

;; https://www.reddit.com/r/emacs/comments/be1n1y/orgmode_set_effort_of_headline_from_sum_of/

;;; Recompute effort of a parent headline from the efforts of the children if
;;; they sum to a higher value.
(defun my-org-update-heading-effort-from-children (pt)
  "Compute the sum of efforts for each child of the heading at point PT. If
the sum is greater than the current effort for this heading, offer to update
it. This function is called recursively on each child, so the entire tree's
efforts may be updated by this function."
  (save-excursion
    (save-restriction
      (goto-char pt)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (outline-show-all)
      (let*
          ((current-effort
            (org-duration-to-minutes
             (or (org-entry-get (point) org-effort-property) 0)))
           (children-effort 0))
        (while (outline-next-heading)
          (let ((x (my-org-update-heading-effort-from-children (point))))
            (setq children-effort (+ children-effort (nth 0 x)))
            (goto-char (nth 1 x))))
        (goto-char pt)
        (let ((children-effort-duration
               (org-duration-from-minutes children-effort)))
          (when (and (< current-effort children-effort)
                     (y-or-n-p-with-timeout
                      (format "Update effort to children's sum (%s)?"
                              children-effort-duration)
                      60 nil))
            (org-entry-put (point) org-effort-property
                           children-effort-duration)
            (setq current-effort children-effort)))
        (list current-effort (point-max))))))

(defun my-org-effort-from-children-hook ()
  "Update effort of a heading from its children before clocking in."
  (my-org-update-heading-effort-from-children (point))
  nil)

(add-hook 'org-clock-in-prepare-hook 'my-org-effort-from-children-hook)

(use-package org-super-links
  :straight '(org-super-links :type git
                              :host github
                              :repo "toshism/org-super-links")
  :custom
  (org-super-links-related-into-drawer t))

(defun my-generate-sanitized-alnum-dash-string (str)
  "Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
  (require 'ffap)
  (let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
         (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ", "." with " dot ", "+" with
         ;; " plus ".
         (str (replace-regexp-in-string
               "&" " and "
               (replace-regexp-in-string
                "\\." " dot "
                (replace-regexp-in-string
                 "\\+" " plus " str))))
         ;; Replace German Umlauts with 7-bit ASCII.
         (str (replace-regexp-in-string "ä" "ae" str nil))
         (str (replace-regexp-in-string "ü" "ue" str nil))
         (str (replace-regexp-in-string "ö" "oe" str nil))
         (str (replace-regexp-in-string "ß" "ss" str nil))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; On emacs 24.5, multibyte punctuation characters like "："
         ;; are considered as alphanumeric characters! Below evals to
         ;; non-nil on emacs 24.5:
         ;;   (string-match-p "[[:alnum:]]+" "：")
         ;; So replace them with space manually..
         (str (if (version< emacs-version "25.0")
                  (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                    (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
                str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

(defun my-id-get-or-generate (&optional force)
  "Returns the ID property if set or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
  (interactive)
  (when force
    (org-entry-put (point) "ID" nil))
  (when (not (org-id-get))
    (progn
      (let* ((my-heading-text (or (if (= (org-outline-level) 0)
                                      (cadar (org-collect-keywords '("TITLE")))
                                    (nth 4 (org-heading-components)))
                                  (c1/org-get-first-sentence))) ;; retrieve heading string
             (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" my-heading-text)) ;; remove progress indicators like "[25%]"
             (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" my-heading-text)) ;; remove progress indicators like "[2/7]"
             (my-heading-text (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" my-heading-text)) ;; remove priority indicators like "[#A]"
             (my-heading-text (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" my-heading-text t)) ;; removes links, keeps their description and ending brackets
             ;;                      (my-heading-text (replace-regexp-in-string "[<\\[][12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)[>\\]]" "" my-heading-text t));; removes day of week and time from date- and time-stamps (doesn't work somehow)
             (my-heading-text (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" my-heading-text t)) ;; removes day of week and time from active date- and time-stamps
             (my-heading-text (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" my-heading-text t)) ;; removes day of week and time from inactive date- and time-stamps
             (new-id (my-generate-sanitized-alnum-dash-string my-heading-text)) ;; get slug from heading text
             (my-created-property (assoc "CREATED" (org-entry-properties))) ;; nil or content of CREATED time-stamp
             )
        (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
          ;; only if no ISO date-stamp is found at the beginning of the new id:
          (if my-created-property (progn
                                    ;; prefer date-stamp of CREATED property (if found):
                                    (setq my-created-datestamp (substring (org-entry-get nil "CREATED" nil) 1 11)) ;; returns "2021-12-16" or nil (if no CREATED property)
                                    (setq new-id (concat my-created-datestamp "-" new-id)))
            ;; use today's date-stamp if no CREATED property is found:
            (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id))))
        (org-set-property "ID" new-id))))
  (kill-new (concat "id:" (org-id-get))) ;; put ID in kill-ring
  (org-id-get) ;; retrieve the current ID in any case as return value
  )

(defun c1/org-get-first-sentence ()
  (require 'org-roam)
  (save-excursion
    (org-roam-end-of-meta-data 'full)
    (while (looking-at org-keyword-regexp)
      (goto-char (match-end 0))
      (forward-line))
    (string-trim (buffer-substring (point) (progn (forward-sentence) (point))))))

(advice-add 'org-id-get-create :override #'my-id-get-or-generate)

(defun ap/org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
  (interactive "P")
  (let* ((new-buffer-p)
         (pos (point))
         (buffer-name (let* ((heading (org-get-heading t t))
                             (level (org-outline-level))
                             (face (intern (concat "outline-" (number-to-string level))))
                             (heading-string (propertize (org-link-display-format heading)
                                                         'face face)))
                        (concat heading-string "::" (buffer-name))))
         (new-buffer (or (get-buffer buffer-name)
                         (prog1 (condition-case nil
                                    (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                  (error (make-indirect-buffer (current-buffer) buffer-name)))
                           (setq new-buffer-p t)))))
    (switch-to-buffer new-buffer)
    (when new-buffer-p
      ;; I don't understand why setting the point again is necessary, but it is.
      (goto-char pos)
      (rename-buffer buffer-name)
      (org-narrow-to-subtree))))

(advice-add 'org-tree-to-indirect-buffer :override 'ap/org-tree-to-indirect-buffer)

(use-package oc
  :ensure nil
  :custom
  (org-cite-global-bibliography `(,(expand-file-name "library.bib" *bibliography-dir*)
                                  "/storage/org/slip-box/lit/refs/bibliography.bib")))

(use-package org-contrib
  :straight t)

(use-package ox-bibtex
  :ensure nil
  :custom
  (org-bibtex-key-property "ID"))

(defun c1/org-bibtex-append (filename)
  "Export each headline in the current file to a bibtex entry.
Headlines are exported using `org-bibtex-headline'."
  (interactive
   (list (read-file-name
          "Bibtex file: " nil nil nil
          (let ((file (buffer-file-name (buffer-base-buffer))))
            (and file
                 (file-name-nondirectory
                  (concat (file-name-sans-extension file) ".bib")))))))
  (let ((error-point
         (catch 'bib
           (let ((bibtex-entries
                  (remove nil (org-map-entries
                               (lambda ()
                                 (condition-case nil
                                     (org-bibtex-headline)
                                   (error (throw 'bib (point)))))))))
             (with-temp-buffer
               (insert (mapconcat #'identity bibtex-entries "\n"))
               (append-to-file (point-min) (point-max) filename))
             (message "Successfully exported %d BibTeX entries to %s"
                      (length bibtex-entries) filename)
             nil))))
    (when error-point
      (goto-char error-point)
      (message "Bibtex error at %S" (nth 4 (org-heading-components))))))

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
  (org-capture-ref-capture-target
   `(:file "/storage/org/gtd/inbox.org"))
  (org-capture-ref-capture-template-set-p t)
  (org-capture-ref-capture-template
   `(:group "org-capture-ref template"
            :type entry
            ,@org-capture-ref-capture-target
            :clock-in nil
            :jump-to-captured t
            :prepare-finalize
            (lambda ()
              (c1/org-bibtex-append "/storage/org/slip-box/lit/other/bibliography.bib"))
            :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
            :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
            :org-entry (lambda () (org-capture-ref-get-org-entry))
            :template
            ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}\n%{outline}%{content}")
            :children (("Interactive + Content"
                        :keys "s"
                        :space " "
                        :content
                        (lambda ()
                          (bir-import (or (buffer-file-name (org-capture-ref-get-buffer-from-html-file-in-query))
                                          (org-capture-ref-get-bibtex-url-from-capture-data)))))
                       ("Interactive + outline"
                        :keys "o"
                        :space " "
                        :content
                        (lambda ()
                          (c1/org-toc-from-html (or (org-capture-ref-get-buffer-from-html-file-in-query)
                                                    (url-retrieve-synchronously (org-capture-ref-get-bibtex-url-from-capture-data))))))
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

(defun c1/org-toc-from-html (html-buffer)
  (let ((out-buf (generate-new-buffer " *occur-toc*")))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (occur-1 "<h[1-6].*>.*</h[1-6]>" "\\&" (list html-buffer) out-buf))
    (with-current-buffer out-buf
      (shell-command-on-region (point-min) (point-max) "pandoc -f html - -t org" out-buf)
      (org-mode)
      (goto-char (point-min))
      (org-map-entries #'org-demote)
      (buffer-string))))

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-after-show . c1/org-maybe-go-to-saved-place))

(use-package org-agenda-dych-mode
  :ensure nil
  :custom
  (org-agenda-dych-default-start "05:00")
  (org-agenda-dych-default-work-hour "16h"))

(defun c1/org-maybe-go-to-saved-place ()
  (let ((beg (org-element-property :begin (org-element-at-point)))
        (end (org-element-property :end (org-element-at-point)))
        (saved-place (assoc buffer-file-name save-place-alist)))
    (when saved-place
      (or revert-buffer-in-progress-p
          (and (integerp (cdr saved-place))
               (and (> (cdr saved-place) beg) (< (cdr saved-place) end))
               (goto-char (cdr saved-place))))
      (setq save-place-mode t))))

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
  :diminish org-gtd-mode
  :bind (:map org-gtd-map
              ("c" . org-gtd-choose)
              ("e" . org-gtd-engage)
              ("E" . org-gtd-plan)
              ("p" . org-gtd-process-inbox)
              ("C" . org-gtd-capture))
  :bind-keymap* ("C-c g" . org-gtd-map)
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
              (org-agenda-breadcrumbs-separator "⋅"))))
      "")
     ("p" "PLanning"
      ((agenda ""
               ((org-agenda-prefix-format " %-5e %t │%(c1/org-agenda-title) ")
                (org-agenda-span 1)
                (org-agenda-sorting-strategy
                 '(time-up habit-down priority-down category-keep))
                (org-overriding-columns-format
                 "%1FIXED %1RIGID %6TIME_ESTIMATE(LENGTH){:} %10Effort(ActLENGTH){:} %22SCHEDULED %30ITEM(TASK) %6CLOCKSUM")
                (org-agenda-start-day nil)
                (org-habit-graph-column 60)
                (org-agenda-overriding-header "")
                (org-agenda-current-time-string "- - - - - - - - - - - - now - - - - - - - - - - - -")
                (org-agenda-time-grid
                 `((daily today require-timed)
                   (800 1000 1200 1400 1600 1800 2000)
                   "" ,(make-string 53 ?\─)))
                (org-agenda-compact-blocks t))))
      "")))
  (org-gtd-directory "/storage/org/gtd/")
  (org-gtd-process-item-hooks nil)
  (org-edna-use-inheritance t)
  :config
  (org-gtd-mode)
  (add-to-list 'org-capture-templates
               `("n" "NEXT action" entry
                 (file+olp ,(file-name-with-extension (expand-file-name org-gtd-default-file-name *gtd-dir*) "org")
                           "Actions")
                 "** NEXT %? %^g\nSCHEDULED: %t"))
  (add-to-list 'org-gtd--agenda-functions #'org-agenda-run-series)
  (add-to-list 'org-gtd--agenda-functions #'org-save-all-org-buffers)
  (transient-insert-suffix 'org-gtd-choose "p" '("r" "Reading" c1/org-gtd--reading))
  (transient-insert-suffix 'org-gtd-choose "p" '("h" "Habit" c1/org-gtd--habit))
  (transient-insert-suffix 'org-gtd-choose "p" '("T" "Topic" c1/org-gtd--topic))
  (transient-insert-suffix 'org-gtd-choose "c" '("D" "Daily" c1/org-gtd--daily)))


(defun c1/org-agenda-title ()
  (let ((title (cadar (org-collect-keywords '("TITLE")))))
    (if title
        (concat title ". ")
      "")))

(defun org-gtd-plan ()
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-buffer-name "*Plan*")
            (org-agenda-window-setup 'other-window)
            (buf))
        (set-window-dedicated-p
         (display-buffer-in-side-window
          (progn (org-agenda nil "p")
                 (setq buf (current-buffer))
                 (delete-window)
                 buf)
          '((side . right)
            (slot . 0)
            (window-width . 58)
            (dedicated . t)
            (window-parameters
             (no-delete-other-windows . t)
             (delete-window . nil))))
         t))
      (org-agenda-dych-mode 1)))

(add-hook 'after-init-hook #'org-gtd-plan 100)

(defun c1/mark-as-project ()
  (interactive)
  (org-set-property "COOKIE_DATA" "todo recursive"))

(add-hook 'org-gtd-process-item-hooks #'c1/mark-as-project)

(defun c1/org-gtd--habit (&optional arg)
  (interactive "P")
  (org-entry-put nil "STYLE" "habit")
  (org-schedule arg))

(defun c1/org-gtd--reading (&optional arg)
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive "P")
  (if (org-gtd--poorly-formatted-project-p)
      (org-gtd--show-error-and-return-to-editing)

    (org-gtd--decorate-item)
    (cl-destructuring-bind
        (first-entry . rest-entries)
        (cdr (org-map-entries #'org-element-at-point t 'tree))
      (org-element-map
          (reverse rest-entries)
          'headline
        (lambda (myelt)
          (org-entry-put (org-gtd-projects--org-element-pom myelt) "TODO" "TODO")))

      (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TODO" "NEXT")
      (org-entry-put (org-gtd-projects--org-element-pom first-entry) "EFFORT"
                     (c1/org-heading-read-time-estimate
                      (org-gtd-projects--org-element-pom first-entry) t))
      (goto-char (org-gtd-projects--org-element-pom first-entry))
      (org-schedule 0)
      
      (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TRIGGER"
                     "tree-walk(2) todo!(NEXT) read-time! scheduled!(\".\") chain!(\"TRIGGER\")"))
    (while (org-up-heading-safe))
    (let ((org-special-ctrl-a t))
      (org-beginning-of-line))
    (insert "[/] ")
    (org-update-statistics-cookies t)
    (org-gtd--refile org-gtd-projects)
    (org-gtd-process-inbox)))

(defun c1/org-heading-read-time-estimate (&optional point as-duration)
  (let (end)
    (org-with-point-at (or point (point))
      (org-back-to-heading t)
      (org-copy-subtree)
      (with-temp-buffer
        (org-mode)
        (org-paste-subtree)
        (replace-regexp-in-region
         (concat org-property-re "\\|" org-keyword-regexp "\\|" org-planning-line-re
                 "\\|" "^[ \t]*#\\+\\(\\S-+?\\):[ \t]*" "\\|" org-drawer-regexp
                 "\\|" org-ts-regexp-both "\\|" org-clock-drawer-re)
         ""
         (point-min) (point-max))
        (flush-lines "^[ \t\n\r]+$")
        (setq end (org-element-property :end (org-element-at-point)))
        (org-back-to-heading t)
        (c1/region-read-time-estimate (point) end as-duration)))))

(defun org-edna-action/read-time! (_entry)
  (org-set-effort nil (c1/org-heading-read-time-estimate nil t)))

(defun c1/region-read-time-estimate (beg end &optional as-duration)
  (let ((case-fold-search t)
        (read-time (* 60 (/ (how-many (rx (+ word)) beg end) 230.0)))
        (img-count 0)
        (pos beg))
    (when (and (eq major-mode 'org-mode) (not org-inline-image-overlays))
      (let ((file-extension-re (image-file-name-regexp))
            (link-abbrevs (mapcar #'car
                                  (append org-link-abbrev-alist-local
                                          org-link-abbrev-alist))))
        (setq img-count (how-many (concat (format "\\[\\[\\(%s\\):\\(?:image/png;base64,\\)?\\([^]]+\\)\\(?:%s\\)?\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
                                                  (regexp-opt (list "http" "https"))
                                                  (substring (image-file-name-regexp) nil -2))
                                          "\\|"
                                          (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                                                  (if (not link-abbrevs) ""
                                                    (concat "\\|" (regexp-opt link-abbrevs)))))
                                  beg end))))
    (while (and (not (= pos end))
                (setq pos (next-single-property-change pos 'display nil end)))
      (when (ignore-errors (image--get-image pos))
        (cl-incf img-count)))
    
    (cond ((= img-count 1)
           (cl-incf read-time 12))
          ((= img-count 2)
           (cl-incf read-time 12)
           (cl-incf read-time 11))
          ((>= img-count 3)
           (cl-incf read-time 12)
           (cl-incf read-time 11)
           (cl-incf read-time (* 10 (- img-count 2)))))
    
    (if as-duration
        (org-duration-from-minutes (/ read-time 60))
      (/ read-time 60))))

(defun org-edna-finder/tree-walk (root-level)
  (save-excursion
    (outline-next-heading)
    (when (> (outline-level) root-level)
      (list (point-marker)))))



(defun c1/org-gtd--topic ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)
  (require 'org-roam)
  (require 'org-fc)
  (when-let ((id (org-id-get-create)))
    (while (org-up-heading-safe))
    (c1/org-bibtex-append "/storage/org/slip-box/lit/other/bibliography.bib")
    (org-cut-subtree)
    (with-temp-buffer
      (org-mode)
      (org-paste-subtree)
      (org-roam-ref-add (concat "cite:&" id))
      (write-region (point-min) (point-max)
                    (concat "/storage/org/slip-box/lit/" id ".org")
                    nil t nil t)
      (save-buffer))
    (org-gtd-process-inbox)
    (pop-to-buffer
     (find-file-noselect
      (concat "/storage/org/slip-box/lit/" id ".org")))))

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



(provide 'init-org)
;;; init-org.el ends here
