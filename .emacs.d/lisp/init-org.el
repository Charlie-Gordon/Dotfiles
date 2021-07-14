;;; init-org.el --- Code for initializing org-mode -*- lexical-binding: t; -*-
;;;; Org
(use-package org
  :termux
  :commands org-anki-sync-entry
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
  (org-mode . visual-line-mode))

(use-package org-edna
  :termux
  :straight t
  :hook
  (org-mode . org-edna-mode)
  :config
  (defun org-edna-action/org-anki-this! (_last-entry)
    "Action to call `org-anki-sync-entry' on this entry
Edna Syntax: org-anki-this!"
    (org-anki-sync-entry))
  :diminish)

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-agenda
  :termux
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
  (org-set-agenda-files-recursively *journals-dir*)
  (setq org-agenda-custom-commands
        '(("R" "List of all headline with REVIEW keyword." search "REVIEW"
           ((org-show-context-detail 'minimal)
            (org-agenda-prefix-format ""))))))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   `(("Q"
      "Questions for this book.")
     ("Qa"
      "Anki flashcard."
      entry
      (file "/storage/journals/org/anki.org")
      "* REVIEW Q:%?")
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
      ,(string-join
        (list
         "** REVIEW Q:What is [[file:%F][%(file-name-sans-extension \"%f\")]] about as a whole?"
         "** REVIEW Q:What [[file:%F][%(file-name-sans-extension \"%f\")]] said in detail, and how?"
         "** REVIEW Q:Is [[file:%F][%(file-name-sans-extension \"%f\")]] true, in whole or part?"
         "** REVIEW Q:What of [[file:%F][%(file-name-sans-extension \"%f\")]]?")
        "\n"))))
  :config
  (defun org-maybe-go-to-quiz ()
    "Go to the first todo element with \"QUIZ\" keyword in current file, do nothing if not found."
    (goto-char
     (or (car (org-map-entries
	       (lambda nil (point-marker)) "todo=\"QUIZ\"" 'file))
	 (point-marker)))))

(use-package org-anki
  :termux
  :straight '(org-anki :type git :host github :repo "eyeinsky/org-anki"
                       :fork t)
  :bind (:map org-mode-map
              ("C-c C-'" . org-anki-cloze-dwim))
  :custom
  (org-anki-default-deck "one-big-deck"))

(use-package org-ref
  :straight t
  :custom
  (reftex-default-bibliography `(,(expand-file-name "muhbib.bib" *library-dir*)))
  (org-ref-bibliography-notes (expand-file-name "org/bib-notes.org" *journals-dir*))
  (org-ref-notes-function #'orb-notes-fn)
  (org-ref-pdf-directory *library-dir*)
  (org-ref-default-bibliography reftex-default-bibliography)
  (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex))


;;;; Outshine
(use-package outshine
  :straight t
  :after outline
  :config
  ;; Required for outshine
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  ;; Enables outline-minor-mode for *ALL* programming buffers
  (add-hook 'prog-mode-hook 'outshine-mode)
  :diminish)

(provide 'init-org)
;;; init-org.el ends here
