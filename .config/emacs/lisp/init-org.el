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
  (org-directory *org-dir*)
  (org-export-coding-system 'utf-8)
  (org-use-speed-commands t)
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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)))

(use-package worf
  :straight t)


(use-package org-edna
  :straight t
  :hook
  (org-mode . org-edna-mode)
  :config
  (defun org-edna-action/org-anki-this! (_last-entry)
    "Action to call `org-anki-sync-entry' on this entry
Edna Syntax: org-anki-this!"
    (org-anki-sync-entry))
  :diminish)

(use-package org-make-toc
  :straight t)

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-agenda
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
  (add-hook 'after-init-hook (lambda nil (org-set-agenda-files-recursively *org-dir*)))
  
  (setq org-agenda-custom-commands
        '(("R" "List of all headline with REVIEW keyword." search "REVIEW"
           ((org-show-context-detail 'minimal)
            (org-agenda-prefix-format ""))))))
;;;###autoload
(defun org-maybe-go-to-quiz ()
  "Go to the first todo element with \"QUIZ\" keyword in current file, do nothing if not found."
  (cond ((derived-mode-p 'org-mode)
         (goto-char
          (or (car (org-map-entries
	            (lambda nil (point-marker))
                    "todo=\"QUIZ\"" 'file))
	      (point-marker))))
        ((and (derived-mode-p 'eaf-mode)
              (string= eaf--buffer-app-name "pdf-viewer"))
         (eaf-interleave-sync-current-note)
         (select-window (get-buffer-window eaf-interleave-org-buffer))
         (org-maybe-go-to-quiz))))

(use-package org-super-agenda
  :straight t
  :hook (org-agenda-mode . org-super-agenda-mode))


(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   `(("Q"
      "Questions for this book.")
     ("Qa"
      "Anki flashcard."
      entry
      (file "/storage/org/anki.org")
      "* REVIEW %?")
     ("Qb"
      "Questions bank on this book."
      entry
      (function org-maybe-go-to-quiz)
      (file "~/.emacs.d/org-template/QUIZ-todo.txt"))
     ("Qq"
      "A question for this book."
      entry
      (function org-maybe-go-to-quiz)
      (file "~/.emacs.d/org-template/QUIZ-question.txt"))
     ("Q4"
      "Essential four questions for reading, from Adler's How to Read A Book"
      entry
      (file+function buffer-file-name org-maybe-go-to-quiz)
      ,(string-join
        (list
         "** REVIEW What is [[file:%F][%(file-name-sans-extension \"%f\")]] about as a whole?"
         "** REVIEW What [[file:%F][%(file-name-sans-extension \"%f\")]] said in detail, and how?"
         "** REVIEW Is [[file:%F][%(file-name-sans-extension \"%f\")]] true, in whole or part?"
         "** REVIEW What of [[file:%F][%(file-name-sans-extension \"%f\")]]?")
        "\n")))))

(use-package edraw-org
  :ensure nil
  :mode
  ("\\.edraw\\.svg$" . edraw-mode))


;;;; Recur

(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :straight t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))


(use-package org-ql
  :straight t)

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
