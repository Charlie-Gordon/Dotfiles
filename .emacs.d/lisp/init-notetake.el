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
;;;; Calibre
(use-package calibredb
  :ensure nil
  :config
  (setq calibredb-root-dir "/storage/library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("/storage/library/"))))

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
  (use-package pdf-view-restore
    :straight t
    :custom
    (pdf-view-restore-filename "/storage/journals/library/.pdf-view-restore"))
  :hook (pdf-view-mode . pdf-view-restore-mode)
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

;;;; Trying out SRS (space-repetition system)
(use-package anki-editor
  :straight t)

(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
