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

;;;; EPUB(with nov.el)
(use-package nov
  :load-path "site-lisp/nov.el/"
  :preface
  (use-package esxml
    :straight t)
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
  (pdf-view-resize-factor 1.1))

;;;; Trying out SRS (space-repetition system)
(use-package anki-editor
  :straight t)

(use-package org-fc
  :straight '(org-fc :type git :host github :repo "l3kn/org-fc"))

(use-package org-drill
  :straight t)

(provide 'init-notetake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notetake.el ends here
