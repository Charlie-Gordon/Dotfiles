;;; init-pdftools.el --- Configure pdf-tools -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Dependencies and Setup
(use-package pdf-tools
  :straight avy `(pdf-avy-highlight :type git :host github :repo "dalanicolai/dala-emacs-lisp"
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

(provide 'init-pdftools)
;;; init-pdftools.el ends here
