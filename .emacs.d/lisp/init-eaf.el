;;; init-eaf.el --- Emacs application framework -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eaf
  :load-path "site-lisp/emacs-application-framework"
  :init
  (use-package epc :defer t :straight t)
  (use-package ctable :defer t :straight t)
  (use-package deferred :defer t :straight t)
  (use-package s :defer t :straight t)
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-search-engines '(("searx" . "https://searx.bar")
                                ("duckduckgo" . "https://duckduckgo.com/?q=%s")))
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-pdf-outline-window-configuration t)
  :config
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))

(use-package eaf-pdf-viewer
  :ensure nil
  :custom
  (eaf-pdf-outline-window-configuration t)
  :config
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "<backspace>" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (add-hook 'eaf-pdf-viewer-hook #'eaf-interleave-app-mode))

(use-package eaf-org
  :ensure nil
  :config
  (defun eaf-org-open-file (file &optional link)
    (eaf-open file))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file)))

;;;###autoload
(defun eaf-interleave--find-bibtex-note (filename)
  (run-hook-with-args-until-success
   'bibtex-completion-find-note-functions
   (car (bibtex-completion-find-key-from-pdf filename))))

(use-package eaf-interleave
  :ensure nil
  :after (bibtex-completion eaf-pdf-viewer)
  :bind
  (:map eaf-interleave-mode-map
        ("M-." . eaf-interleave-sync-pdf-page-current)
        ("M-n" . eaf-interleave-sync-pdf-page-next)
        ("M-p" . eaf-interleave-sync-pdf-page-previous))
  (:map eaf-interleave-app-mode-map
        ("M-i" . eaf-interleave-add-note)
        ("M-." . eaf-interleave-sync-current-note)
        ("M-N" . eaf-interleave-sync-next-note)
        ("M-P" . eaf-interleave-sync-previous-note))
  :config
  (add-to-list 'eaf-interleave-org-notes-dir-list bibtex-completion-notes-path)
  (setq eaf-interleave-disable-narrowing t
        eaf-interleave--find-note-path-function #'eaf-interleave--find-bibtex-note))

(provide 'init-eaf)
;;; init-eaf.el ends here
