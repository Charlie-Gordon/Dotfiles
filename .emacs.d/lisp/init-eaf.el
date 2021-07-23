;;; init-eaf.el --- Emacs application framework -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/eaf-maybe-format-mode-line ()
  (when (derived-mode-p 'eaf-mode)
    (let ((app-mode-line-format
           (intern (concat "eaf-" eaf--buffer-app-name "-mode-line-format"))))
      (when (custom-variable-p app-mode-line-format)
        (setq mode-line-format (default-value app-mode-line-format))
        (force-mode-line-update)))))

(defun my/eaf-pdf-viewer-maybe-set-total-page ()
  (or eaf-pdf-viewer-total-page
      (setq eaf-pdf-viewer-total-page (eaf-call-sync "call_function" eaf--buffer-id "page_total_number"))))

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
  (eaf-setq eaf-browser-enable-adblocker "true")
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
  (add-hook 'eaf-pdf-viewer-hook #'eaf-activate-emacs-window)
  (add-hook 'eaf-pdf-viewer-hook #'my/eaf-maybe-format-mode-line)
  (add-hook 'eaf-pdf-viewer-hook #'my/eaf-pdf-viewer-maybe-set-total-page)
  (add-hook 'eaf-pdf-viewer-hook #'eaf-interleave-app-mode)
  (defvar-local eaf-pdf-viewer-total-page nil)
  (defvar-local eaf-pdf-viewer-current-page nil))

(defcustom eaf-pdf-viewer-mode-line-format
  '(" "
    "("
    "/"
    (:eval (my/eaf-pdf-viewer-maybe-set-total-page))
    ")"
    "  "
    "%b" "  " mode-lines-modes mode-line-misc-info
    )
  "Mode line format for EAF's pdf-viewer application.")

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
              ("M-." . eaf-interleave-sync-current-note)
              ("M-N" . eaf-interleave-sync-next-note)
              ("M-P" . eaf-interleave-sync-previous-note))
  :config
  (add-to-list 'eaf-interleave-org-notes-dir-list bibtex-completion-notes-path)
  (setq eaf-interleave-disable-narrowing t
        eaf-interleave--url-prop org-noter-property-doc-file
        eaf-interleave--find-note-path-function #'eaf-interleave--find-bibtex-note))

(provide 'init-eaf)
;;; init-eaf.el ends here
