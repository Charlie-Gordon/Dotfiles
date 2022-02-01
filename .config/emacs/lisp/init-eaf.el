;;; init-eaf.el --- Emacs application framework -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package eaf
  :straight '(emacs-application-framework
              :type git
              :host github
              :repo "emacs-eaf/emacs-application-framework"
              :files ("core" "extension" "*.el" "*.py")))

(defun eaf-toggle ()
  (interactive)
  (if (member "pdf" eaf-find-file-ext-blacklist)
      (setq eaf-find-file-ext-blacklist
            (delete "pdf" eaf-find-file-ext-blacklist))
    (push "pdf" eaf-find-file-ext-blacklist)))

(use-package eaf-org
  :ensure nil
  :custom
  (eaf-org-override-pdf-links-store t))


(use-package eaf-pdf-viewer
  :straight '(eaf-pdf-viewer
              :type git
              :host github
              :repo "emacs-eaf/eaf-pdf-viewer"
              :files ("core" "app" "*.el" "*.py"))
  :custom
  (eaf-pdf-outline-window-configuration t)
  (eaf-pdf-dark-mode "ignore")
  (eaf-pdf-marker-fontsize 16)
  :config
  (eaf-bind-key scroll_up "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "<backspace>" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key jump_to_page "M-g l" eaf-pdf-viewer-keybinding))


;;;###autoload
(defun eaf-interleave--find-bibtex-note (filename)
  (run-hook-with-args-until-success
   'bibtex-completion-find-note-functions
   (car (bibtex-completion-find-key-from-pdf filename))))

(use-package eaf-interleave
  :ensure nil
  :disabled
  :after (bibtex-completion eaf-pdf-viewer)
  :hook
  (org-mode . eaf-interleave-mode)
  (eaf-pdf-viewer . eaf-interleave-app-mode)
  :bind
  (:map eaf-interleave-mode-map
        ("M-." . eaf-interleave-sync-pdf-page-current)
        ("M-n" . eaf-interleave-sync-pdf-page-next)
        ("M-p" . eaf-interleave-sync-pdf-page-previous))
  (:map eaf-interleave-app-mode-map
        ("q" . eaf-interleave-quit)
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
