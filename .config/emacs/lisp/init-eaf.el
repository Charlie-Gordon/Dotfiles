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
  (if (cl-remove-if-not (lambda (ext)
                          (member ext eaf-pdf-extension-list))
                        eaf-find-file-ext-blacklist)
      (setq eaf-find-file-ext-blacklist
            (cl-remove-if (lambda (ext)
                            (member ext eaf-pdf-extension-list))
                          eaf-find-file-ext-blacklist))
    (mapc (lambda (ext)
            (push ext eaf-find-file-ext-blacklist))
          eaf-pdf-extension-list)))

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

(provide 'init-eaf)
;;; init-eaf.el ends here
