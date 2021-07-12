;;; init-eaf.el --- Emacs application framework -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
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
  :config
  (add-to-list 'eaf-wm-focus-fix-wms "wmctrl -m")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))
;; unbind, see more in the Wiki

(use-package eaf-org
  :ensure nil
  :config
  (defun eaf-org-open-file (file &optional link)
    (eaf-open file))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file)))

(use-package eaf-interleave
  :ensure nil
  :custom
  (eaf-interleave--page-note-prop org-noter-property-note-location))

(provide 'init-eaf)
;;; init-eaf.el ends here
