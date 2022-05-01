;;; init-face.el --- The look of emacs
;;;; Modus themes
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :straight t
  :custom
  (modus-themes-links '(bold faint))
  (modus-themes-org-agenda '((scheduled . rainbow)))
  (modus-themes-region '(accented))
  (modus-themes-org-blocks '(gray-background)))
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
(set-face-attribute 'variable-pitch nil :font "New Century Schoolbook" :height 175)

(set-fontset-font
 t
 (if (version< emacs-version "28.1")
     '(127744 . 129744)
   'emoji)
 (cond
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")))

(add-to-list 'face-font-rescale-alist '("Umpush" . 0.85))

(provide 'init-face)
;;; init-face.el ends here
