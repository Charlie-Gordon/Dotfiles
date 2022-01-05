;;; init-face.el --- The look of emacs
;;;; Modus themes
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :straight t)
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile Oblique")

(add-to-list 'face-font-rescale-alist '("Umpush" . 0.85))
(add-to-list 'face-font-rescale-alist '(".*sans forgetica.*" . 0.9))

(provide 'init-face)
;;; init-face.el ends here
