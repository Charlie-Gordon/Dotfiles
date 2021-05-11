;;; init-10-face.el --- The look of emacs
;;;; Fringe
(fringe-mode 8)
;;;; Time in modeline
(setq mode-line-format
      '("%e" "%z" mode-line-front-space mode-line-mule-info mode-line-client mode- mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
(display-time-mode 1)
(setq display-time-day-and-date t)
;;;; Maximize frame
;;;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;; Modus themes
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :ensure t)
;;;; Diminish
(use-package diminish
  :ensure t)
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
;;;; FreeSans for unicode
(set-fontset-font t nil (font-spec :family "FreeSans"
				     :size 20))
(provide 'init-10-face.el)
