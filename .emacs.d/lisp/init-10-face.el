;;; init-10-face.el --- The look of emacs
;;;; Screenful
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)
;;;; Remove lame startup screen
(setq inhibit-startup-message t)
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
(use-package modus-themes)
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
;;;; Font for Thai character
(set-fontset-font t 'thai (font-spec :family "TH SarabunPSK"
				     :size 24))
(provide 'init-10-face.el)
