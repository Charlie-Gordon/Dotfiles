;;; early-init.el --- Emacs 27+ pre-initialization config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.

;;; Code:

;;;; Initialise installed packages
(setq package-enable-at-startup nil)

;;;; Is this operating system an Android?
(defconst *termux* (string-match "Android" (shell-command-to-string "uname -a")))

(if *termux*
    (menu-bar-mode -1)
;;;; Disable GUI elements
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 4)
  ;; Time in modeline
  (display-time-mode 1)
  (setq display-time-day-and-date t))
(setq enable-recursive-minibuffers t)
;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)
(setq pop-up-windows nil)
;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)
;; Optional additional aesthetic changes
;; Adapted from `sanity.el' in Elegant Emacs by Nicolas P. Rougier (rougier)
;; https://github.com/rougier/elegant-emacs
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(provide 'early-init)
;;; early-init.el ends here

