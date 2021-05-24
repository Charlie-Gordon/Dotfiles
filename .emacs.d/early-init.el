;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.

;;; Initialise installed packages
(setq package-enable-at-startup t)

;;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
