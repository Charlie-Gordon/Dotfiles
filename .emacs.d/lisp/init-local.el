;;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Buffer management
;; Use `ibuffer' instead of `list-buffers'
(use-package ibuffer
  :termux
  :ensure nil
  :bind (:map ctl-x-map
	      ("C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil "Do not open on the other window; use the current one.")
  (ibuffer-show-empty-filter-groups nil "Do not show empty filter groups.")
  (ibuffer-movement-cycle nil "Do not go to the top when moving downward at the last item on the list.")
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil))
;;;; Window management
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(;; Top side window
     ("\\*Messages.*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 1))
     ("\\*Warnings.*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 0))
     ;; Right side window
     ("\\*[Hh]elp\*.*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0))
     ("\\*Faces\\|Colors\\*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0))
     ("\\qutebrowser.*"
      (display-buffer-in-side-window)
      (window-width . 0.45)
      (side . right)
      (slot . 0))
     ("\\mpv.*"
      (display-buffer-in-side-window)
      (window-height . 0.45)
      (side . right)
      (slot . 2))
     ;; Bottom buffer
     ("\\*.*\\(e?shell\\|v?term\\).*"
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.2))))
  (window-combination-resize t)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop))

;;;; LBRY
(straight-use-package
 '(lbry-mode.el :type git :host gitlab :repo "Charlie-Gordon/lbry-mode-el"))
;;;; PARALLEL
(straight-use-package
 '(parallel-mode.el :type git :host gitlab :repo "Charlie-Gordon/parallel-mode.el"))
;;;; EMMS
(use-package emms
  :straight t
  :config
  (use-package emms-setup
    :ensure nil
    :custom
    (emms-source-file-default-directory "/storage/journals/resources/")
    :config
    (emms-all)
    (emms-default-players)))
;;;; Pass
(use-package password-store
  :straight t)
;;;; Torrent
(use-package transmission
  :straight t)
;;;; MPV
(use-package mpv
  :straight t)
;;;; Vterm
(use-package vterm
  :straight t)

(provide 'init-local)
;;; init-local.el ends here
