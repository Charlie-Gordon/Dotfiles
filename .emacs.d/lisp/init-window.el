;;; init-window.el --- Window configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package winner
  :ensure nil
  :bind
  ("s-<left>" . winner-undo)
  ("s-<right>" . winner-redo)
  :config
  (winner-mode))

(use-package ace-window
  :straight t
  :custom
  (aw-keys '(?q ?w ?f ?a ?r ?s ?t ?x ?c ?d ?v))
  (aw-background nil)
  (aw-display-mode-overlay nil)
  :config
  (ace-window-display-mode t)
  (global-set-key (kbd "C-x o") #'ace-window))

;; Configure ‘display-buffer’ behaviour for some special buffers
(setq display-buffer-alist
      `(;; Messages, errors, processes, Calendar in the bottom side window
        (,(rx bos (or "*Apropos"                ; Apropos buffers
                      "*Man"                    ; Man buffers
                      "*Process List*"          ; Processes
                      "*Proced"                 ; Proced processes list
                      "*compilation"            ; Compilation buffers
                      "*Flycheck errors*"       ; Flycheck error list
                      "*Calendar"               ; Calendar window
                      "*env-info"               ; Environment information
                      "*Cargo"                  ; Cargo process buffers
                      "*Word"                   ; WordNut buffers
                      "*Reconcile*"             ; Reconcile in ledger-mode
                      (and (1+ nonl) " output*"))) ; AUCTeX command output
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.45))
        ;; REPLs on the bottom half
        (,(rx bos (or "*cider-repl"     ; CIDER REPL
                      "*intero"         ; Intero REPL
                      "*idris-repl"     ; Idris REPL
                      "*ielm"           ; IELM REPL
                      "*SQL"))          ; SQL REPL
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.50))
        (,(rx bos "*Messages" (* anything))
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        (,(rx bos "*" (or "Backtrace" "Warnings" "Compile-Log") "*") 
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2)
         (window-parameters . ((no-other-window . t))))
        (,(rx bos "*Outline" (* anything) "*")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-width . 80)
         (side . right))
        (,(rx bos "*Help" (* anything) "*")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-width . 80)
         (side . right))
        (,(rx bos (* anything) "mpv")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-width . 0.3)
         (side . right)
         (side . 0))
        (,(rx bos "*Faces*")
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 0))
        (,(rx bos "*Custom" (* anything) "*")
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 1))
        ;; bottom buffer (NOT side window)
        ("\\*\\vc-\\(incoming\\|outgoing\\).*"
         (display-buffer-at-bottom))
        (,(rx bos "*" (or "Output" "Register Preview") (* anything)) 
         (display-buffer-at-bottom))
        (,(rx bos "*" (or (and anything "shell")
                          (and anything "term")))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.2))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(provide 'init-window)
;;; init-window.el ends here


