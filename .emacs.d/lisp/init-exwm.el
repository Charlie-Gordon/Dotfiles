;;; init-exwm.el --- Emacs X Window Manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/kbd-setup ()
  (interactive)
  (shell-command-to-string (string-join 
                            `(,(executable-find "setxkbmap")
                              "-model pc104aw-zqu"
                              "-layout us,us,th"
                              "-variant cmk_ed_dh,,pat"
                              "-option grp:alt_space_toggle")
                            " "))
  (shell-command-to-string (concat (executable-find "xmodmap") " -verbose "
                                   (getenv "USERMODMAP"))))

(add-hook 'after-init-hook #'my/kbd-setup)

(global-set-key (kbd "<f8>") #'my/kbd-setup)

(defun ambrevar/exwm-rename-buffer-to-title ()
  "Rename EXWM buffer to its window title."
  (exwm-workspace-rename-buffer exwm-title))

(use-package pinentry
  :straight t
  :config
  ;; Get encryption established
  (setf epg-pinentry-mode 'loopback)
  (pinentry-start)
  ;; Support for encrytion (gpg)
  (defun pinentry-emacs (desc prompt ok error)
    (let ((str (read-passwd
                (concat
                 (replace-regexp-in-string "%22" "\""
                                           (replace-regexp-in-string
                                            "%0A" "\n" desc))
                 prompt ": "))))
      str)))

(use-package exwm
  :straight t
  :when window-system
  :config
  (use-package exwm-config
    :ensure nil
    :config
    (setq
     ;; Prefix keys to ignore
     exwm-input-prefix-keys '(?\C-x
                              ?\C-u
                              ?\C-h
                              ?\C-g
                              ?\C-'
                              ?\C-,
                              ?\M-x
                              ?\M-`
                              ?\M-&
                              ?\M-:)
     ;; Line-editing keybindings for X windows
     exwm-input-simulation-keys '(;; Backward-char
				  ([?\C-b] . [left])
				  ;; Forward-char
				  ([?\C-f] . [right])
				  ;; Previous-line
				  ([?\C-p] . [up])
				  ;; Next-line
				  ([?\C-n] . [down])
				  ;; Beginning of the line
				  ([?\C-a] . [home])
				  ;; End of the line
				  ([?\C-e] . [end])
				  ;; Forward-word
				  ([?\M-f] . [C-right])
				  ;; Backward-word
				  ([?\M-b] . [C-left])
                                  ;; Delete-word
                                  ([?\M-d] . [C-delete])
				  ;; Down screenful
				  ([?\M-v] . [prior])
				  ;; Up screenful
				  ([?\C-v] . [next])
				  ;; Delete following char
				  ([?\C-d] . [delete])
				  ;; Kill(Cut) the input line
				  ([?\C-k] . [S-end C-x])
				  ;; Yank(Paste)
				  ([?\C-y] . [C-v]))
     exwm-input-global-keys
     `(;; 's-r': Reset (to line-mode).
       ([?\s-r] . exwm-reset)
       ;; 's-w': Switch workspace.
       ([?\s-w] . exwm-workspace-switch)
       ;; 's-&': Launch application.
       ([?\s-&] . (lambda (command)
                    (interactive (list (read-shell-command "$ ")))
                    (start-process-shell-command command nil command)))
       ;; 's-.': Reload init.el
       ([?\s-.] . my/reload-emacs-configuration)
       ([?\s-d] . modus-themes-toggle)
       (,(kbd "<print>") . screenshot-svg)
       ;; Eshell
       (,(kbd "s-<return>") . eshell)
       ;; 's-N': Switch to certain workspace.
       ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-%d" i)) .
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create ,i))))
                 (number-sequence 0 9))))
    (setq exwm-manage-force-tiling t)
    ;; Set the initial workspace number
    (setq exwm-workspace-number 4)
    ;; Proper modeline
    (add-hook 'exwm-input-input-mode-change-hook 'force-mode-line-update)
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook #'(lambda ()
					  (exwm-workspace-rename-buffer exwm-class-name)))
    ;; Default is save-buffers-kill-terminal, but that may kill daemon before its finished
    (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
    
    (add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)
    ;; Enable EXWM
    (exwm-enable)))

(use-package exwm-edit
  :straight t
  :bind
  (:map exwm-mode-map
        ("C-c '" . exwm-edit--compose))
  :hook
  (exwm-edit-compose . visual-line-mode))

(provide 'init-exwm)
;;; init-exwm.el ends here
