;;; init-face.el --- The look of emacs
;;;; Modus themes
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :straight t)
;;;; Grayscale theme for the night
(use-package tao-theme
  :straight t)
;;;; Diminish
(use-package diminish
  :straight t)
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile Oblique")
;; FreeSans for unicode
;; (set-fontset-font t nil (font-spec :family "FreeSans"
;; 				   :size 20))
;;;; EXWM
(use-package exwm
  :straight t
  :when window-system
  :config
  (use-package pinentry :straight t)
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
  ;; Set the initial workspace number
  (setq exwm-workspace-number 4)
  ;; Proper modeline
  (add-hook 'exwm-input-input-mode-change-hook 'force-mode-line-update)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook #'(lambda ()
					(exwm-workspace-rename-buffer exwm-class-name)))
  ;; Enable EXWM
  (exwm-enable)
  ;; Get encryption established
  (setf epg-pinentry-mode 'loopback)
  (pinentry-start)))

(provide 'init-face)
;;; init-face.el ends here
