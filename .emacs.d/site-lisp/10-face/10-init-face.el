;;; 10-init-face.el --- The look of emacs
;;;; Modus themes
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :ensure t)
;;;; Grayscale theme for the night
(use-package tao-theme
  :ensure t)
;;;; Diminish
(use-package diminish
  :ensure t)
;;;; Iosevka font
(set-face-attribute 'default nil :font "Iosevka SS09-14")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile Oblique")
;;;; EXWM
(use-package exwm
  :ensure
  :when window-system
  :config
  (use-package exwm-edit :ensure t)
  (use-package pinentry :ensure t)
  (use-package exwm-config
    :ensure nil
    :custom
    ;; Prefix keys to ignore
    (exwm-input-prefix-keys `(?\C-x ?\C-u ?\C-h ?\C-g ?\M-x ?\M-` ?\M-& ?\M-:))
    (exwm-input-global-keys (append exwm-input-global-keys
				  (mapcar '(lambda (key)
					     (cons (vector (car key)) (cdr key)))
					  (cdr exwm-input-global-keys-map))))
    ;; Line-editing keybindings for X windows
    (exwm-input-simulation-keys '(;; Backward-char
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
				  ;; Down screenful
				  ([?\M-v] . [prior])
				  ;; Up screenful
				  ([?\C-v] . [next])
				  ;; Delete following char
				  ([?\C-d] . [delete])
				  ;; Kill(Cut) the input line
				  ([?\C-k] . [S-end C-x])
				  ;; Yank(Paste)
				  ([?\C-y] . [C-v])))
    :init

    :config
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

(provide '10-init-face.el)
;;; 10-init-face ends here
