;;; init-10-face.el --- The look of emacs
;;;; Fringe
(fringe-mode 4)
;;;; Time in modeline
(display-time-mode 1)
(setq display-time-day-and-date t)
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
;;;; FreeSans for unicode
(set-fontset-font t nil (font-spec :family "FreeSans"
				   :size 20))
;;;; EXWM
(use-package exwm
  :when window-system
  :config
  (use-package exwm-config
    :custom
    ;; Prefix keys to ignore
    (exwm-input-prefix-keys `(?\C-x
			      ?\C-u
			      ?\C-h
			      ?\C-g
			      ?\M-x
			      ?\M-`
			      ?\M-&
			      ?\M-:))
    ;; Global keys for EXWM
    (exwm-input-global-keys `(([?\s-.] . reload-emacs-configuration)
			      ([?\s-w] . exwm-workspace-switch)
			      ([?\s-&] . (lambda (command)
					   (interactive (list (read-shell-command "$ ")))
					   (start-process-shell-command command nil command)))
			      ([?\s-d] . modus-themes-toggle)
			      ([?\s-s] . magit-status-dotfiles)
			      ([?\s-c] . (lambda (package-name)
					   (interactive"P")
					   (consult-ripgrep user-emacs-directory
							    (concat "\\(use-package " package-name))))
			      (,(kbd "s-<return>") . eshell)
			      ,@(mapcar (lambda (i)
					  `(,(kbd (format "s-%d" i)) .
					    (lambda ()
					      (interactive)
					      (exwm-workspace-switch-create ,i))))
					(number-sequence 0 9))))
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
    ;; Set the initial workspace number
    :config
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 4))
    ;; Proper modeline
    (add-hook 'exwm-input-input-mode-change-hook 'force-mode-line-update)
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook #'(lambda ()
					  (exwm-workspace-rename-buffer exwm-class-name)))
    ;; Fix Ido
    ;; (exwm-config-ido)
    ;; (ido-mode 0)
    ;; Enable EXWM
    (exwm-enable)
    :ensure nil)
  :ensure t)

(provide 'init-10-face.el)
