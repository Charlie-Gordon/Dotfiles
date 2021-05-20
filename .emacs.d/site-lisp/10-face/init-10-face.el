;;; init-10-face.el --- The look of emacs
;;;; Fringe
(fringe-mode 4)
;;;; Time in modeline
;; (setq mode-line-format
;;       '("%e" "%z" mode-line-front-space mode-line-mule-info mode-line-client mode- mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;  (vc-mode vc-mode)
 ;; "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
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
;;;; EXWM
(use-package exwm
  :when window-system
  :config
  (use-package exwm-config
    :after selectrum
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
    (exwm-input-global-keys `(([?\s-.] . #'reload-emacs-configuration)
			      ([?\s-w] . exwm-workspace-switch)
			      ([?\s-&] . (lambda (command)
					   (interactive (list (read-shell-command "$ ")))
					   (start-process-shell-command command nil command)))
			      ([?\s-d] . #'modus-themes-toggle)
			      ([?\s-s] . #'magit-status-dotfiles)
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
    ;; Fix Ido
    (exwm-config-ido)
    (ido-mode 0)
    ;; Proper modeline
    (add-hook 'exwm-input-input-mode-change-hook 'force-mode-line-update)
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
	      (lambda ()
      		(exwm-workspace-rename-buffer exwm-class-name)))
    ;; Prefered completion
    (selectrum-mode +1)
    ;; Enable EXWM
    (exwm-enable)
    :ensure nil)
  :ensure t)
