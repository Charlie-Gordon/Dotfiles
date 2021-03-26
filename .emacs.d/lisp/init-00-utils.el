;;; init-00-utils.el --- Functions used by other el files
;;;; Reload configuration
(defun reload-emacs-configuration ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Dired with root permissions
(defun sudired (editing-directory)
  "Dired with root permission"
  (interactive "D")
  (dired-at-point (concat "/doas:root@localhost:" (expand-file-name editing-directory))))
;;;; Toggle theme function
 (defun modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes."
    (interactive) 
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (modus-themes-load-vivendi))
    (modus-themes-load-operandi)))
      
;;;; Find-file with root permissions
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
	       (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/doas:root@localhost:" buffer-file-name))))
;;;; EWW reflow document when scaled text
(defun text-scale-mode-hook ()
  (eww-reload :local))

(provide 'init-00-utils.el)


