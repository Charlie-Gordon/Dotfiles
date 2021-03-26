;;; init-00-utils.el --- Functions used by other el files
;;;; Reload configuration
(defun reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Dired with root permissions
(defun sudired (editing-directory)
  "Dired with root permission."
  (interactive "D")
  (dired-at-point (concat "/doas:root@localhost:" (expand-file-name editing-directory))))
;;;; Find-file with root permissions
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
	       (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/doas:root@localhost:" buffer-file-name))))
;;;; EWW reflow document when scaled text
(defun text-scale-mode-hook ()
  "Rerender content of EWW when uses text-scale mode."
  (eww-reload :local))
;;;; Magit shortcuts
(defun magit-status-dotfiles ()
  "Open magit in dotfiles repository."
  (magit-status "/yadm::"))
(provide 'init-00-utils.el)


