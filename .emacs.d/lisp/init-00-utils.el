;;; init-00-utils.el --- Functions used by other el files
;;;; Reload configuration
(defun reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Dired with root permissions
;; (defun sudired (editing-directory)
;;   "Dired with root permission."
;;   (interactive "D")
;;   (dired-at-point (concat "/doas:root@localhost:" (expand-file-name editing-directory))))
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
;;;; EWW thingy
(defun eww-parse-content (url) 
  "Use `url-retrieve then `eww-render and output results in a hidden buffer called `output
then append everything to the actual buffer called `Content"
  (let ((url-mime-accept-string eww-accept-content-types))
    (progn
      (url-retrieve (eww--dwim-expand-url url) 'eww-render
		    (list (eww--dwim-expand-url url) nil (get-buffer-create " output")))
      (with-current-buffer " output"
	(prepend-to-buffer "Content" (point-min) (point-max))
      (with-current-buffer "Content"
	(goto-char (point-max))
	(newline))))))
;;;; Magit shortcuts
(defun magit-status-dotfiles ()
  (interactive)
  "Open magit in dotfiles repository."
  (magit-status "/yadm::"))
;;;; Fixing EMMS volume control
;; (defadvice emms-volume-change-function (after fbsd-mixer-volume-change activate)
;;   "Fixes to work with fbsd's mixer."
;;   (cond
;;    ((executable-find "mixer") 'emms-volume-mixer-change)))


;; (defun emms-volume-mixer-change (amount)
;;   "Change mixer of FreeBSD master volume by AMOUNT."
;;   (message "Volume is: %s"
;; 	   (with-temp-buffer
;; 	     (when (zerop
;; 	       (call-process "mixer" nil "volume" nil
;; 			     "vol"
;; 			     (format "%s%d" (if (< amount 0)
;; 		       				    "-"
;; 						  "+")
;; 				     (abs amount))))
;; 	       (if (and (forward-line -1)
;; 			(re-search-forward "[0-9]$" nil))
;; 		   (format "%s" (match-string 0)))))))
    
(provide 'init-00-utils.el)
()






