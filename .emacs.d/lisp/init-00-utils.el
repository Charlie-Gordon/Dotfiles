;;; init-00-utils.el --- Functions used by other el files
;;;; Reload configuration
(defun reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;; ;;;; Dired with root permissions
;; (defun sudired (editing-directory)
;;   "Dired with root permission."
;;   (interactive "D")
;;   (dired-at-point (concat "/doas:root@localhost:" (expand-file-name editing-directory))))
;;;; Find-file with root permissions
;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;; 	       (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/doas:root@localhost:" buffer-file-name))))
;;;; EWW reflow document when scaled text
(defun text-scale-mode-hook ()
  "Rerender content of EWW when uses text-scale mode."
  (eww-reload :local))
;;;; Magit shortcuts
(defun magit-status-dotfiles ()
  (interactive)
  "Open magit in dotfiles repository."
  (magit-status "/yadm::"))
;;;; Occur
(defun occur-list-urls (&optional to-buffer)
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((to-buffer (or to-buffer
		       (current-buffer))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
  "\\&" (list (current-buffer)) (get-buffer-create " occur-output"))
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;; mpv-play-url
;; https://gist.github.com/bsless/19ca4a37eee828b1b62c84971181f506#file-yt-mpv-el
(defun mpv-play-url (&optional url &rest args)
  "Start mpv for URL."
  (interactive)
  (let ((url (or url
		 (if (consp (eww-suggested-uris))
		     (car (eww-suggested-uris))
		   (eww-suggested-uris)))))
    (if url
	(start-process "mpv"  nil "mpv" url)
      (message "No valid URL."))))
       
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






