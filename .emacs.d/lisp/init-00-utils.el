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
;; From https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (when (and (buffer-file-name)
	     (not (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/doas::" buffer-file-name))))
;;;; EWW reflow document when scaled text
(defun text-scale-mode-hook ()
  "Rerender content of EWW when uses text-scale mode."
  (eww-reload :local))
;;;; Eww rename buffer
(defun eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

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
  (interactive"sURL: ")
  (let ((url (or url
		 (if (consp (eww-suggested-uris))
		     (car (eww-suggested-uris))
		   (eww-suggested-uris)))))
    (if url
	(start-process "mpv"  nil "mpv" url)
      (message "No valid URL."))))

(defun ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (ytel-video-id video)))
      (start-process "ytel mpv" nil
		     "mpv"
		     (concat "https://www.youtube.com/watch?v=" id
		     "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")))
      (message "Starting streaming..."))
;;;; EXWM processes
(defun exwm-start-process (key command)
  (exwm-input-set-key key `(lambda ()
			     (interactive)
			     (start-process "exwm-processes" nil "sh" "-c" ,command))))

(provide 'init-00-utils.el)
