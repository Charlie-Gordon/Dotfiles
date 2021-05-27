;;; init-00-utils.el --- Functions used by other el files
;;;; Reload configuration
(defun reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Find-file with root permissions
;; From https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (when (and (buffer-file-name)
	     (not (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/doas::" buffer-file-name))))
;;;; Magit shortcuts
(defun magit-status-dotfiles ()
  (interactive)
  "Open magit in dotfiles repository."
  (magit-status "/yadm::"))
;;;; Screenshot
;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  ;; Privacy
  (make-variable-buffer-local 'display-time-mode)
  (display-time-mode 0)
  (force-mode-line-update)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
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
	(progn (start-process "mpv"  nil "mpv" url)
	       (message "%s%s" "Playing " url))
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

(provide 'init-00-utils.el)

;;;; Volume change
(use-package 00-volume-control
  :init (define-prefix-command 'volume-control-map)
  :bind-keymap ("s-v" . volume-control-map)
  :bind (:map volume-control-map
	      ("=" . volume-increase)
	      ("+" . volume-increase)
	      ("-" . volume-decrease)
	      ("m" . volume-mute)
	      ("M" . volume-max))
  :ensure nil)
;;; init-00-utils.el ends here.
