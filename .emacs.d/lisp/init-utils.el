;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Reload configuration
;;;###autoload
(defun my/reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Find-file with root permissions
;; From https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;;;###autoload
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (when (and (buffer-file-name)
             (not (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/doas::" buffer-file-name))))

;;;; Magit shortcuts
;;;###autoload
(defun my/magit-status-dotfiles ()
  (interactive)
  "Open magit in dotfiles repository."
  (magit-status "/yadm::"))

(global-set-key (kbd "C-c d") 'my/magit-status-dotfiles)
;;;; Screenshot
;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
;;;###autoload
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  ;; Privacy
  (let ((time display-time-mode)
        (filename (make-temp-file "Emacs" nil ".svg"))
        data)
    (when time
      (display-time-mode 0))
    (force-mode-line-update)
    (with-temp-file filename
      (setq data (x-export-frames nil 'svg))
      (insert data))
    (when time
      (display-time-mode 1))
    (kill-new filename)
    (message filename)))

;;;; Occur
;;;###autoload
(defun occur-list-urls (&optional to-buffer)
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (require 'browse-url)
  (let ((to-buffer (or to-buffer
                       (current-buffer))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 browse-url-button-regexp "\\&" (list (current-buffer)) (get-buffer-create " occur-output"))
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;; mpv-play-url
;; https://gist.github.com/bsless/19ca4a37eee828b1b62c84971181f506#file-yt-mpv-el
;;;###autoload
(defun my/mpv-play-url (&optional url &rest args)
  "Start mpv for URL."
  (interactive"sURL: ")
  (require 'eww)
  (unless url (setq url
                    (if (consp (eww-suggested-uris))
                        (car (eww-suggested-uris))
                      (eww-suggested-uris))))
  (shell-command (concat (executable-find "mpv") " " (shell-quote-argument url)) "*Messages*"))

(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/")))

;;;; Volume control
(use-package volume-control
  :ensure nil
  :after exwm
  :bind-keymap ("C-' v" . volume-control-map))

(provide 'init-utils)
;;; init-utils.el ends here
