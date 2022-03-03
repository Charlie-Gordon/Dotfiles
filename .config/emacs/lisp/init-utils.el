;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Reload configuration
;;;###autoload
(defun c1/reload-emacs-configuration ()
  "Reload emacs' init.el file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;;;; Find-file with root permissions
;; From https://emacsredux.com/blog/2013/04/21/edit-files-as-root/

(defun doas-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/doas::"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/doas::" buffer-file-name))))

(global-set-key [remap find-file-read-only] #'doas-edit)

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
    (unless time
      (display-time-mode 1))
    (kill-new filename)
    (message filename)))

;;;; Occur
;;;###autoload

(defun occur-list-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (require 'thingatpt)                  ; Thing-at-point library
  (add-hook 'occur-hook #'goto-address-mode)
  (setq goto-address-url-regexp thing-at-point-short-url-regexp)
  (occur thing-at-point-short-url-regexp "\\&")
  (remove-hook 'occur-hook #'goto-address-mode)
  (run-with-idle-timer 1 nil (lambda nil (setq goto-address-url-regexp thing-at-point-short-url-regexp))))

;;;; mpv-play-url
;; https://gist.github.com/bsless/19ca4a37eee828b1b62c84971181f506#file-yt-mpv-el
;;;###autoload
(defun c1/mpv-play-url (&optional url &rest _args)
  "Start mpv for URL."
  (interactive"sURL: ")
  (mpv-start url))

(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/")))

(provide 'init-utils)
;;; init-utils.el ends here
