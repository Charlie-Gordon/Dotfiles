;;; init-browser.el --- Extensions for EWW                 -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Extensions for the eww, intended for my Emacs setup.
;; Note that some of this code came from Protesilaos Stavrou's dotfiles
;; <https://protesilaos.com/dotemacs/> (for documentation please refer to
;; <https://protesilaos.com/dotemacs/#h:524bc702-ff55-4ed9-9a38-26d30d64591d>)
;; that I adapted to fit my setup.

;;; Code:

(use-package webjump
  :straight (:type built-in)
  :bind ("s-j" . webjump))

(use-package org-web-tools
  :straight '(org-web-tools :type git
                            :host github
                            :repo "alphapapa/org-web-tools"
                            :fork t
                            :branch "fix-linked-images"))


(use-package browse-url
  :ensure nil
  :custom
  (browse-url-new-window-flag t)
  (browse-url-handlers
   `((,(rx "youtube.com/result" (* anything)) . ,browse-url-secondary-browser-function)
     (,(rx "youtu" (? ".") "be") . c1/mpv-play-url)
     (,(concat (regexp-opt '("ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma" "mov" "avi" "divx"
                             "ogm" "ogv" "asf" "mkv" "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
                             "flv" "webm" "aif" "opus"))
               "$")
      . c1/mpv-play-url)
     ("." . eww-browse-url)))
  (browse-url-secondary-browser-function
   'browse-url-default-browser))

(use-package shr
  :ensure nil
  :custom
  (shr-max-image-proportion 0.6)
  (shr-discard-aria-hidded t)
  (shr-image-animate nil)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-width 70)
  (shr-cookie-policy nil))

(use-package eww
  :defer t
  :after shr
  :bind
  (:map eww-mode-map
        ("<return>" . eww-follow-link)
        ("W" . c1/mpv-play-url)
        ("R" . prot/eww-readable)
        ("L" . eww-list-bookmarks)
        ("t" . eww-readable)
        ("d" . prot/eww-download-html)
        ("n" . shr-next-link)
        ("p" . shr-previous-link)
        ("u" . eww-back-url)
        :map eww-link-keymap
        ("v" . nil) ;; stop overriding `eww-view-source'
        :map eww-buffers-mode-map
        ("d" . eww-bookmark-kill)
        :map eww-bookmark-mode-map
        ("d" . eww-bookmark-kill))
  :custom
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)")
  (eww-download-directory (expand-file-name "~/Downloads/"))
  (eww-bookmarks-directory (expand-file-name "bookmarks/"  user-emacs-directory))
  (eww-header-line-format nil)
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]"))

(defun c1/fix-webjump-url-encode (str)
  "Like `webjump-url-encode' but doesn't replace \" \" with \"+\".

Meant to override `webjump-url-encode'"
  (mapconcat (lambda (c)
               (let ((s (char-to-string c)))
                 (if (string-match "[a-zA-Z_./~-]" s) s
                   (upcase (format "%%%02x" c)))))
             (encode-coding-string str 'utf-8)
             ""))

(advice-add 'webjump-url-encode :override #'c1/fix-webjump-url-encode)

(use-package elpher
  :straight t)

(defgroup prot/eww ()
  "Tweaks for EWW."
  :group 'eww)

;;;; Basic setup
(defun prot/eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    ;; If `name' is nil, just don't change the buffer name.
    (when name
      (rename-buffer (format "*%s # eww*" name) t))))

(add-hook 'eww-after-render-hook #'prot/eww--rename-buffer)
(advice-add 'eww-back-url :after #'prot/eww--rename-buffer)
(advice-add 'eww-forward-url :after #'prot/eww--rename-buffer)

;;;###autoload
(defun prot/eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun prot/eww-readable ()
  "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
  (interactive)
  (let ((shr-width (current-fill-column))
        (shr-max-image-proportion 0.35))
    (eww-readable)))

;;;###autoload
(defun text-scale-mode-hook ()
  "Rerender content of EWW when uses text-scale mode."
  (eww-reload :local))

(provide 'init-browser)
;;; init-browser.el ends here


