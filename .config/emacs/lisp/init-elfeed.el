;;; init-elfeed.el --- Elfeed configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  i-m

;; Author: i-m <i@fbsd>
;; Keywords: convenience, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package elfeed
  :straight t
  :bind (("s-e" . elfeed)
         :map elfeed-show-mode-map
         ("C" . elfeed-capture-entry)
         :map elfeed-search-mode-map
         ("C" . elfeed-capture-entry))
  :custom
  (elfeed-search-trailing-width 60)
  (elfeed-db-directory (locate-user-emacs-file "elfeed"))
  (elfeed-show-entry-switch #'elfeed-display-buffer)
  (elfeed-show-unique-buffers t)
  :config
  (defun elfeed-search-show-entry-pre (&optional lines)
    "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
    (interactive "p")
    (forward-line (or lines 0))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1)))

  (define-key elfeed-search-mode-map (kbd "n") (lambda () (interactive) (elfeed-search-show-entry-pre 1)))
  (define-key elfeed-search-mode-map (kbd "p") (lambda () (interactive) (elfeed-search-show-entry-pre -1)))
  (define-key elfeed-search-mode-map (kbd "M-RET") #'elfeed-search-show-entry-pre)

  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf))

  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  (define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
  (define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command))

(defun elfeed-capture-entry ()
  "Capture selected entries into inbox."
  (interactive)
  (elfeed-search-tag-all 'opened)
  (previous-logical-line)
  (let ((entries (or (elfeed-search-selected) (-list elfeed-show-entry))))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (cl-flet ((raise-frame nil nil))
                  (org-protocol-capture (list :template "z"
                                              :url it
                                              :title (format "%s: %s"
                                                             (elfeed-feed-title (elfeed-entry-feed entry))
                                                             (elfeed-entry-title entry))
                                              :elfeed-data entry))))
    (when (eq major-mode 'elfeed-search-mode)
      (mapc #'elfeed-search-update-entry entries))
    (unless (use-region-p) (forward-line))))

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save))
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)
  (use-package elfeed-tube-mpv :ensure nil))

(use-package elfeed-org
  :straight t
  :custom
  (rmh-elfeed-org-files (list "/storage/org/notecard/other/elfeed.org"))
  :config
  (elfeed-org)
  (require 'dash)
  (defun my-elfeed-org-update ()
    "AUtomatically Update the feeds from feeds.org when updated"
    (setq my-elfeed-org-last (or (and (boundp 'elfeed-feeds) elfeed-feeds) nil))
    (elfeed)
    (setq my-elfeed-org-current elfeed-feeds)

    (let ((elfeed-feeds (-difference my-elfeed-org-current my-elfeed-org-last)))
      ;; You don't need the line below if you don't want to see a message about what feeds are being updated
      (message "%s" elfeed-feeds)
      (mapc #'elfeed-update-feed (elfeed--shuffle (elfeed-feed-list))))
    (setq elfeed-feeds my-elfeed-org-current)))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
