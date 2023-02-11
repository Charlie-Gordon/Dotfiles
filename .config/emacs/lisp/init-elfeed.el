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
         ("w" . c1/elfeed-db-save)
         ("C" . elfeed-capture-entry)
         :map elfeed-search-mode-map
         ("w" . c1/elfeed-db-save)
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

(defun c1/elfeed-db-save ()
  "Command to write elfeed databases."
  (interactive)
  (elfeed-db-save))

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

(defun elfeed-declickbait-entry (entry)
  (let ((title (elfeed-entry-title entry)))
    (setf (elfeed-meta entry :title)
          (elfeed-title-transform title))))

(defun elfeed-title-transform (title)
  "Declickbait string TITLE."
  (let* ((trim "\\(?:\\(?:\\.\\.\\.\\|[!?]\\)+\\)")
         (arr (split-string title nil t trim))
         (s-table (copy-syntax-table)))
    (modify-syntax-entry ?\' "w" s-table)
    (with-syntax-table s-table
      (mapconcat (lambda (word)
                   (cond
                    ((member word '("AND" "OR" "IF" "ON" "IT" "TO"
                                    "A" "OF" "VS" "IN" "FOR" "WAS"
                                    "IS" "BE"))
                     (downcase word))
                    ((member word '("WE" "DAY" "HOW" "WHY" "NOW" "OLD"
                                    "NEW" "MY" "TOO" "GOT" "GET" "THE"
                                    "ONE" "DO" "YOU"))
                     (capitalize word))
                    ((> (length word) 3) (capitalize word))
                    (t word)))
                 arr " "))))

(add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube"
                   :files ("*.el"))
  :after elfeed
  :demand t
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save))
  :custom
  (elfeed-tube-auto-save-p t)
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)
  (use-package elfeed-tube-mpv :ensure nil)


  (aio-defun c1/attach-elfeed-tube-entry (url)
             (interactive "MURL: ")
             (require 'elfeed-tube-utils)
             (when (or (null url) (string-empty-p url))
               (setq url (org-entry-get nil "URL")))

             (string-match (concat elfeed-tube-youtube-regexp
                                   (rx (zero-or-one "watch?v=")
                                       (group (1+ (not (or "&" "?"))))))
                           url)
             (if-let ((video-id (match-string 1 url))
                      (attach-dir (org-attach-dir 'get-create))
                      (origin-buf (current-buffer)))
                 (progn
                   (message "Creating a video summary...")
                   (cl-letf* ((elfeed-show-unique-buffers t)
                              (elfeed-show-entry-switch #'display-buffer)
                              (elfeed-tube-save-indicator nil)
                              (elfeed-tube-auto-save-p nil)
                              (api-data (aio-await
                                         (elfeed-tube--aio-fetch
                                          (concat (aio-await (elfeed-tube--get-invidious-url))
                                                  elfeed-tube--api-videos-path
                                                  video-id
                                                  "?fields="
                                                  ;; "videoThumbnails,descriptionHtml,lengthSeconds,"
                                                  "title,author,authorUrl,published,videoId")
                                          #'elfeed-tube--nrotate-invidious-servers)))
                              (feed-id (concat "https://www.youtube.com/feeds/videos.xml?channel_id="
                                               (nth 1 (split-string (plist-get api-data :authorUrl)
                                                                    "/" t))))
                              (author `((:name ,(plist-get api-data :author)
                                               :uri ,feed-id)))
                              (entry (elfeed-tube--entry-create feed-id api-data))
                              ((symbol-function 'elfeed-entry-feed)
                               (lambda (_)
                                 (elfeed-feed--create
                                  :id feed-id
                                  :url feed-id
                                  :title (plist-get api-data :author)
                                  :author author))))
                     (aio-await (elfeed-tube--fetch-1 entry t))
                     (let ((buff (get-buffer-create (elfeed-show--buffer-name entry)))
                           (basename (concat (format-time-string "%FT%T%z") "-"
                                             (elfeed-entry-title entry) ".txt"))
                           (count))
                       (with-current-buffer buff
                         (elfeed-show-mode)
                         (setq elfeed-show-entry entry)
                         (elfeed-show-refresh)
                         (write-region (point-min) (setq count (point-max))
                                       (expand-file-name basename attach-dir)))
                       (with-current-buffer origin-buf
                         (run-hook-with-args 'org-attach-after-change-hook attach-dir)
                         (org-attach-tag)
                         (message "Attached \"%s\" (%s char.) to %s" basename (file-size-human-readable count) (org-get-heading t t t t))))))
               (message "Not a youtube video URL, aborting."))))

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
