;;; parallel.el --- Read Text With Visible Connections  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  i-am

;; Author: i-am <i@fbsd>
;; Keywords: hypermedia, faces

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A way of viewing EDL files to show visible connections
;;
;;; Code:

(require 'shr)
(require 'eww)
(require 'url)

(defface xanalink
  '((t :box (:line-width 3 :color "tomato")))
  "Face used for link in parallel-mode")
(defcustom parallel-url-regexp
  "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
  "Regular expression that matches URLS.")

(defcustom parallel-url-regexp-span
  (concat parallel-url-regexp ",start=\\([0-9]*\\)?,length=\\([0-9]*\\)?")
  "Regular expression that matches URLS with span.  
This is `match-string' data
`(match-string 5)' matches NUM after 'start='  
`(match-string 6)' matches NUM after 'length='")

(defvar edl-highlights
      '(("^\\(span\\|xanalink\\):" . font-lock-function-name-face)
	(",start\\|,length" . font-lock-constant-face)
	("#.*$" . font-lock-comment-face)))
(defun eww-parse-content (url &optional to-buffer) 
  "Use `url-retrieve' then `eww-render' and parse to a hidden buffer called 'URL#output'
 then append region from POINT-START to POINT-END to existing TO-BUFFER or create a new buffer"
  (let ((url-mime-accept-string eww-accept-content-types)
	(buf (or to-buffer
		 (concat " " url "#output"))))
    (url-retrieve (eww--dwim-expand-url url) 'eww-render
       		  (list (eww--dwim-expand-url url) nil (get-buffer-create buf)))))

;;;###autoload
(define-derived-mode edl-mode fundamental-mode "EDL"
  "Major mode for reading EDL format"
  (setq font-lock-defaults '(edl-highlights)))

;; Move this to somewhere else?
(defun occur-list-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (add-hook 'occur-hook #'goto-address-mode)
  (occur-1 browse-url-botton-regexp "\\&" (list (current-buffer)) (get-buffer-create " occur-output"))
  (remove-hook 'occur-hook #'goto-address-mode))

(defun parallel ()
  "Start `parallel' session."
  (interactive)
  (cond ((eq major-mode 'edl-mode)
	   (goto-char (point-min))
	   (while (progn
		    (forward-line 1)
		    (re-search-forward parallel-url-regexp-span nil t)
		    (save-match-data
		      (eww-parse-content (format "%s" (match-string 1))))
		    (let* ((buf (get-buffer (concat " " (match-string 1) "#output")))
			   (desired-beg (string-to-number (match-string 5)))
			   (desired-leng (string-to-number (match-string 6)))
			   (desired-end (+ desired-beg desired-leng))
			   (valid-region (buffer-size buf))
			   ;; Check if desired-beg is in range of the size of BUFFER,
			   ;; if not, fallbacks on the beginning of BUFFER.
			   (region-beg (if (< desired-beg valid-region)
					   desired-beg
					 (point-min)))
			   ;; Check if desired-end is in range of the size of BUFFER,
			   ;; if not, fallbacks on the end of BUFFER.
			   ;; NOTE: the end of BUFFER in this case is just
			   ;; 1 plus the size of BUFFER, which is equivalent to
			   ;; calling `(point-max) on the specified BUFFER.
			   (region-end (if (< (- desired-end 1) valid-region)
					   desired-end
					 (+ 1 valid-region))))
		      (with-current-buffer (get-buffer-create "*paralleldoc*")
			(insert-buffer-substring buf region-beg region-end)))
    		    ;; return nil when point reached the end of buffer
		    ;; in effect, stops the `while' function
		    (not (eq (point) (point-max))))))
	(t (user-error "This is not an EDL file."))))

(provide 'parallel.el)
;;; parallel.el ends here
