;;; org-marginalia-command-chain.el --- Command chains from functions of org-marginalia  -*- lexical-binding: t; -*-

;; Keywords: extensions

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
;;; Code:

;;;###autoload
(defun org-marginalia-make-annotation ()
  (interactive)
  (let ((mark-end (region-end)))
    (org-marginalia-mark (region-beginning) (region-end))
    (org-marginalia-save)
    (org-marginalia-open (1- mark-end))
    (end-of-buffer)))

;;;###autoload
(defun org-marginalia-browse-forward ()
  (interactive)
  (let ((buf (current-buffer)))
    (org-marginalia-next) (org-marginalia-open (point))
    (pop-to-buffer buf nil t)))

;;;###autoload
(defun org-marginalia-browse-backward ()
  (interactive)
  (let ((buf (current-buffer)))
    (org-marginalia-prev) (org-marginalia-open (point))
    (pop-to-buffer buf nil t)))

(provide 'org-marginalia-command-chain)
;;; org-marginalia-command-chain.el ends here
