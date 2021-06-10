;;; init-outline.el --- Configuration for Outline minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  i-am

;; Author: i-am <i@fbsd>
;; Keywords: convenience, extensions

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

;; My configuration for Outline minor mode

;;; Code:

;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/

;;;###autoload
(defun outline-overview ()
  "Show only outline headings."
  (interactive)
  (outline-show-all)
  (outline-hide-body))

(defun outline-python ()
  "Fold only definitions in Python."
  (setq outline-regexp
	(rx (or
	     ;; Definitions
	     (group (group (* space)) bow (or "class" "def") eow)
	     ;; Decorators
	     (group (group (* space)) "@"))))
  (outline-overview))

(provide 'init-outline)
;;; init-outline.el ends here


