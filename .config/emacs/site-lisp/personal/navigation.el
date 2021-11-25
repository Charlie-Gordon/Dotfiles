;;; navigation.el --- Extension for Navigation -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extensions for `navigation'
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:
;;;; Dependencies and Setup
(require 'consult)
(require 'affe)

(defvar navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'consult-find-site-lisp)
    (define-key map "p" #'consult-grep-package)
    (define-key map "o" #'affe-search-org)
    map)
  "Keymap for navigation")

;;;; Commands

;;;###autoload
(defun consult-find-site-lisp (dir)
  (interactive"P")
  (let ((consult-find-command "find . -ipath *ARG* OPTS"))
    (consult-find (expand-file-name "site-lisp" user-emacs-directory))))

;;;###autoload
(defun consult-grep-package (package-name)
  (interactive"P")
  (consult-ripgrep (expand-file-name "lisp/" user-emacs-directory)
		   (concat "(use-package " package-name)))

;;;###autoload
(defun affe-search-org()  
    (interactive)
    (let ((affe-grep-command "rg -t org --null --line-buffered --color=never --max-columns=1000 --no-heading --line-number -v ^$ ."))
      (affe-grep *org-dir*)))

(provide 'navigation)
;;; navigation.el ends here
