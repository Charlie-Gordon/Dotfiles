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
(eval-when-compile
  (require 'consult))

(defvar navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'consult-find-emacs-dir)
    (define-key map "s" #'consult-find-site-lisp)
    (define-key map "g" #'consult-find-git-dir)
    (define-key map "p" #'consult-grep-package)
    (define-key map "j" #'consult-find-journals)
    map)
  "Keymap for navigation")

;;;; Commands
;;;###autoload
(defun consult-find-git-dir (file-name)
  (interactive"P")
  (let ((consult-find-command "find . -ipath *ARG* OPTS"))
    (consult-find "~/Git/" file-name)))

;;;###autoload
(defun consult-find-emacs-dir (file-name)
  (interactive"P")
  (let ((consult-find-command "find . -ipath *ARG* OPTS"))
    (consult-find user-emacs-directory file-name)))

;;;###autoload
(defun consult-find-site-lisp (dir)
  (interactive"P")
  (let ((consult-find-command "find . -ipath *ARG* OPTS"))
    (consult-find (expand-file-name "site-lisp" user-emacs-directory))))

;;;###autoload
(defun consult-grep-package (package-name)
  (interactive"P")
  (consult-ripgrep user-emacs-directory
		   (concat "\\(use-package #^site-lisp\\|^init.*\\.el " package-name)))

;;;###autoload
(defun consult-find-journals (file-name)
  (interactive"P")
  (let ((consult-find-command "find . -ipath *ARG* OPTS"))
    (consult-find "/storage/journals" file-name)))

(provide 'navigation)
;;; navigation.el ends here
