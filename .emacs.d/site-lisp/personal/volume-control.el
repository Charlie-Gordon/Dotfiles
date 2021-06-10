;;; volume-control.el --- Extension to control system volume -*- lexical-binding: t; -*-

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
;; Extensions for emacs
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dependencies and Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Keymap
(defvar volume-control-map
  (let ((map (make-sparse-keymap)))
    (define-key map "=" #'volume-increase)
    (define-key map "+" #'volume-increase)
    (define-key map "-" #'volume-decrease)
    (define-key map "m" #'volume-mute)
    (define-key map "M" #'volume-max)
    map)
  "Keymap for volume control"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun volume-change (amount)
  "Change mixer master volume by AMOUNT.
0 means mute. Override all keymap with volume-change-map"
  (with-temp-buffer
    (when (zerop (call-process "mixer" nil (current-buffer) nil
			       "vol"
			       (cond
				((zerop amount) "mute")
				((eq 100 amount) "100")
				(t (format "%s%d"
					   (if (< amount 0) "-" "+")
					   (abs amount))))))
      (delete-char -1)
      (message "%s" (buffer-string))))
  ;; Setting transcient keymap
  (unless overriding-terminal-local-map
    (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
          (map 00-volume-control-map))
      (when (keymapp map) (set-transient-map map t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilites functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun volume-increase (&optional amount)
  "Increase volume by 3 or, by prefix argument.
AMOUNT has to be an integer. if AMOUNT is zero then mute."
  (interactive"P")
  (funcall 'volume-change (or amount 3)))

(defun volume-decrease (&optional amount)
  "Decrease volume by 3 or, by prefix argument.
AMOUNT has to be an integer. if AMOUNT is zero then mute."
  (interactive"P")
  (funcall 'volume-change (- (or amount 3))))

(defun volume-mute ()
  (interactive)
  (funcall 'volume-change 0))

(defun volume-max ()
  (interactive)
  (funcall 'volume-change 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'volume-control)
;;; volume-control.el ends here
