;;; org-noter-highlight.el --- Highlighting note in org-noter  -*- lexical-binding: t; -*-
;; Copyright (C) 2021  i-am

;; Author: i-am <i@fbsd>
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
;;; Dependencies and setup
(eval-when-compile
  (require 'org-noter))

(defvar org-noter-highlight-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-noter-sync-current-note)
    (define-key map [return] 'org-noter-sync-current-note)
    (define-key map [d] 'org-noter-highlight-remove-at-point)
    map)
  "Keymap on highlighted text")

(defvar org-noter-highlight-base-properties `(keymap ,org-noter-highlight-mouse-map))

(defvar org-noter-highlight-color-history nil
  "A list of recently used colors for `org-noter-insert-note-highlight'")

(defcustom org-noter-property-highlight-color "HIGHLIGHT_BG_COLOR"
  "Name of the property that specifies the properties of highlighted
 note. Used by `org-noter-highlight--add-property'.")

(defcustom org-noter-property-span "NOTER_SPAN"
  "Name of the property that specifies the span of highlight
associated with the current note.
Used by `org-noter-highlight--add-property'.")

;;; Build property list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-noter-highlight-read-color (&optional prompt)
  "Read and return a color using PROMPT

Offer randomly selected color as default value but avoid 
last value of `org-noter-highlight-color-history' and hundred shades of
gray unless user explicitly selcected."
  (let* ((initial-list (defined-colors-with-face-attributes))
	 (no-repeat (remove (car org-noter-highlight-color-history)
			    initial-list))
	 (no-gray (seq-remove (lambda (elt) (string-match-p "gr[ae]y+" elt)) no-repeat))
	 (random-default (seq-random-elt no-gray))
	 (prompt (format "%s%s: "
			   (or prompt "Highlight color")
			   (if random-default
			       (concat " (" random-default ")") "")))
	 (current-completing-read-function completing-read-function)
	 (completing-read-function
          (lambda (prompt collection &optional predicate require-match
                          initial-input _hist _def inherit-input-method)
            (funcall current-completing-read-function
                     prompt collection predicate require-match
                     initial-input 'org-noter-highlight-color-history
                     random-default
                     inherit-input-method))))
    (read-color prompt)))

(defun org-noter-highlight-plist (&optional bg-color note-info-prop)
  (let* ((bg-color (or bg-color
		       (substring-no-properties (org-noter-highlight-read-color))))
	 (fg-color (readable-foreground-color bg-color))
	 (mouse-bg-color (color-lighten-name bg-color 25))
	 (mouse-fg-color (readable-foreground-color mouse-bg-color)))
    (nconc (list 'face (list :background bg-color
			     :foreground fg-color))
	   (list 'font-lock-face (list :background bg-color
				       :foreground fg-color))
	   (list 'mouse-face (list :background mouse-bg-color
				   :foreground mouse-fg-color))
	   (list 'note-info note-info-prop)
	   org-noter-highlight-base-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-noter-highlight--add-property (begin end color &optional page)
  "Insert `org-noter-property-span' and `org-noter-property-highlight-color' 
in current entry with the cons cell of beginning and end point"
  (let ((page (or page (with-selected-window (org-noter--get-doc-window) nov-documents-index))))
    (org-entry-put nil org-noter-property-span (format "%s" (cons page (cons begin end))))
    (org-entry-put nil org-noter-property-highlight-color (format "%s" color))))

(defun org-noter-rehighlight-entry (&optional pom)
  "Rehighlight the text from the value of `org-noter-property-span' of 
entry at point-or-marker POM"
  (org-noter--with-selected-notes-window
   "No notes window."
   ;; Check if notes correspond to current document.
   (if (string= (org-entry-get nil org-noter-property-doc-file t)
		(org-noter--session-property-text session))
       (let ((span (read (org-entry-get pom org-noter-property-span)))
	     (color (org-entry-get pom org-noter-property-highlight-color)))
	 (org-noter-highlight (cdr span) color (org-element-context)))
     (user-error "%s" "Document mismatch."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-noter-highlight (span color &optional note-info)
  "Add PROP to region from BEG to END indicated by SPAN cons cell which 
points to (BEG . END)"
  (with-selected-window (org-noter--get-doc-window)
    (let ((inhibit-read-only t))
      (add-text-properties (car span) (cdr span) (org-noter-highlight-plist color note-info)))))

(defun org-noter-rehighlight-buffer ()
  "Call `org-noter-rehighlight-entry' on all entries with `org-noter-property-note-location'
 key that matches with `nov-documents-index'. Should be a hook of `nov-post-html-render-hook'"
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window."
   ;; Check if notes correspond to current document.
   (if (string= (org-entry-get nil org-noter-property-doc-file t) 
		(org-noter--session-property-text session))
       ;; Get page index from document window.
       (let* ((current-page (with-selected-window (org-noter--get-doc-window)
			      nov-documents-index))
	      ;; A list of all spans in note window, looks like this,
	      ;; ("(PAGE_INDEX HL_BEGIN . HL_END)"
	      ;;  "(PAGE_INDEX HL_BEGIN . HL_END)"
	      ;; ...)
	      (all-note (org-property-values org-noter-property-span))
	      ;; Convert all strings to actual list with `read',
	      ;; yields ((PAGE_IND HL_BEG . HL_END)
	      ;;         (PAGE_IND HL_BEG . HL_END) ... )
	      (alist-note (mapcar 'read all-note))
	      ;; Delete all elements that DO NOT match `current-page'
	      (current-page-note (assoc-delete-all current-page alist-note
						   (lambda (x y) (not (eq x y))))))
	 ;; On each element, find org entry with the exact span, then call
	 ;; `org-noter-rehighlight-entry' on it.
	 (seq-do (lambda (elt)
		   (org-noter-rehighlight-entry (org-find-property org-noter-property-span
								   (format "%s" elt))))
		 current-page-note))
     (user-error "%s" "Document mismatch."))))
				  
(defun org-noter-insert-note-highlight (span-beg span-end)
  "Add note with highlighting to the region"
  (interactive"r")
  (let* ((inhibit-read-only t)
	 (color (org-noter-highlight-read-color))
	 (note-entry-prop))
    (org-noter-insert-note (org-noter--get-precise-info))
    (org-noter-highlight (cons span-beg span-end) color (org-element-at-point))
    (org-noter-highlight--add-property span-beg span-end color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-noter-highlight-remove-at-point ()
  (interactive)
  (let* ((origin-point (point))
	 (note-entry-info (get-text-property origin-point 'note-info))
	 (note-entry-begin (plist-get (cadr note-entry-info) :begin)))
  (org-noter--with-selected-notes-window
   "No notes window."
   (save-excursion
     (goto-char note-entry-begin)
     (org-delete-property org-noter-property-span)))
  (org-noter-rehighlight-buffer)
  (goto-char origin-point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-noter-highlight)
;;; org-noter-highlight.el ends here
