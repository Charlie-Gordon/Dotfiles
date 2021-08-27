;;; init-const.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *journals-dir* (if *termux* "~/storage/shared/journals/"
			   "/storage/journals/"))

(defconst *library-dir* (if *termux* "" "/storage/library/"))

(defconst *org-dir* (if *termux* "" "/storage/org/"))

(defconst *bibliography-dir* (if *termux* "" "/storage/bib/"))

(provide 'init-const)
;;; init-const.el ends here
