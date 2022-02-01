;;; init-const.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *journals-dir* (if (file-exists-p "storage/journals/")
                             "storage/journals/"))

(defconst *library-dir* (if (file-exists-p "/storage/library/")
                            "/storage/library/"))

(defconst *org-dir* (if (file-exists-p "/storage/org/")
                        "/storage/org/"))

(defconst *bibliography-dir* (if (file-exists-p "/storage/bib/")
                                 "/storage/bib/"))

(defconst *gtd-dir* (if (file-exists-p "/storage/bib/")
                        "/storage/org/gtd/"))


(provide 'init-const)
;;; init-const.el ends here
