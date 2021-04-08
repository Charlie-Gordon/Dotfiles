;;; edl-mode.el --- Major Mode for EDL files         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  i-am

;; Author: i-am <i@fbsd>
;; Keywords: hypermedia
;;; Code:
(setq edl-highlights
      '(("span\\|xanalink" . font-lock-function-name-face)
	("start\\|length" . font-lock-constant-face)))
;;;###autoload
(define-derived-mode edl-mode fundamental-mode "EDL"
  "Major mode for reading EDL format"
  (setq font-lock-defaults '(edl-highlights)))

(provide 'edl-mode.el)
;;; edl-mode.el ends here
