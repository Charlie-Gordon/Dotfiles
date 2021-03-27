;;; edl-mode.el --- Major Mode for EDL files         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  i-am

;; Author: i-am <i@fbsd>
;; Keywords: hypermedia

(setq edl-highlights
      '(("span\\|xanalink" . font-lock-function-name-face)
	("start\\|length" . font-lock-constant-face)))

(define-derived-mode edl-mode fundamental-mode "EDL"
  "Major mode for editing EDL format"
  (setq font-lock-defaults '(edl-highlights)))  
