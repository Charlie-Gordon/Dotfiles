;; Syntax highlighting
(setq edl-highlights
      '(("span\\|xanalink" . font-lock-function-name-face)
	("start\\|length" . font-lock-constant-face)))

(define-derived-mode edl-mode fundamental-mode "EDL"
  "Major mode for editing EDL format"
  (setq font-lock-defaults '(edl-highlights)))  
