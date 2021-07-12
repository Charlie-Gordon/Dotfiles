;;; init-python.el --- Python environment -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pydoc-info
  :straight t)

(info-lookup-add-help
 :mode 'python-mode
 :parse-rule 'pydoc-info-python-symbol-at-point
 :doc-spec
 '(("(python)Index" pydoc-info-lookup-transform-entry)
   ("(sphinx)Index" pydoc-info-lookup-transform-entry)))

(provide 'init-python)
;;; init-python.el ends here
