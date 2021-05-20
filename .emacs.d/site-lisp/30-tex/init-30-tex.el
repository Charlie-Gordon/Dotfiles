(use-package tex
  :ensure auctex
  :custom
  (TeX-PDF-mode t
   TeX-engine 'xetex
   TeX-command-default "LaTeXMK"
   latex-run-command "LaTeXMK"
   LaTeX-command "LaTeXMK"
   TeX-auto-save t
   TeX-parse-self t
   TeX-command-list
   '(("LaTeXMK" "latexmk -pvc -pdf %s" TeX-run-TeX nil t :help "Run latexmk")
	 ("LuaLaTeX" "%`lualatex%(mode) --synctex=1 --8bit --shell-escape%' %t" TeX-run-TeX nil t :help "Run lualatex")
     ("XeLaTeX" "%`xelatex%(mode) -synctex=1 -8bit -shell-escape%' %t" TeX-run-TeX nil t :help "Run xelatex")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")) 
   LaTeX-verbatim-environments '("verbatim" "verbatim*" "Verbatim" "Verbatim*" "lstlisting" "code" "minted" "gascode" "ccode" "pythoncode" "javacode" "bashcode")   
   TeX-view-program-selection '((output-pdf "PDF Tools") (output-html "xdg-open"))
   TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
   TeX-view-style nil
   bibtex-maintain-sorted-entries t
   bibtex-align-at-equal-sign t)
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LaTeXMK")))
  (add-hook 'LaTeX-mode-hook #'(lambda () (setq TeX-command-default "LaTeXMK")))
  (setq-default TeX-master nil))

(use-package reftex
  :disabled
  :custom
  (reftex-plug-into-AUCTeX t
   reftex-bibliography-commands '("addbibresource" "bibliography" "nobibliography")
   bibtex-dialect 'biblatex
   reftex-use-external-file-finders t)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(provide 'init-30-tex.el)
