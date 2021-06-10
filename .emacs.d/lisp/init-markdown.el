;;; init-markdown.el --- Markdown editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :straight t
  :disabled
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
