;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Better help
(use-package helpful
  :straight t
  :bind
  ("C-h v" . helpful-variable)
  ("C-h M" . helpful-macro)
  ("C-h o" . helpful-symbol)
  ("C-h c" . helpful-command)
  ("C-h C-k" . helpful-kill-buffers)
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-misc)
;;; init-misc.el ends here
