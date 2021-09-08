;;; init-uniquify.el --- Configure uniquification of buffer names -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package uniquify :ensure nil)

(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " ❱ "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)
;;; init-uniquify.el ends here
