;;; init-outline.el --- Configuration for Outline minor mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package outline
  :ensure nil
  :init
  (setq outline-minor-mode-prefix "\M-o")
  :config
  ;; Customize the distracting folding markers.
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot standard-display-table 'selective-display
			  (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
			    (vconcat (mapcar (lambda (c) (+ face-offset c)) "-"))))
  ;; Python mode-specific Outline folding.
  (add-hook 'python-mode-hook 'outline-python))

(provide 'init-outline)
;;; init-outline.el ends here


