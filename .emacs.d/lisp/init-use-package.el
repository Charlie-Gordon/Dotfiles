;;; init-use-package.el --- Configuration for use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq use-package-verbose t)

(use-package diminish
  :straight t)

(add-to-list 'use-package-keywords :termux)  ; Create :termux keyword.
;; Empty functions
(defun use-package-handler/:termux (name _keyword pred rest state)
  "Empty function, do nothing"
  (let ((body (use-package-process-keywords name rest state)))
    `(,@body)))

(defalias 'use-package-normalize/:termux 'use-package-normalize/:disabled)

;; Load only use-package with :termux keyword when *termux* is t by
;; default. I want my Termux to be as light as possible.
(add-to-list 'use-package-defaults '(:unless *termux* (lambda
                                                        (name args)
                                                        (not (plist-member args :termux)))))

(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)

(provide 'init-use-package)
;;; init-use-package.el ends here
