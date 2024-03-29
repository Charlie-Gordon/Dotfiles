;;; init-completion.el --- Minimal completion framework config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Selectrum
;; (use-package selectrum
;;   :straight t
;;   :bind (:map ctl-x-map
;;               ("C-z" . selectrum-repeat))
;;   :custom
;;   ;; Fill as many candidates as possible, even if it doesn't have that
;;   ;; many.
;;   (selectrum-fix-vertical-window-height selectrum-max-window-height)
;;   :hook
;;   (after-init . selectrum-mode))

(use-package vertico
  :straight '(vertico :type git
                      :rego "melpa/melpa"
                      :files ("*" (:exclude ".git") "extensions/"))
  :custom
  (vertico-resize t)
  :init
  (vertico-mode 1)
  :config
  (use-package vertico-quick
    :ensure nil
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit))))

;;;; Orderless
(use-package orderless
  :straight t
  :custom
  (vertico--highlight-function #'orderless-highlight-matches)
  (selectrum-refine-candidates-function #'orderless-filter)
  (completion-styles
   '(orderless basic initials shorthand))
  (completion-category-overrides
   '((file (styles . (partial-completion basic orderless))))))

;;;; Marginalia
(use-package marginalia
  :straight t
  :config (marginalia-mode 1))

;;;; Consult
(use-package consult
  :straight t
  ;;  Replacing functions with their consult counterparts
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)) ;; required by consult-line to detect isearch
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file
   consult--source-bookmark
   :preview-key (kbd "M-."))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;;;; Embark
(use-package embark
  :straight t
  :bind
  ("s-'" . embark-act)
  ("s-\"" . embark-dwim)
  ("M-:" . pp-eval-expression)
  (:map embark-symbol-map
        ("h" . helpful-symbol))
  (:map embark-url-map
	("s" . browse-url-xdg-open)
	("m" . c1/mpv-play-url))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t)

;;;; Company
;; (use-package company
;;   :straight t
;;   :hook (after-init . global-company-mode)
;;   :diminish)

(use-package corfu
  :straight t
  :custom
  (tab-always-indent 'complete)
  (corfu-preselect-first nil)
  (completion-cycle-threshold 3)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

(provide 'init-completion)
;;; init-completion.el ends here
