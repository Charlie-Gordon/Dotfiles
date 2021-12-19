;;; init-bongo.el --- Extensions for Bongo                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package bongo
  :straight t
  :bind ("C-c b" . bongo)
  :custom
  (bongo-default-directory "/storage/music/")
  (bongo-logo nil))

(provide 'init-bongo)
