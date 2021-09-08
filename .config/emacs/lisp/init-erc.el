;;; init-erc.el --- Configure ERC (Emacs's irc client) -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package erc
  :ensure nil
  :custom
  (erc-paranoia t)
  (erc-autojoin-channels-alist '(("irc.irchighway.net" "#ebooks")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (use-package erc-dcc
    :ensure nil
    :custom
    (erc-dcc-get-default-directory "/tmp/"))
  (use-package erc-hl-nicks :straight t))

(provide 'init-erc)
;;; init-erc.el ends here
