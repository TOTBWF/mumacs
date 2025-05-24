;;; tools/direnv ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/elpaca)

(use-package envrc
  :ensure t
  :config
  (after-elpaca-init-hook . envrc-global-mode))

(provide 'tools/direnv)
;;; direnv.el ends here
