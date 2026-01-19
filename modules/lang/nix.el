;;; lang/nix ---  -*- lexical-binding: t; -*-

;;; Commentary:
(require 'editor/lsp)

;;; Code:
(use-package nix-mode
  :ensure t
  :commands nix-mode
  :eglot
  (nix-mode . '("nixd")))

(provide 'lang/nix)
;;; nix.el ends here
