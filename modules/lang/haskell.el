;;; lang/haskell ---  -*- lexical-binding: t; -*-

;;; Commentary:
(require 'core/path)

;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-to-path "~/.cabal/bin")
  (add-to-path "~/.ghcup/bin/")
  :hook
  (haskell-mode . lsp)
  (haskell-mode . interactive-haskell-mode)
  :bind
  (:map haskell-mode-map
	("C-c RET" . haskell-interactive-switch))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-args-ghci
   '("-ferror-spans"
     "-fdefer-typed-holes"
     "-fno-diagnostics-show-caret"
     "-fmax-valid-hole-fits=0")))

(use-package lsp-haskell
  :after haskell
  :custom
  (lsp-haskell-hlint-on nil))

(provide 'lang/haskell)
;;; haskell.el ends here
