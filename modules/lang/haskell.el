;;; lang/haskell ---  -*- lexical-binding: t; -*-

;;; Commentary:
(require 'core/path)

;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  ;; :hook
  ;; (haskell-mode . lsp)
  ;; (haskell-mode . interactive-haskell-mode)
  :bind
  (:map haskell-mode-map
	("C-c RET" . haskell-interactive-switch))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-args-cabal-repl
   (mapcar
    (lambda (opt) (concat "--ghc-option=" opt))
    '("-ferror-spans"
      "-fdefer-typed-holes"
      "-fmax-relevant-binds=0"
      "-fno-diagnostics-show-caret"
      "-fmax-valid-hole-fits=0")))
  (haskell-process-args-ghci
   '("-ferror-spans"
     "-fdefer-typed-holes"
     "-fmax-relevant-binds=0"
     "-fno-diagnostics-show-caret"
     "-fmax-valid-hole-fits=0")))

(provide 'lang/haskell)
;;; haskell.el ends here
