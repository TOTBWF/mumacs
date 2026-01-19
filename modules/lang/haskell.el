;;; lang/haskell ---  -*- lexical-binding: t; -*-

;;; Commentary:
(require 'core/path)

;;; Code:

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :hook
  (haskell-mode-hook . interactive-haskell-mode)
  :eglot
  (haskell-mode . (eglot-alternatives '(("haskell-language-server" "--lsp")
                                        ("haskell-language-server-wrapper" "--lsp"))))
  :bind
  (:map haskell-mode-map
	("C-c RET" . haskell-interactive-switch))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-show-overlays nil)
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

(use-package happy-mode
  :ensure
  (happy-mode
   :type git
   :host github
   :repo "sergv/happy-mode")
  :mode "\\.y\\'")

(provide 'lang/haskell)
;;; haskell.el ends here
