;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/path)
(require 'info)

(use-package auctex)

(use-package reftex
  :straight nil
  :after auctex
  :hook (LaTeX-mode . turn-on-reftex))

(use-package company-reftex
  :after reftex
  :company
  (LaTeX-mode . company-reftex-citations))

(use-package company-auctex
  :after auctex
  :hook
  (LaTeX-mode . company-mode)
  :company
  (LaTeX-mode . company-auctex-labels)
  (LaTeX-mode . (company-autex-macros company-auctex-symbols company-auctex-environments)))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex))

;; Make TeXLive manuals available in `info', and also add all binaries to the path.


(provide 'lang/latex)
;;; latex.el ends here
