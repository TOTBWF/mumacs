;;; lang/lean ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package nael-mode
  :ensure (nael
           :type git
           :host codeberg
           :repo "mekeor/nael"
           :files ("nael/*.el" "nael-lsp/*.el" "nael-markdown/*.el"))
  :mode "\\.lean\\'")


(provide 'lang/lean)
;;; lean.el ends here
