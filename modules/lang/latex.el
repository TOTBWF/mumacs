;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/spelling)
(require 'core/path)
(require 'info)

(use-package auctex
  :custom
  (font-latex-user-keyword-classes
   '(("citations" (("cite" "{") ("citep" "{") ("citet" "{")) font-lock-constant-face command)))
  (LaTeX-verbatim-environments
   '("code" "verbatim" "verbatim*" "filecontents" "filecontents*")))

;; Set up `spell-fu', and exclude some faces from spellchecking.
(use-package spell-fu
  :after auctex
  :preface
  (require 'spell-fu)
  (defun spell-fu-LaTeX-hook ()
    (spell-fu-mode 1)
    (push 'font-latex-sedate-face spell-fu-faces-exclude)
    (push 'font-lock-constant-face spell-fu-faces-exclude)
    (push 'font-latex-verbatim-face spell-fu-faces-exclude))
  :hook (LaTeX-mode . spell-fu-LaTeX-hook))

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
  :hook (LaTeX-mode . turn-on-cdlatex)
  :custom
  (cdlatex-sub-super-scripts-outside-math-mode nil))

(provide 'lang/latex)
;;; latex.el ends here
