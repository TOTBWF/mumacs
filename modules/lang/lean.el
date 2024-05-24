;;; lang/lean ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lean4-mode
  :requires polymode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :mode "\\.lean\\'")

(provide 'lang/lean)
;;; lean.el ends here
