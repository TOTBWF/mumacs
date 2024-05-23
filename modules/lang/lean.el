;;; lang/lean ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :commands (lean4-mode))

(provide 'lang/lean)
;;; lean.el ends here
