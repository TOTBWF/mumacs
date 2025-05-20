;;; lang/lean ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lean4-mode
  :requires polymode markdown-mode
  :ensure (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :mode "\\.lean\\'"
  :config
  ;; Set up a `polymode' that enables `markdown-mode' in comments.
  (define-hostmode poly-lean4-hostmode
    :mode 'lean4-mode)
  (define-innermode poly-lean4-markdown-innermode
    :mode 'markdown-mode
    :head-matcher "/-[-!]"
    :tail-matcher "-/"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-lean4-mode
    :hostmode 'poly-lean4-hostmode
    :innermodes '(poly-lean4-markdown-innermode)))

(provide 'lang/lean)
;;; lean.el ends here
