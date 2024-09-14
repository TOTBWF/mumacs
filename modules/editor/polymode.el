;;; editor/polymode ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package polymode
  :config
  (define-hostmode poly-markdown-hostmode
    :mode 'markdown-mode)

  (define-innermode poly-markdown-agda-innermode
    :mode 'agda2-mode
    :head-matcher "^[\t]*```agda\n"
    :tail-matcher "^[\t]*```\n"
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-markdown-mode
    :hostmode 'poly-markdown-hostmode
    :innermodes '(poly-markdown-agda-innermode))

  )

;; Experimenting with `polymode' and literate Agda.


(provide 'editor/polymode)
;;; polymode.el ends here
