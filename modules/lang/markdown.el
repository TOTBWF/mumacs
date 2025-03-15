;;; lang/markdown ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :straight nil
  :custom
  (markdown-enable-math t)
  )

(use-package spell-fu
  :commands
  (spell-fu-mode)
  :preface
  (defun spell-fu-markdown-hook ()
    (setq spell-fu-faces-exclude '(markdown-code-face markdown-comment-face markdown-math-face))
    (spell-fu-mode 1))
  :hook (markdown-mode . spell-fu-markdown-hook))

(provide 'lang/markdown)
;;; markdown.el ends here
