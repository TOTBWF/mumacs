;;; lang/markdown ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :straight nil
  :spell-fu
  (markdown-mode-hook
   :exclude
   markdown-code-face
   markdown-comment-face
   markdown-math-face)
  :custom
  (markdown-enable-math t))

(provide 'lang/markdown)
;;; markdown.el ends here
