;;; lang/markdown ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; [NOTE: markdown-mode and auto-mode-alist]
;; Unfortunately, `markdown-mode' is a bad citizen, and clobbers `auto-mode-alist'
;; before it even loads.  This means that anyone mentioning /anything/
;; `markdown-mode' related will trash the load order.
;;
;; This is particularly bad with `elpaca', as the problematic code is marked
;; as an autoload.  This means that elpaca will pull this code out into a separate
;; `markdown-mode-autoloads' file that gets loaded before init is complete.
;;
;; To work around this, we add a hook onto `elpaca-after-init-hook' that repairs the
;; `auto-mode-alist'.
;;
;; Note that this is intended behaviour; see https://github.com/jrblevin/markdown-mode/issues/127.

;;; Code:

(use-package markdown-mode
  :ensure t
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
