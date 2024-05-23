;;; core/selectrum ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/straight)

(require 'use-package)
(use-package selectrum
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode 1))

;; `ctrlf' is a handy way of searching within a buffer.
(use-package ctrlf
  :config
  (ctrlf-change-search-style 'fuzzy)
  (define-key ctrlf-mode-map (kbd "C-j") #'ctrlf-forward-literal)
  (define-key ctrlf-mode-map (kbd "C-k") #'ctrlf-backward-literal)
  (meow-define-keys
    'normal
    '("/" . ctrlf-forward-literal))
  (define-key Info-mode-map (kbd "/") #'ctrlf-forward-literal)
  )

(provide 'core/selectrum)
;;; core/selectrum.el ends here
