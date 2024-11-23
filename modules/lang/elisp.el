;;; lang/elisp ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/snippets)
(require 'editor/flycheck)
(require 'editor/company)

(use-package elisp-mode
  :straight nil
  :config
  ;; Auto-insert header and `provide' block when a file is created.
  (create-file-template ".*.el$" "emacs-lisp-template" 'emacs-lisp-mode)
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (emacs-lisp-mode . company-mode))

(use-package emacsql
  :advice
  (calculate-lisp-indent :around emacsql--calculate-vector-indent))

(provide 'lang/elisp)
;;; elisp.el ends here
