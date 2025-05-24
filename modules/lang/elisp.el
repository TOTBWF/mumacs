;;; lang/elisp ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/advice)
(require 'editor/snippets)
(require 'editor/flymake)
(require 'editor/company)

(use-package elisp-mode
  :ensure nil
  :config
  ;; Auto-insert header and `provide' block when a file is created.
  (create-file-template ".*.el$" "emacs-lisp-template" 'emacs-lisp-mode)
  :hook
  (emacs-lisp-mode-hook . company-mode))

(use-package emacsql
  :advice
  (calculate-lisp-indent :around emacsql--calculate-vector-indent))

(use-package package-lint
  :ensure t)

(use-package flymake-elpaca
  :ensure (:fetcher github :repo "totbwf/flymake-elpaca")
  :hook
  (emacs-lisp-mode-hook . flymake-elpaca-setup))

(provide 'lang/elisp)
;;; elisp.el ends here
