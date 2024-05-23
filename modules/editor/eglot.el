;;; editor/eglot ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/company)
(require 'editor/flycheck)

(require 'eglot)

(use-package eglot
  :straight nil
  :commands eglot
  :preface
  :hook
  (eglot . company-mode))

(use-package flycheck-eglot
  :after flycheck eglot
  :preface
  ;; Silence the byte compiler.
  (declare-function global-flycheck-eglot-mode "flycheck-eglot")
  :config
  (global-flycheck-eglot-mode))

(provide 'editor/eglot)
;;; eglot.el ends here
