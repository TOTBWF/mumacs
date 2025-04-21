;;; editor/lsp ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for the various LSP clients that Emacs supports.

;;; Code:
(require 'editor/company)
(require 'editor/flycheck)

;;; Eglot:

(use-package eglot
  ;; Disable for now
  :disabled
  :straight nil
  :commands eglot
  :hook
  (eglot-hook . company-mode))

(use-package flycheck-eglot
  ;; Disable for now
  :disabled
  :after flycheck eglot
  :preface
  ;; Silence the byte compiler.
  (declare-function global-flycheck-eglot-mode "flycheck-eglot")
  :config
  (global-flycheck-eglot-mode))

;;; LSP mode

(use-package lsp-mode
  :commands lsp-mode
  :custom
  (lsp-enable-file-watchers nil "Do not watch files; this gets extremely slow.")
  (read-process-output-max (expt 2 16)))

(provide 'editor/lsp)
;;; lsp.el ends here
