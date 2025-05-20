;;; tools/ediff ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(use-package ediff
  :elpaca nil
  :commands
  ediff
  ediff-mode
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (add-to-list 'meow-mode-state-list '(ediff-mode . motion)))

(provide 'tools/ediff)
;;; ediff.el ends here
