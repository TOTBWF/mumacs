;;; lang/idris ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package idris2-mode
  :ensure (idris2-mode :type git :host github :repo "idris-community/idris2-mode")
  :mode "\\.idr\\'")

(provide 'lang/idris)
;;; idris.el ends here
