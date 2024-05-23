;;; editor/compilation ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Replace ANSI color escape codes with faces.
(use-package ansi-color
  :custom
  (ansi-color-for-compilation-mode t)
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'editor/compilation)
;;; compilation.el ends here
