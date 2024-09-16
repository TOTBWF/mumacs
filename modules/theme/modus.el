;;; theme/modus ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/straight)

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi t))

(provide 'theme/modus)
;;; modus.el ends here
