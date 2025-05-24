;;; theme/modus ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/elpaca)

(use-package modus-themes
  :ensure t
  :demand t
  :config
  (load-theme 'modus-vivendi t))

(provide 'theme/modus)
;;; modus.el ends here
