;;; editor/projectile ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(use-package projectile
  :preface
  ;; Silence the byte compiler.
  (declare-function projectile-mode "straight/build/projectile/projectile.el")
  :defer nil
  :custom
  (projectile-completion-system 'default)
  (projectile-project-search-path '(("~/Documents/Projects")))
  :config
  (projectile-mode 1)
  :bind
  (:map meow-leader-keymap
	("p" ("project" . projectile-command-map))))

(use-package rg)

(provide 'editor/projectile)
;;; projectile.el ends here
