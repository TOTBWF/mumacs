;;; tools/ripgrep ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; There are two major Emacs packages for `ripgrep'
;; - `rg'
;; - `ripgrep'
;;
;; We opt to use the former, as it has more features
;; (`wgrep' integration, more customization, etc).

;;; Code:
(require 'editor/project)

(use-package rg
  :commands rg rg-dwim
  :bind
  (:map project-prefix-map
	("g" . rg-dwim)))

(use-package wgrep)
  ;; :demand t)

(provide 'tools/ripgrep)
;;; ripgrep.el ends here
