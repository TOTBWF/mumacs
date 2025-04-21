;;; lang/ocaml ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/company)

(use-package tuareg
  :mode ("\\.ml'" "\\.mli'" "\\.opam'"))

(use-package merlin
  :after tuareg
  :hook (tuareg-mode-hook . merlin-mode))

(use-package merlin-company
  :after merlin
  :company
  (tuareg-mode . merlin-company-backend))

(use-package merlin-eldoc
  :after merlin
  :hook
  (tuareg-mode-hook . merlin-eldoc-setup))

(use-package merlin-iedit
  :after merlin
  :bind
  (:map merlin-mode-map
	("C-;" . merlin-iedit-occurrences)))

(use-package utop
  :hook
  (tuareg-mode-hhok . utop-minor-mode)
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs"))

(use-package ocp-indent
  :after tuareg
  :demand t
  :functions ocp-indent-region
  :config
  ;; ocp-indent-buffer is broken...
  (advice-add 'ocp-indent-buffer
              :override (lambda (&rest _r)
                          (ocp-indent-region (point-min) (point-max)))))

(use-package opam-switch-mode
  :after tuareg
  :hook
  (tuareg-mode-hook . opam-switch-mode)
  :bind
  (:map opam-switch-mode-map
	("C-c C-w" ("set switch" . opam-switch-set-switch))))

(use-package dune
  :mode ("dune-project'" "dune'"))

(provide 'lang/ocaml)
;;; ocaml.el ends here
